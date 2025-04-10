#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [require-and-provide-suggestions refactoring-suite?]
  [require-and-provide-suggestions-all-enabled-for-test refactoring-suite?]))


(require guard
         racket/list
         racket/match
         racket/symbol
         rebellion/base/comparator
         rebellion/base/option
         rebellion/collection/entry
         rebellion/collection/list
         rebellion/private/static-name
         rebellion/streaming/transducer
         resyntax/base
         resyntax/private/logger
         syntax/parse)


(module+ test
  (require rackunit
           rebellion/collection/list
           (submod "..")))


;@----------------------------------------------------------------------------------------------------


(define-syntax-class export-spec
  #:attributes (id)
  (pattern id:id))


(define-refactoring-rule provide-deduplication
  #:description "Providing the same identifier multiple times is unnecessary."
  #:literals (provide)
  (provide spec:export-spec ...)
  #:when (check-duplicate-identifier (attribute spec.id))

  #:with (deduped-spec ...)
  (remove-duplicates (attribute spec) bound-identifier=?
                     #:key (λ (spec-stx) (syntax-parse spec-stx [:export-spec #'id])))

  (provide deduped-spec ...))


(struct parsed-simple-import (phase phase-form kind plain-spec) #:transparent)


(define (set-phase phase phase-form imports)
  (for/list ([import imports])
    (match-define (parsed-simple-import _ _ kind spec) import)
    (parsed-simple-import phase phase-form kind spec)))


(define-syntax-class import-spec
  #:attributes (parsed-imports)
  #:literals (for-syntax for-template for-label for-meta)

  (pattern (for-syntax spec:phaseless-import-spec ...)
    #:attr parsed-imports (set-phase 1 'for-syntax (attribute spec.parsed)))

  (pattern (for-template spec:phaseless-import-spec ...)
    #:attr parsed-imports (set-phase -1 'for-template (attribute spec.parsed)))

  (pattern (for-label spec:phaseless-import-spec ...)
    #:attr parsed-imports (set-phase #false 'for-label (attribute spec.parsed)))

  (pattern (for-meta phase:phase-level spec:phaseless-import-spec ...)
    #:attr parsed-imports
    (set-phase (syntax->datum (attribute phase)) 'for-meta (attribute spec.parsed)))

  (pattern spec:phaseless-import-spec #:attr parsed-imports (list (attribute spec.parsed))))


(define-syntax-class phase-level
  (pattern #false)
  (pattern :exact-integer))


(define-syntax-class phaseless-import-spec
  #:attributes (parsed)
  #:literals (only-in
              except-in
              prefix-in
              rename-in
              for-syntax
              for-template
              for-label
              for-meta)

  (pattern :simple-module-import-spec)
  
  (pattern (~or (only-in _:simple-module-import-spec _ ...)
                (except-in _:simple-module-import-spec _ ...)
                (prefix-in _ _:simple-module-import-spec)
                (rename-in _:simple-module-import-spec _ ...))
    #:cut
    #:attr parsed (parsed-simple-import 0 'plain 'other-known this-syntax))
  
  (pattern ((~and form
                  (~not for-syntax)
                  (~not for-template)
                  (~not for-label)
                  (~not for-meta))
            subspec ...)
    #:attr parsed (parsed-simple-import 0 'plain 'other-unknown this-syntax)))


(define-syntax-class simple-module-import-spec
  #:attributes (parsed)
  (pattern mod:collection-module-path #:attr parsed (parsed-simple-import 0 'plain 'collection #'mod))
  (pattern mod:file-module-path #:attr parsed (parsed-simple-import 0 'plain 'file #'mod)))


(define-syntax-class collection-module-path
  (pattern :id))


(define-syntax-class file-module-path
  (pattern :str))


(define-syntax-class module-path
  (pattern :id))


(define (phase-level->phase-form level)
  (match level
    [0 'plain]
    [1 'for-syntax]
    [-1 'for-template]
    [#false 'for-label]
    [_ 'for-meta]))


(define phase-form<=> (comparator-of-constants 'for-syntax 'for-template 'for-label 'for-meta 'plain))
(define import-kind<=> (comparator-of-constants 'collection 'other-known 'file))


(define (false-last<=> cmp)
  (make-comparator
   #:name 'false-last<=>
   (λ (left right)
     (cond
       [(and left right) (compare cmp left right)]
       [left lesser]
       [right greater]
       [else equivalent]))))


(module+ test
  (test-case "false-last<=>"
    (define sorted
      (transduce (list "foo" #false "bar" #false "baz")
                 (sorting (false-last<=> string<=>))
                 #:into into-list))
    (check-equal? sorted (list "bar" "baz" "foo" #false #false))))


(define (parsed-simple-import-path import)
  (match import
    [(parsed-simple-import _ _ 'collection id) (symbol->immutable-string (syntax-e id))]
    [(parsed-simple-import _ _ 'file str-stx) (string->immutable-string (syntax-e str-stx))]
    [(parsed-simple-import _ _ 'other-known _) #false]))


(define (parsed-import-spec-without-syntax import)
  (match import
    [(parsed-simple-import phase phase-form 'collection id)
     (parsed-simple-import phase phase-form 'collection (symbol->immutable-string (syntax-e id)))]
    [(parsed-simple-import phase phase-form 'file str-stx)
     (parsed-simple-import phase phase-form 'file (string->immutable-string (syntax-e str-stx)))]
    [(parsed-simple-import _ _ 'other-known _) import]))


(define parsed-import-spec<=>
  (comparator-chain (comparator-map phase-form<=> parsed-simple-import-phase-form)
                    (comparator-map import-kind<=> parsed-simple-import-kind)
                    (comparator-map (false-last<=> string<=>) parsed-simple-import-path)))


(define phase-level<=>
  (comparator-chain (comparator-map phase-form<=> phase-level->phase-form) real<=>))


(define (simple-spec? spec)
  (define kind (parsed-simple-import-kind spec))
  (and (not (equal? kind 'other-known)) (not (equal? kind 'other-unknown))))


(define (import-specs-all-known? specs)
  (for/and ([spec specs])
    (not (equal? (parsed-simple-import-kind spec) 'other-unknown))))


(define (import-specs-tidy? specs)
  (and (sorted? specs parsed-import-spec<=>)
       (sorted? (filter simple-spec? specs) parsed-import-spec<=> #:strictly? #true)))


(define (import-specs-tidy specs)
  (transduce specs
             (indexing parsed-simple-import-phase)
             (grouping into-list)
             (sorting phase-level<=> #:key entry-key)
             (append-mapping (λ (e) (build-tidy-require-spec (entry-key e) (entry-value e))))
             #:into into-list))


(define/guard (build-tidy-require-spec phase imports)
  (define sorted-specs
    (transduce imports
               (sorting parsed-import-spec<=>)
               (deduplicating-consecutive #:key parsed-import-spec-without-syntax)
               (mapping parsed-simple-import-plain-spec)
               #:into into-list))
  (log-resyntax-debug "sorted specs at phase ~a: ~a" phase sorted-specs)
  (guard (not (equal? phase 0)) #:else sorted-specs)
  (define import-header
    (match phase
      [1 (list #'for-syntax)]
      [-1 (list #'for-template)]
      [#false (list #'for-label)]
      [_ (list #'for-meta #`#,phase)]))
  (list #`(#,@import-header #,@sorted-specs)))


(module+ test
  (test-case "import tidying"
    (define col1 #'racket/hash)
    (define col2 #'racket/list)
    (define col3 #'racket/string)
    (define path1 #'"apple.rkt")
    (define path2 #'"banana.rkt")
    (define col1-plain (parsed-simple-import 0 'plain 'collection col1))
    (define col2-plain (parsed-simple-import 0 'plain 'collection col2))
    (define col3-plain (parsed-simple-import 0 'plain 'collection col3))
    (define path1-plain (parsed-simple-import 0 'plain 'file path1))
    (define path2-plain (parsed-simple-import 0 'plain 'file path2))

    (test-case "import-specs-tidy?"
      (check-true (import-specs-tidy? '()))
      (check-true (import-specs-tidy? (list col1-plain)))
      (check-true (import-specs-tidy? (list col1-plain col2-plain)))
      (check-false (import-specs-tidy? (list col2-plain col1-plain)))
      (check-true (import-specs-tidy? (list col1-plain col2-plain col3-plain)))
      (check-false (import-specs-tidy? (list col3-plain col2-plain col1-plain))))

    (test-case "import-specs-tidy"
      (check-equal? (import-specs-tidy '()) '())
      (check-equal? (import-specs-tidy (list col1-plain)) (list col1))
      (check-equal? (import-specs-tidy (list col1-plain col2-plain)) (list col1 col2))
      (check-equal? (import-specs-tidy (list col2-plain col1-plain)) (list col1 col2))
      (check-equal? (import-specs-tidy (list col1-plain col2-plain col3-plain)) (list col1 col2 col3))
      (check-equal? (import-specs-tidy (list col3-plain col2-plain col1-plain)) (list col1 col2 col3))
      (check-equal? (import-specs-tidy (list path1-plain col1-plain)) (list col1 path1))
      (check-equal? (import-specs-tidy (list path2-plain path1-plain)) (list path1 path2)))))


(define-refactoring-rule tidy-require
  #:description
  "Keep imports in `require` sorted and grouped by phase, with collections before files."
  #:literals (require)
  (require spec:import-spec ...)
  #:do [(define specs (append* (attribute spec.parsed-imports)))]
  #:when (import-specs-all-known? specs)
  #:when (not (import-specs-tidy? specs))
  #:with (tidy ...) (import-specs-tidy specs)
  (require tidy ...))


(define (sorted? seq comparator #:strictly? [strictly? #false])
  (define-values (vs next) (sequence-generate* seq))
  (let loop ([vs vs] [next next] [previous absent])
    (match* (vs previous)
      [(#false _) #true]
      [((list v) (== absent))
       (define-values (vs* next*) (next))
       (loop vs* next* (present v))]
      [((list v) (present prev))
       (cond
         [(if strictly?
              (compare-infix comparator prev < v)
              (compare-infix comparator prev <= v))
          (define-values (vs* next*) (next))
          (loop vs* next* (present v))]
         [else #false])])))


(module+ test
  (test-case "sorted?"
    (check-true (sorted? '(1 2 3 4 5) real<=>))
    (check-true (sorted? '(1 2 3 4 5) real<=> #:strictly? #true))
    (check-true (sorted? '(1 1 1 1 1) real<=>))
    (check-false (sorted? '(1 1 1 1 1) real<=> #:strictly? #true))
    (check-false (sorted? '(1 2 3 4 3) real<=>))
    (check-false (sorted? '(1 2 3 4 3) real<=> #:strictly? #true))))


(define-refactoring-suite require-and-provide-suggestions-all-enabled-for-test
  #:rules (provide-deduplication
           tidy-require))


(define-refactoring-suite require-and-provide-suggestions
  #:rules (provide-deduplication

           ;; Excluded because of discrepancy between this and racket-mode (see
           ;; https://github.com/jackfirth/resyntax/issues/432 for details)
           ;;
           ;; tidy-require

           ))
