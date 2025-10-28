#lang racket/base


(provide (rename-out [resyntax-test-app #%app])
         (rename-out [resyntax-test-module-begin #%module-begin])
         (rename-out [resyntax-test-require require])
         #%top-interaction
         header
         test
         no-change-test
         analysis-test)


(require (for-syntax racket/base
                     racket/match
                     resyntax/test/private/statement)
         rackunit
         rebellion/base/comparator
         rebellion/base/range
         rebellion/collection/range-set
         resyntax/test/private/rackunit
         resyntax/default-recommendations
         syntax/parse/define
         (only-in racket/base [#%module-begin racket-module-begin]))


;@----------------------------------------------------------------------------------------------------


(begin-for-syntax
  (define (resyntax-test-shape-tag-expander kw)
    (match kw
      ['#:statement #'expand-statement]
      ['#:code-block #'expand-code-block]
      ['#:code-line #'expand-code-line]
      ['#:range-set #'expand-range-set])))


(define-syntax-parse-rule (resyntax-test-app . tail)
  #:with (shape-tag:keyword _ ...) (attribute tail)
  #:with expander (resyntax-test-shape-tag-expander (syntax-e (attribute shape-tag)))
  (expander tail))


(define-syntax-parse-rule (expand-statement statement)
  #:do [(define statement-stx (attribute statement))]
  #:with (#:statement statement-id:id _ ...) statement-stx
  #:do [(define statement-id-stx (attribute statement-id))
        (syntax-parse-state-cons! 'literals statement-id-stx)
        (define transformer (syntax-local-value statement-id-stx (λ () #false)))
        (unless transformer
          (raise-syntax-error #false
                              "unbound statement"
                              statement-stx
                              statement-id-stx))
        (unless (statement-transformer? transformer)
          (raise-syntax-error #false
                              "not defined as a statement"
                              statement-stx
                              statement-id-stx))
        (define transformer-proc (statement-transformer-procedure transformer))]
  #:with result
  (syntax-local-apply-transformer transformer-proc statement-id-stx 'module #false statement-stx)
  result)


(define-syntax-parse-rule (expand-code-block (#:code-block str))
  (code-block 'str))


(define-syntax-parse-rule (expand-code-line (#:code-line str))
  (code-block 'str))


(define-syntax-parse-rule (expand-range-set (#:range-set (#:line-range first-line last-line) ...))
  (range-set (line-range 'first-line 'last-line) ...))


(define (line-range first-line last-line)
  (closed-range first-line last-line #:comparator natural<=>))


(define-syntax (statement stx)
  (syntax-parse stx
    #:track-literals
    [(statement statement-id:id . tail)
     #:do [(syntax-parse-state-cons! 'literas #'statement-id)]
     (define transformer (syntax-local-value #'statement-id (λ () #false)))
     (unless transformer
       (raise-syntax-error #false
                           "unbound identifier"
                           this-syntax
                           #'statement-id))
     (unless (statement-transformer? transformer)
       (raise-syntax-error #false
                           "not defined as a statement"
                           this-syntax
                           #'statement-id))
     (syntax-local-apply-transformer (statement-transformer-procedure transformer)
                                     #'statement-id
                                     'module
                                     #false
                                     stx)]))


(define-syntax resyntax-test-require
  (statement-transformer
   (λ (stx)
     (syntax-parse stx
       #:track-literals
       [(_ _ module:id suite:id)
        #`(begin
            (require (only-in module suite))
            ; Using syntax/loc to ensure that if add-suite-under-test! throws a runtime
            ; error because suite isn't a refactoring suite, the error will point to the
            ; require: statement.
            #,(syntax/loc this-syntax (add-suite-under-test! suite)))]))))


(begin-for-syntax
  (define-syntax-class literal-code
    #:description "a code block"
    #:opaque
    (pattern ((~or #:code-line #:code-block) str:str))))


(define-syntax header
  (statement-transformer
   (λ (stx)
     (syntax-parse stx
       #:track-literals
       [(_ _ header-code:literal-code)
        ; Using syntax/loc so that errors thrown by set-header! point to the header:
        ; statement.
        (syntax/loc stx (set-header! header-code))]))))


(begin-for-syntax
  (define-splicing-syntax-class test-parameters
    #:attributes ([id 1] [value 1])

    (pattern (~seq)
      #:with (id ...) '()
      #:with (value ...) '())

    (pattern (~seq (#:option #:lines (~and line-set (#:range-set _ ...))))
      #:with (id ...) (list #'current-line-mask)
      #:with (value ...) (list #'line-set)))
  
  (define-splicing-syntax-class code-block-test-args
    #:attributes ([check 1])

    (pattern (~seq input-code:literal-code expected-code:literal-code)
      #:with (check ...)
      (list (syntax/loc #'input-code (check-suite-refactors input-code expected-code))))

    (pattern (~seq input-code:literal-code ...+
                   expected-code:literal-code)
      #:when (>= (length (attribute input-code)) 2)
      #:with (check ...)
      (for/list ([input-stx (in-list (attribute input-code))])
        (quasisyntax/loc input-stx
          (check-suite-refactors #,input-stx expected-code))))))


(define-syntax test
  (statement-transformer
   (λ (stx)
     (syntax-parse stx
       #:track-literals
       [(#:statement _ name:str params:test-parameters args:code-block-test-args)
        #`(test-case 'name
            (parameterize ([params.id params.value] ...)
              args.check ...))]))))


(define-syntax no-change-test
  (statement-transformer
   (λ (stx)
     (syntax-parse stx
       #:track-literals
       [(#:statement _ name:str params:test-parameters code:literal-code)
        #`(test-case 'name
            (parameterize ([params.id params.value] ...)
              #,(syntax/loc #'code (check-suite-does-not-refactor code))))]))))


(begin-for-syntax
  (define-syntax-class property-value
    #:description "a literal syntax property value (an unquoted symbol, boolean, number, or string)"
    (pattern (~or :id :boolean :number :str))))


(define-syntax analysis-test
  (statement-transformer
   (λ (stx)
     (syntax-parse stx
       #:track-literals
       #:datum-literals (option @within @inspect @property @assert)
       [(#:statement _ name:str
         code:literal-code
         (~seq (#:option #:within context-block:literal-code) ...
               (#:option #:inspect target-block:literal-code)
               (#:option #:property property-key:id)
               (~and assert-option (#:option #:assert expected-value:property-value))))
        #`(test-case 'name
            #,(syntax/loc this-syntax
                (check-suite-analysis code
                                      (list context-block ...)
                                      target-block
                                      'property-key
                                      'expected-value)))]))))


;; Helper function to check if any require: statements are present
(begin-for-syntax
  (define (has-require-statements? body-stxs)
    (for/or ([stmt (in-list body-stxs)])
      (syntax-parse stmt
        #:literals (resyntax-test-require)
        [(#:statement resyntax-test-require . _) #true]
        [_ #false]))))


;; Custom #%module-begin that automatically includes default-recommendations
;; when no explicit require: statements are present
(define-syntax (resyntax-test-module-begin stx)
  (syntax-parse stx
    [(_ body ...)
     #:do [(define has-require? (has-require-statements? (attribute body)))]
     #:attr default-require (and (not has-require?) #'(add-suite-under-test! default-recommendations))
     #'(racket-module-begin
        (module+ test
          ;; We always clear the suites under test first in case this test file is executing in the
          ;; same (dynamic) module namespace as another test file. If we didn't do this, because the
          ;; suites under test are stored in a global parameter, then a test runner that reuses
          ;; namespaces across files might accidentally run extra refactoring rules that the test file
          ;; didn't specify. The `raco cover` tool is one such test runner: without this reset,
          ;; coverage reporting with `raco cover` stops working.
          (clear-suites-under-test!)
          ;; Similarly, we also clear the test header global parameter.
          (clear-header!)
          (~? default-require)
          body ...))]))


;@----------------------------------------------------------------------------------------------------


(module reader racket/base


  (require racket/contract/base)


  (provide
   (contract-out
    [read procedure?]
    [read-syntax procedure?]))


  (require (only-in racket/base [read-syntax racket:read-syntax])
           racket/list
           racket/path
           racket/port
           racket/syntax-srcloc
           resyntax/private/syntax-traversal
           resyntax/private/universal-tagged-syntax
           resyntax/test/private/grammar
           resyntax/test/private/tokenizer
           syntax/parse)
  

  (define (read in)
    (read-using-syntax-reader read-syntax in))


  (define (read-syntax source-name in source-mod-path-stx start-line start-column start-pos)
    (define start-srcloc (srcloc source-name start-line start-column start-pos 0))
    (define definitely-original-syntax (with-input-from-string "foo" racket:read-syntax))
    (unless (syntax-original? definitely-original-syntax)
      (raise-arguments-error 'read-syntax
                             "fundamental assumptions about syntax originality violated"))
    (define parse-tree (parse source-name (make-refactoring-test-tokenizer in)))
    (define cleaned-parse-tree
      (add-uts-properties
       (replace-option-identifiers-with-keywords
        (join-multiline-code-blocks
         (replace-grammar-tags-with-shape-tags parse-tree)))))
    (define statements
      (syntax-parse cleaned-parse-tree
        [(#:program statement ...) (attribute statement)]))
    (define raw-module-id (datum->syntax #false 'module start-srcloc definitely-original-syntax))
    (define module-level-separators
      (append (list "" "#lang ") (make-list (length statements) "\n") (list "")))
    (define module-id (syntax-property raw-module-id 'uts-separators module-level-separators))
    (define derived-modname-symbol (derive-module-name-from-source source-name))
    (define raw-modname
      (datum->syntax #false derived-modname-symbol start-srcloc definitely-original-syntax))
    (define modname (syntax-property raw-modname 'uts-content ""))
    (define prelude-srcloc (srcloc-extend-right start-srcloc (string-length "resyntax/test")))
    (define raw-prelude
      (datum->syntax #false 'resyntax/test prelude-srcloc definitely-original-syntax))
    (define prelude (syntax-property raw-prelude 'uts-content "resyntax/test"))
    (define module-datum (list* module-id modname prelude statements))
    (define whole-program-srcloc (syntax-srcloc cleaned-parse-tree))
    (check-universal-tagged-syntax
     (datum->syntax #false module-datum whole-program-srcloc definitely-original-syntax)))


  (define (srcloc-extend-right loc amount)
    (struct-copy srcloc loc [span (+ (srcloc-span loc) amount)]))


  (define (derive-module-name-from-source source-name)
    (cond
      [(symbol? source-name) source-name]
      [(path? source-name)
       (string->symbol (path->string (path-replace-extension (file-name-from-path source-name) #"")))]
      [else 'anonymous-resyntax-test]))


  (define (replace-grammar-tags-with-shape-tags grammar-stx)
    (syntax-traverse grammar-stx
      [(tag:id subform ...)
       (define tag-stx (attribute tag))
       (define as-kw (string->keyword (symbol->string (syntax-e tag-stx))))
       (define as-kw-stx (datum->syntax #false as-kw tag-stx tag-stx))
       (replace-grammar-tags-with-shape-tags
        (datum->syntax #false (cons as-kw-stx (attribute subform)) this-syntax this-syntax))]
      #:parent-context-modifier (λ (stx) stx)
      #:parent-srcloc-modifier (λ (stx) stx)
      #:parent-props-modifier (λ (stx) stx)))


  (define-syntax-class multi-line-code-block-tag
    (pattern
      (~or #:standalone-code-block
           #:prefixed-standalone-code-block
           #:first-code-block
           #:middle-code-block
           #:last-code-block
           #:prefixed-first-code-block
           #:prefixed-middle-code-block
           #:prefixed-last-code-block)))


  (define (join-multiline-code-blocks stx)
    (syntax-traverse stx
      [(tag:multi-line-code-block-tag line:str ...)
       (define normalized-id (datum->syntax #false '#:code-block #false (attribute tag)))
       (define joined-lines
         (apply string-append
                (for/list ([line-stx (in-list (attribute line))])
                  (syntax-e line-stx))))
       (define joined-srcloc (srcloc-spanning (first (attribute line)) (last (attribute line))))
       (define joined-lines-stx (datum->syntax #false joined-lines joined-srcloc #false))
       (datum->syntax #false (list normalized-id joined-lines-stx) this-syntax this-syntax)]
      #:parent-context-modifier (λ (stx) stx)
      #:parent-srcloc-modifier (λ (stx) stx)
      #:parent-props-modifier (λ (stx) stx)))


  (define (replace-option-identifiers-with-keywords stx)
    (syntax-traverse stx
      [((~and option-tag #:option) name:id expr)
       (define name-stx (attribute name))
       (define as-kw (string->keyword (symbol->string (syntax-e name-stx))))
       (define as-kw-stx (datum->syntax #false as-kw name-stx name-stx))
       (define new-datum (list (attribute option-tag) as-kw-stx (attribute expr)))
       (datum->syntax #false new-datum this-syntax this-syntax)]
      #:parent-context-modifier (λ (stx) stx)
      #:parent-srcloc-modifier (λ (stx) stx)
      #:parent-props-modifier (λ (stx) stx)))


  (define (add-uts-properties stx)
    (syntax-traverse stx
      #:datum-literals (require header test no-change-test analysis-test)

      [:id
       (define as-string (symbol->string (syntax-e this-syntax)))
       (syntax-property this-syntax 'uts-content as-string)]

      [((~and tag #:statement) require-id:require mod suite)
       (define tag-with-prop
         (syntax-property (attribute tag) 'uts-separators (list "" ": " " " "\n")))
       (define new-datum
         (list tag-with-prop
               (add-uts-properties (attribute require-id))
               (add-uts-properties (attribute mod))
               (add-uts-properties (attribute suite))))
       (datum->syntax #false new-datum this-syntax this-syntax)]

      [((~and tag #:statement) header-id:header code)
       (define tag-with-prop
         (syntax-property (attribute tag) 'uts-separators (list "" ":\n" "")))
       (define new-datum
         (list tag-with-prop
               (add-uts-properties (attribute header-id))
               (add-uts-properties (attribute code))))
       (datum->syntax #false new-datum this-syntax this-syntax)]

      [((~and tag #:statement) (~and test-id (~or test no-change-test analysis-test)) arg ...)
       (define separators (append (list "" ": " "\n") (make-list (length (attribute arg)) "")))
       (define tag-with-prop (syntax-property (attribute tag) 'uts-separators separators))
       (define new-datum
         (list* tag-with-prop
                (add-uts-properties (attribute test-id))
                (for/list ([arg-stx (in-list (attribute arg))])
                  (add-uts-properties arg-stx))))
       (datum->syntax #false new-datum this-syntax this-syntax)]

      [((~and tag #:code-line) code:str)
       (define tag-with-prop (syntax-property (attribute tag) 'uts-separators (list "- " "")))
       (define code-with-prop
         (syntax-property (attribute code) 'uts-content (syntax-e (attribute code))))
       (datum->syntax #false (list tag-with-prop code-with-prop) this-syntax this-syntax)]

      [((~and tag #:code-block) code:str)
       (define dash-line "--------------------\n")
       (define tag-with-prop
         (syntax-property (attribute tag) 'uts-separators (list dash-line dash-line)))
       (define code-with-prop
         (syntax-property (attribute code) 'uts-content (syntax-e (attribute code))))
       (datum->syntax #false (list tag-with-prop code-with-prop) this-syntax this-syntax)]

      [((~and tag #:option) option:keyword expr)
       (define expr-ends-in-newline?
         (syntax-parse (attribute expr)
           [((~or #:code-line #:code-block) _ ...) #true]
           [_ #false]))
       (define separators (list "@" "" (if expr-ends-in-newline? "" "\n")))
       (define tag-with-prop (syntax-property (attribute tag) 'uts-separators separators))
       (define option-stx (attribute option))
       (define option-as-string (keyword->string (syntax-e option-stx)))
       (define option-with-prop (syntax-property option-stx 'uts-content option-as-string))
       (define new-datum (list tag-with-prop option-with-prop (add-uts-properties (attribute expr))))
       (datum->syntax #false new-datum this-syntax this-syntax)]

      [((~and tag #:range-set) line-range ...)
       (define separators
         (append (list "")
                 (make-list (sub1 (length (attribute line-range))) ", ")
                 (list "")))
       (define tag-with-prop (syntax-property (attribute tag) 'uts-separators separators))
       (define new-datum
         (cons tag-with-prop
               (for/list ([line-range-stx (in-list (attribute line-range))])
                 (add-uts-properties line-range-stx))))
       (datum->syntax #false new-datum this-syntax this-syntax)]

      [((~and tag #:line-range) first last)
       (define tag-with-prop (syntax-property (attribute tag) 'uts-separators (list "" ".." "")))
       (define new-datum
         (list tag-with-prop
               (add-uts-properties (attribute first))
               (add-uts-properties (attribute last))))
       (datum->syntax #false new-datum this-syntax this-syntax)]

      [(~or :str :number)
       (define as-string (format "~v" (syntax-e this-syntax)))
       (syntax-property this-syntax 'uts-content as-string)]

      #:parent-context-modifier (λ (stx) stx)
      #:parent-srcloc-modifier (λ (stx) stx)
      #:parent-props-modifier (λ (stx) stx)))


  (define (read-using-syntax-reader syntax-reader in)
    (syntax->datum (syntax-reader #false in)))

  (define (srcloc-spanning first-stx last-stx)
    (define total-span
      (+ (- (syntax-position last-stx) (syntax-position first-stx)) (syntax-span last-stx)))
    (srcloc (syntax-source first-stx)
            (syntax-line first-stx)
            (syntax-column first-stx)
            (syntax-position first-stx)
            total-span)))


;@----------------------------------------------------------------------------------------------------


(module* resyntax racket/base


  (require racket/contract/base)
  

  (provide
   (contract-out
    [rename default-resyntax-test-recommendations refactoring-suite refactoring-suite?]))


  (require (except-in (submod "..") #%app)
           racket/list
           racket/string
           resyntax/base
           syntax/parse)


  (define-refactoring-rule unnecessary-multi-line-code-block
    #:description
    "Multi-line code blocks with a single line of code can be written in a more succinct form."
    #:uses-universal-tagged-syntax? #true
    #:literals (test)
    ((~and statement-tag #:statement) (~and test-id:test) test-name:str (#:code-block code:str) ...+)
    #:do [(define code-strings (map syntax-e (attribute code)))]
    #:when (for/and ([s (in-list code-strings)])
             (string-with-one-newline-at-end? s))
    #:with code-line-with-seps (syntax-property #'#:code-line 'uts-separators (list "- " ""))
    (statement-tag test-id test-name (code-line-with-seps code) ...))
  

  (define (string-with-one-newline-at-end? s)
    (equal? (string-find s "\n") (sub1 (string-length s))))

  
  (define-refactoring-suite default-resyntax-test-recommendations
    #:rules (unnecessary-multi-line-code-block)))
