#lang racket/base


(provide #%app
         #%datum
         #%module-begin
         begin
         code-block
         header:
         line-range
         range-set
         require:
         statement
         test:
         no-change-test:
         analysis-test:)


(require (for-syntax racket/base
                     racket/sequence
                     resyntax/test/private/statement
                     syntax/parse)
         racket/stxparam
         rackunit
         rebellion/base/comparator
         rebellion/base/range
         rebellion/collection/range-set
         resyntax/test/private/rackunit
         resyntax/default-recommendations
         syntax/parse/define
         (only-in racket/base [#%module-begin racket-module-begin]))


;@----------------------------------------------------------------------------------------------------


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


(define-syntax require:
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
  (define-syntax-class literal-code-block
    #:description "a code block"
    #:opaque
    #:literals (code-block)
    (pattern (code-block str:str))))


(define-syntax header:
  (statement-transformer
   (λ (stx)
     (syntax-parse stx
       #:track-literals
       [(_ _ header-code:literal-code-block)
        ; Using syntax/loc so that errors thrown by set-header! point to the header:
        ; statement.
        (syntax/loc stx (set-header! header-code))]))))


(begin-for-syntax
  (define-splicing-syntax-class test-parameters
    #:attributes ([id 1] [value 1])
    #:literals (range-set)
    #:datum-literals (option @lines)

    (pattern (~seq)
      #:with (id ...) '()
      #:with (value ...) '())

    (pattern (~seq (option @lines (~and line-set (range-set . _))))
      #:with (id ...) (list #'current-line-mask)
      #:with (value ...) (list #'line-set)))
  
  (define-splicing-syntax-class code-block-test-args
    #:attributes ([check 1])

    (pattern (~seq input-code:literal-code-block expected-code:literal-code-block)
      #:with (check ...)
      (list (syntax/loc #'input-code (check-suite-refactors input-code expected-code))))

    (pattern (~seq input-code:literal-code-block ...+
                   expected-code:literal-code-block)
      #:when (>= (length (attribute input-code)) 2)
      #:with (check ...)
      (for/list ([input-stx (in-list (attribute input-code))])
        (quasisyntax/loc input-stx
          (check-suite-refactors #,input-stx expected-code))))))


(define-syntax test:
  (statement-transformer
   (λ (stx)
     (syntax-parse stx
       #:track-literals
       [(_ _ name:str params:test-parameters args:code-block-test-args)
        #`(test-case name
            (parameterize ([params.id params.value] ...)
              args.check ...))]))))


(define-syntax no-change-test:
  (statement-transformer
   (λ (stx)
     (syntax-parse stx
       #:track-literals
       [(_ _ name:str params:test-parameters code:literal-code-block)
        #`(test-case name
            (parameterize ([params.id params.value] ...)
              #,(syntax/loc #'code (check-suite-does-not-refactor code))))]))))


(begin-for-syntax
  (define-syntax-class property-value
    #:description "a literal syntax property value (an unquoted symbol, boolean, number, or string)"
    (pattern (~or :id :boolean :number :str))))


(define-syntax analysis-test:
  (statement-transformer
   (λ (stx)
     (syntax-parse stx
       #:track-literals
       #:datum-literals (option @within @inspect @property @assert)
       [(_ _ name:str
           code:literal-code-block
           (~seq (option @within context-block:literal-code-block) ...
                 (option @inspect target-block:literal-code-block)
                 (option @property property-key:id)
                 (~and assert-option (option @assert expected-value:property-value))))
        #`(test-case name
            #,(syntax/loc this-syntax
                (check-suite-analysis code
                                      (list context-block ...)
                                      target-block
                                      'property-key
                                      'expected-value)))]))))


;; Helper function to check if any require: statements are present
(begin-for-syntax
  (define (has-require-statements? body-stx)
    (for/or ([stmt (in-list (syntax->list body-stx))])
      (syntax-parse stmt
        #:literals (statement require:)
        [(statement require: . _) #true]
        [_ #false]))))


;; Custom #%module-begin that automatically includes default-recommendations
;; when no explicit require: statements are present
(define-syntax (#%module-begin stx)
  (syntax-parse stx
    #:literals (begin)
    [(_ (begin . body)) ; The brag grammar adds a (begin ...) around everything in the module
     (define has-require? (has-require-statements? #'body))
     (if has-require?
         #`(racket-module-begin . body)
         #`(racket-module-begin 
            (add-suite-under-test! default-recommendations)
            . body))]))


(define (line-range first-line last-line)
  (closed-range first-line last-line #:comparator natural<=>))


;@----------------------------------------------------------------------------------------------------


(module reader racket/base


  (require racket/contract/base)


  (provide
   (contract-out
    [read procedure?]
    [read-syntax procedure?]))


  (require racket/list
           resyntax/private/syntax-traversal
           resyntax/test/private/grammar
           resyntax/test/private/tokenizer
           syntax/parse)
  

  ;@--------------------------------------------------------------------------------------------------


  (define (read in)
    (read-using-syntax-reader read-syntax in))


  (define (read-syntax source-name in)
    (define parse-tree (parse source-name (make-refactoring-test-tokenizer in)))
    (define cleaned-parse-tree
      (syntax-traverse parse-tree
        #:datum-literals (standalone-code-block
                          starting-code-block
                          middle-code-block
                          ending-code-block)
        [((~or id:standalone-code-block
               id:starting-code-block
               id:middle-code-block
               id:ending-code-block)
          line:str
          ...)
         (define id-stx (attribute id))
         (define normalized-id (datum->syntax #false 'code-block id-stx id-stx))
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
    (define module-datum
      `(module refactoring-test racket/base
         (module test resyntax/test
           ,cleaned-parse-tree)))
    (datum->syntax #f module-datum))


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
