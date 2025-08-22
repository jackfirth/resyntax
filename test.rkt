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
     (define has-require? (has-require-statements? (attribute body)))
     (if has-require?
         #`(racket-module-begin (module+ test body ...))
         #`(racket-module-begin
            (module+ test
              (add-suite-under-test! default-recommendations)
              body ...)))]))


;@----------------------------------------------------------------------------------------------------


(module reader racket/base


  (require racket/contract/base)


  (provide
   (contract-out
    [read procedure?]
    [read-syntax procedure?]))


  (require racket/list
           racket/syntax-srcloc
           resyntax/private/syntax-traversal
           resyntax/test/private/grammar
           resyntax/test/private/tokenizer
           syntax/parse)
  

  (define (read in)
    (read-using-syntax-reader read-syntax in))


  (define (read-syntax source-name in)
    (define parse-tree (parse source-name (make-refactoring-test-tokenizer in)))
    (define cleaned-parse-tree
      (replace-option-identifiers-with-keywords
       (join-multiline-code-blocks
        (replace-grammar-tags-with-shape-tags parse-tree))))
    (define statements
      (syntax-parse cleaned-parse-tree
        [(#:program statement ...) (attribute statement)]))
    (define module-datum
      `(module refactoring-test resyntax/test
         ,@statements))
    (define whole-program-srcloc (syntax-srcloc cleaned-parse-tree))
    (datum->syntax #f module-datum whole-program-srcloc))


  (define (replace-grammar-tags-with-shape-tags grammar-stx)
    (syntax-traverse grammar-stx
      [(tag:id subform ...)
       (define tag-stx (attribute tag))
       (define as-kw (string->keyword (symbol->string (syntax-e tag-stx))))
       (define as-kw-stx (datum->syntax #false as-kw tag-stx #false))
       (replace-grammar-tags-with-shape-tags
        (datum->syntax #false (cons as-kw-stx (attribute subform)) this-syntax #false))]
      #:parent-context-modifier (λ (stx) stx)
      #:parent-srcloc-modifier (λ (stx) stx)
      #:parent-props-modifier (λ (stx) stx)))


  (define-syntax-class multi-line-code-block-tag
    (pattern
      (~or #:standalone-code-block #:starting-code-block #:middle-code-block #:ending-code-block)))


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
       (datum->syntax #false (list normalized-id joined-lines-stx) this-syntax #false)]
      #:parent-context-modifier (λ (stx) stx)
      #:parent-srcloc-modifier (λ (stx) stx)
      #:parent-props-modifier (λ (stx) stx)))


  (define (replace-option-identifiers-with-keywords stx)
    (syntax-traverse stx
      [((~and option-tag #:option) name:id expr)
       (define name-stx (attribute name))
       (define as-kw (string->keyword (symbol->string (syntax-e name-stx))))
       (define as-kw-stx (datum->syntax #false as-kw name-stx #false))
       (define new-datum (list (attribute option-tag) as-kw-stx (attribute expr)))
       (datum->syntax #false new-datum this-syntax #false)]
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
    #:literals (statement test: code-block)
    (statement-id:statement (~and test-id (~literal test:)) test-name:str (code-block code:str) ...+)
    #:do [(define code-strings (map syntax-e (attribute code)))]
    #:when (for/and ([s (in-list code-strings)])
             (string-with-one-newline-at-end? s))
    #:with tagged-statement-id
    (syntax-property (attribute statement-id)
                     'uts-separators
                     (list* "" " " "\n" (make-list (length (attribute code)) "")))
    #:with code-line-id (syntax-property #'code-line 'uts-separators (list "- " ""))
    #:with (replacement-code ...)
    (for/list ([code-stx (in-list (attribute code))])
      (syntax-property code-stx 'uts-atom-content (syntax-e code-stx)))
    (tagged-statement-id test-id test-name (code-line-id replacement-code) ...))
  

  (define (string-with-one-newline-at-end? s)
    (equal? (string-find s "\n") (sub1 (string-length s))))

  
  (define-refactoring-suite default-resyntax-test-recommendations
    #:rules (unnecessary-multi-line-code-block)))
