#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [let-binding-suggestions (listof refactoring-rule?)]))


(require (for-syntax racket/base)
         (only-in racket/class
                  define/augment
                  define/augment-final
                  define/augride
                  define/overment
                  define/override
                  define/override-final
                  define/public
                  define/public-final
                  define/pubment
                  define/private)
         racket/list
         racket/match
         racket/sequence
         racket/syntax
         rebellion/base/immutable-string
         rebellion/base/option
         rebellion/private/guarded-block
         rebellion/type/object
         resyntax/default-recommendations/private/let-binding
         resyntax/refactoring-rule
         resyntax/default-recommendations/private/lambda-by-any-name
         resyntax/syntax-replacement
         syntax/parse
         syntax/parse/define
         syntax/parse/lib/function-header)


;@----------------------------------------------------------------------------------------------------


(define-refactoring-rule let-to-define
  #:description "Internal definitions are recommended instead of let expressions, to reduce nesting."
  [(header:header-form-allowing-internal-definitions let-expr:body-with-refactorable-let-expression)
   (header.formatted ... let-expr.refactored ...)])


(define-splicing-syntax-class header-form-allowing-internal-definitions
  #:attributes ([formatted 1])
  #:literals (let let* let-values when unless with-handlers parameterize)

  (pattern (~seq lambda:lambda-by-any-name ~! formals:formals)
    #:with (formatted ...) #'(lambda formals))

  (pattern (~seq define:define-by-any-name ~! header:function-header)
    #:with (formatted ...) #'(define header))

  (pattern (~seq let ~! (~optional name:id) header)
    #:with (formatted ...) #'(let (~? name) header))

  (pattern (~seq let* ~! header)
    #:with (formatted ...) #'(let* header))

  (pattern (~seq let-values ~! header)
    #:with (formatted ...) #'(let-values header))

  (pattern (seq when ~! condition)
    #:with (formatted ...) #'(when condition))

  (pattern (seq unless ~! condition)
    #:with (formatted ...) #'(unless condition))

  (pattern (seq with-handlers ~! handlers)
    #:with (formatted ...) #'(with-handlers handlers))

  (pattern (seq parameterize ~! handlers)
    #:with (formatted ...) #'(parameterize handlers)))


;; There's a lot of variants of define that support the same grammar but have different meanings. We
;; can recognize and refactor all of them with this syntax class.
(define-syntax-class define-by-any-name
  #:literals (define
               define/augment
               define/augment-final
               define/augride
               define/overment
               define/override
               define/override-final
               define/public
               define/public-final
               define/pubment
               define/private)
  (pattern
      (~or define
           define/augment
           define/augment-final
           define/augride
           define/overment
           define/override
           define/override-final
           define/public
           define/public-final
           define/pubment
           define/private)))


(define-refactoring-rule and-let-to-cond-define
  #:description "This and expression can be turned into a cond expression with internal definitions,\
 reducing nesting."
  #:literals (and let)
  [(and guard-expr let-expr:refactorable-let-expression)
   (cond
     NEWLINE [(not guard-expr) #false]
     NEWLINE [else let-expr.refactored ...])])


(define-syntax-class cond-clause
  #:attributes ([formatted 1])
  #:literals (else =>)
  (pattern (~and clause (~or [else body ...+] [expr:expr => body-handler:expr] [expr:expr body ...+]))
    #:with (formatted ...)
    #'(NEWLINE clause)))


(define-syntax-class refactorable-cond-clause
  #:attributes ([refactored 1])
  #:literals (else =>)

  (pattern [else let-expr:body-with-refactorable-let-expression]
    #:with (refactored ...) #'(NEWLINE [else let-expr.refactored ...]))
  
  (pattern (~and [expr let-expr:body-with-refactorable-let-expression] (~not [expr => _ ...]))
    #:with (refactored ...) #'(NEWLINE [expr let-expr.refactored ...])))


(define-refactoring-rule cond-let-to-cond-define
  #:description "The body of a cond clause supports internal definitions, which are preferred over\
 let expressions because they reduce nesting."
  #:literals (cond)
  [(cond
     clause-before:cond-clause ...
     refactorable:refactorable-cond-clause
     clause-after:cond-clause ...)
   (cond
     clause-before.formatted ... ...
     refactorable.refactored ...
     clause-after.formatted ... ...)])


(define-refactoring-rule if-then-let-to-cond-define
  #:description "This if expression can be turned into a cond expression with internal definitions,\
 reducing nesting."
  #:literals (if)
  [(if condition let-expr:refactorable-let-expression else-expr)
   (cond
     NEWLINE [condition let-expr.refactored ...]
     NEWLINE [else NEWLINE else-expr])])


(define-refactoring-rule if-else-let-to-cond-define
  #:description "This if expression can be turned into a cond expression with internal definitions,\
 reducing nesting."
  #:literals (if)
  [(if condition then-expr let-expr:refactorable-let-expression)
   (cond
     NEWLINE [condition NEWLINE then-expr]
     NEWLINE [else let-expr.refactored ...])])


(define-refactoring-rule let*-once-to-let
  #:description "A let* expression with a single binding is equivalent to a let expression."
  #:literals (let*)
  [(let* (~and header ([id:id rhs:expr])) body ...)
   (let header (~@ NEWLINE body) ...)])


(define let-binding-suggestions
  (list let-to-define
        and-let-to-cond-define
        cond-let-to-cond-define
        if-then-let-to-cond-define
        if-else-let-to-cond-define
        let*-once-to-let))
