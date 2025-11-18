#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [simplify-named-let-initialization refactoring-suite?]))


(require racket/list
         racket/syntax
         resyntax/base
         resyntax/default-recommendations/private/definition-context
         resyntax/default-recommendations/private/pure-expression
         resyntax/default-recommendations/private/syntax-lines
         syntax/parse)


;@----------------------------------------------------------------------------------------------------


(define-definition-context-refactoring-rule simplify-named-let-initialization-rule
  #:description
  "Complex multi-line initialization expressions in named `let` loops can be extracted into `define`\
 bindings to improve readability."
  #:literals (let)
  (~seq leading-body ...
        (let loop-name:id ([binding-id:id binding-expr:expr] ...)
          loop-body ...))
  
  #:do [(define-values (bindings-to-extract remaining-bindings)
          (for/fold ([extracted '()]
                     [remaining '()])
                    ([id (in-list (attribute binding-id))]
                     [expr (in-list (attribute binding-expr))])
            (if (multiline-syntax? expr)
                (values (cons (list id expr) extracted)
                        remaining)
                (values extracted
                        (cons (list id expr) remaining)))))]
  
  ;; Check that at least one binding expression is multi-line
  #:when (not (null? bindings-to-extract))
  
  ;; Check that all non-multi-line (remaining) binding expressions are pure
  ;; (so we can safely reorder by extracting the multi-line ones)
  #:when (for/and ([binding (in-list remaining-bindings)])
           (syntax-parse (cadr binding)
             [:pure-expression #true]
             [_ #false]))
  
  #:with ((extracted-id extracted-expr) ...) (reverse bindings-to-extract)
  #:with ((kept-id kept-expr) ...) (reverse remaining-bindings)
  #:with (init-id ...) (for/list ([id (in-list (attribute extracted-id))])
                         (format-id id "init-~a" id))
  
  (leading-body ...
   (define init-id extracted-expr) ...
   (let loop-name ([kept-id kept-expr] ...
                   [extracted-id init-id] ...)
     loop-body ...)))


(define-refactoring-suite simplify-named-let-initialization
  #:rules (simplify-named-let-initialization-rule))
