#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [class-shortcuts refactoring-suite?]))


(require racket/class
         resyntax/base
         syntax/parse)


;@----------------------------------------------------------------------------------------------------


(define-syntax-class manual-method-chain
  #:attributes (initial-object [method 1] [arg 2])
  #:literals (send)

  (pattern (send inner-chain:manual-method-chain last-method:id last-method-arg ...)
    #:attr initial-object (attribute inner-chain.initial-object)
    #:attr [method 1] (append (attribute inner-chain.method) (list #'last-method))
    #:attr [arg 2] (append (attribute inner-chain.arg) (list (attribute last-method-arg))))

  (pattern (send (~and initial-object (~not _:manual-method-chain))
                 first-method:id
                 first-method-arg ...)
    #:attr [method 1] (list #'first-method)
    #:attr [arg 2] (list (attribute first-method-arg))))


(define-refactoring-rule send-chain-to-send+
  #:description
  "This method chain made of nested `send` expressions can be written more clearly as a `send+`\
 expression."
  chain:manual-method-chain
  #:when (>= (length (attribute chain.method)) 3)
  (send+ chain.initial-object (chain.method chain.arg ...) ...))


(define-refactoring-rule instantiate-to-make-object
  #:description "The `instantiate` form is for mixing positional and by-name constructor arguments.\
 When no by-name arguments are needed, use `make-object` instead."
  #:literals (instantiate)
  (instantiate cls (by-position-arg ...+))
  (make-object cls by-position-arg ...))


(define-refactoring-rule instantiate-to-new
  #:description "The `instantiate` form is for mixing positional and by-name constructor arguments.\
 When no positional arguments are needed, use `new` instead."
  #:literals (instantiate)
  (instantiate cls () by-name-arg ...+)
  (new cls by-name-arg ...))


(define-refactoring-suite class-shortcuts
  #:rules (instantiate-to-make-object
           instantiate-to-new
           send-chain-to-send+))
