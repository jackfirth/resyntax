#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [fuse-map-with-for refactoring-suite?]))


(require resyntax/base
         resyntax/default-recommendations/analyzers/identifier-usage
         resyntax/default-recommendations/let-replacement/private/let-binding
         resyntax/default-recommendations/private/lambda-by-any-name
         syntax/parse)


;@----------------------------------------------------------------------------------------------------


;; A short lambda suitable for fusing with a for loop. Needs both single-attribute and 
;; multi-attribute versions of the body for different template contexts.
(define-syntax-class fuseable-map-lambda
  #:attributes (x single-body [multi-body 1])

  ;; Lambdas with let expressions that can be refactored - must come first as it's most specific
  (pattern
    (_:lambda-by-any-name (x:id)
                          original-body:body-with-refactorable-let-expression)
    #:attr [multi-body 1] (attribute original-body.refactored)
    ;; For single body, we need to wrap multiple forms in a begin
    #:attr single-body #'(begin original-body.refactored ...))

  ;; Lambdas with multiple body forms (two or more)
  (pattern (_:lambda-by-any-name (x:id) first-form second-form rest-form ...)
    #:attr [multi-body 1] (cons (attribute first-form)
                                 (cons (attribute second-form) (attribute rest-form)))
    #:attr single-body #'(begin first-form second-form rest-form ...))

  ;; Short lambdas with a single body form
  (pattern (_:lambda-by-any-name (x:id) only-form)
    #:attr [multi-body 1] (list (attribute only-form))
    #:attr single-body #'only-form))


(define-definition-context-refactoring-rule fuse-map-with-for-rule
  #:description
  "A `map` expression producing a list for a `for` loop can be fused with the loop."
  #:analyzers (list identifier-usage-analyzer)
  #:literals (define map in-list for for*)
  (~seq body-before ...
        (define ys:id (map function:fuseable-map-lambda list-expr:expr))
        ((~or for-id:for for-id:for*)
         (~and original-clauses
               ([y-var:id (in-list ys-usage:id)] remaining-clause ...+))
         for-body ...)
        body-after ...)

  ;; Check that ys is only used in the for loop, not elsewhere
  #:when (free-identifier=? (attribute ys) (attribute ys-usage))
  #:when (equal? (syntax-property #'ys 'usage-count) 1)

  ;; Generate the refactored code - fuse as nested clauses
  (body-before ...
   (for-id ([function.x (in-list list-expr)]
            [y-var (in-list (function.multi-body ...))]
            remaining-clause ...)
           for-body ...)
   body-after ...))


;; Rule for when there are no remaining clauses - use internal definition
(define-definition-context-refactoring-rule fuse-map-with-for-single-clause-rule
  #:description
  "A `map` expression producing a list for a `for` loop can be fused with the loop."
  #:analyzers (list identifier-usage-analyzer)
  #:literals (define map in-list for for*)
  (~seq body-before ...
        (define ys:id (map function:fuseable-map-lambda list-expr:expr))
        ((~or for-id:for for-id:for*)
         (~and original-clauses
               ([y-var:id (in-list ys-usage:id)]))
         for-body ...)
        body-after ...)

  ;; Check that ys is only used in the for loop, not elsewhere
  #:when (free-identifier=? (attribute ys) (attribute ys-usage))
  #:when (equal? (syntax-property #'ys 'usage-count) 1)

  ;; Generate the refactored code - use internal definition
  (body-before ...
   (for-id ([function.x (in-list list-expr)])
           (define y-var function.single-body)
           for-body ...)
   body-after ...))


(define-refactoring-suite fuse-map-with-for
  #:rules (fuse-map-with-for-rule
           fuse-map-with-for-single-clause-rule))
