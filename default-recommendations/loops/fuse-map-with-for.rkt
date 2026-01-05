#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [fuse-map-with-for refactoring-suite?]))


(require resyntax/base
         racket/list
         resyntax/default-recommendations/analyzers/identifier-usage
         resyntax/default-recommendations/let-replacement/private/let-binding
         resyntax/default-recommendations/private/lambda-by-any-name
         syntax/parse)


;@----------------------------------------------------------------------------------------------------


;; A short lambda suitable for fusing with a for loop. For multi-body lambdas, we need to
;; separate the prefix forms (all but last) from the result form (the last).
(define-syntax-class fuseable-map-lambda
  #:attributes (x single-body [multi-body 1] [prefix-forms 1] result-form)

  ;; Lambdas with let expressions that can be refactored
  (pattern
    (_:lambda-by-any-name (x:id)
                          original-body:body-with-refactorable-let-expression)
    #:with (multi-body ...) #'(original-body.refactored ...)
    #:do [(define refactored-forms (attribute original-body.refactored))
          (define prefix-list (if (null? refactored-forms) '() (drop-right refactored-forms 1)))
          (define result (if (null? refactored-forms) #'(begin) (last refactored-forms)))]
    #:attr [prefix-forms 1] prefix-list
    #:attr result-form result
    #:attr single-body #'(begin original-body.refactored ...))

  ;; Lambdas with multiple body forms (two or more)
  (pattern (_:lambda-by-any-name (x:id) prefix-form ... last-form)
    #:when (not (null? (attribute prefix-form)))
    #:with (multi-body ...) #'(prefix-form ... last-form)
    #:attr [prefix-forms 1] (attribute prefix-form)
    #:attr result-form #'last-form
    #:attr single-body #'(begin prefix-form ... last-form))

  ;; Short lambdas with a single body form
  (pattern (_:lambda-by-any-name (x:id) only-form)
    #:with (multi-body ...) #'(only-form)
    #:attr [prefix-forms 1] '()
    #:attr result-form #'only-form
    #:attr single-body #'only-form))


(define-definition-context-refactoring-rule fuse-map-with-for*-rule
  #:description
  "A `map` expression producing a list for a `for*` loop can be fused with the loop."
  #:analyzers (list identifier-usage-analyzer)
  #:literals (define map in-list for*)
  (~seq body-before ...
        (define ys:id (map function:fuseable-map-lambda list-expr:expr))
        (for*-id:for*
         (~and original-clauses
               ([y-var:id (in-list ys-usage:id)] remaining-clause ...))
         for-body ...)
        body-after ...)

  ;; Check that ys is only used in the for loop, not elsewhere
  #:when (free-identifier=? (attribute ys) (attribute ys-usage))
  #:when (equal? (syntax-property #'ys 'usage-count) 1)

  ;; Generate the refactored code - fuse as nested clauses
  (body-before ...
   (for*-id ([function.x (in-list list-expr)]
             [y-var (in-list function.single-body)]
             remaining-clause ...)
            for-body ...)
   body-after ...))


;; Rule for when there are single-clause for loops (not for*) - use internal definition
(define-definition-context-refactoring-rule fuse-map-with-for-single-clause-rule
  #:description
  "A `map` expression producing a list for a single-clause `for` loop can be fused with the loop."
  #:analyzers (list identifier-usage-analyzer)
  #:literals (define map in-list for)
  (~seq body-before ...
        (define ys:id (map function:fuseable-map-lambda list-expr:expr))
        (for-id:for
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
           function.prefix-forms ...
           (define y-var function.result-form)
           for-body ...)
   body-after ...))


(define-refactoring-suite fuse-map-with-for
  #:rules (fuse-map-with-for*-rule
           fuse-map-with-for-single-clause-rule))
