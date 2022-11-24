#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [syntax-shortcuts refactoring-suite?]))


(require racket/syntax
         rebellion/private/static-name
         resyntax/refactoring-rule
         resyntax/refactoring-suite
         syntax/parse)


;@----------------------------------------------------------------------------------------------------


(define-syntax-class format-id-argument
  #:attributes (uses-syntax-e? simplified)
  #:literals (syntax-e)

  (pattern (syntax-e simplified:expr)
    #:with uses-syntax-e? #'#true)

  (pattern simplified:expr
    #:with uses-syntax-e? #'#false))


(define-refactoring-rule syntax-e-in-format-id-unnecessary
  #:description
  "Using `syntax-e` on the arguments of `format-id` is unnecessary, `format-id` already unwrap
 syntax object arguments."
  #:literals (format-id syntax-e)

  [(format-id lctx:expr fmt:expr arg:format-id-argument ...+)
   #:when
   (for/or ([uses-syntax-e? (attribute arg.uses-syntax-e?)])
     (syntax-e uses-syntax-e?))

   (format-id lctx fmt arg.simplified ...)])


(define syntax-shortcuts
  (refactoring-suite
   #:name (name syntax-shortcuts)
   #:rules
   (list syntax-e-in-format-id-unnecessary)))
