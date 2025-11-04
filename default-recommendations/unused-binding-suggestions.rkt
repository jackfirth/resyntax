#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [unused-binding-suggestions refactoring-suite?]))


(require racket/list
         resyntax/base
         resyntax/default-recommendations/analyzers/identifier-usage
         resyntax/default-recommendations/private/pure-expression
         syntax/parse)


;@----------------------------------------------------------------------------------------------------


(define-syntax-class side-effect-free-definition
  #:attributes (id)
  #:literals (define)
  (pattern (define (id:id . _) . _))
  (pattern (define id:id :pure-expression)))


(define-definition-context-refactoring-rule unused-definition
  #:description "This definition is not used."
  #:analyzers (list identifier-usage-analyzer)
  (~seq before ... definition:side-effect-free-definition first-after remaining-after ...)
  #:when (equal? (syntax-property (attribute definition.id) 'usage-count) 0)
  (before ...
   (~focus-replacement-on (~replacement first-after #:original-splice (definition first-after)))
   remaining-after ...))


(define-refactoring-suite unused-binding-suggestions
  #:rules (unused-definition))
