#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [expansion-analyzer? (-> any/c boolean?)]
  [make-expansion-analyzer
   (->* ((-> syntax? syntax-property-bundle?)) (#:name (and/c symbol? symbol-interned?))
        expansion-analyzer?)]
  [expansion-analyze (-> expansion-analyzer? syntax? syntax-property-bundle?)]))


(require rebellion/custom-write
         resyntax/private/syntax-property-bundle)


;@----------------------------------------------------------------------------------------------------


(struct expansion-analyzer (name implementation)
  #:omit-define-syntaxes
  #:constructor-name constructor:expansion-analyzer
  #:property prop:object-name (struct-field-index name)
  #:property prop:custom-write (make-named-object-custom-write 'expansion-analyzer))


(define (make-expansion-analyzer implementation #:name [name #false])
  (constructor:expansion-analyzer name implementation))


(define (expansion-analyze analyzer expanded-syntax)
  ((expansion-analyzer-implementation analyzer) expanded-syntax))
