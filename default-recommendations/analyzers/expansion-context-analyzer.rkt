#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [expansion-context-analyzer expansion-analyzer?]))


(require racket/stream
         rebellion/streaming/transducer
         resyntax/private/analyzer
         resyntax/private/syntax-path
         resyntax/private/syntax-property-bundle
         resyntax/private/syntax-traversal
         syntax/parse)


;@----------------------------------------------------------------------------------------------------


(define (annotate-expansion-contexts expanded-stx)
  (let loop ([expanded-stx expanded-stx] [phase 0])
    (syntax-search expanded-stx
      #:literal-sets ([kernel-literals #:phase phase])
      
      ;; Phase mismatch - recurse with correct phase
      [(id:id _ ...)
       #:do [(define id-phase (syntax-property (attribute id) 'phase))]
       #:when (not (equal? id-phase phase))
       (loop this-syntax id-phase)]

      ;; Skip quote-syntax - no expansion context inside
      [(quote-syntax _) (stream)]
      
      ;; Forms directly under #%module-begin are in module context
      [(#%module-begin form ...)
       (for/stream ([form-stx (in-list (attribute form))])
         (define path (syntax-property form-stx 'expansion-path))
         (and path (syntax-property-entry path 'expansion-context 'module)))]
      
      ;; Body forms of lambda are in internal-definition context
      [(lambda formals body ...+)
       (for/stream ([body-stx (in-list (attribute body))])
         (define path (syntax-property body-stx 'expansion-path))
         (and path (syntax-property-entry path 'expansion-context 'internal-definition)))]
      
      ;; Body forms of case-lambda are in internal-definition context
      [(case-lambda [formals body ...+] ...)
       (for*/stream ([bodies (in-list (attribute body))]
                     [body-stx (in-list bodies)])
         (define path (syntax-property body-stx 'expansion-path))
         (and path (syntax-property-entry path 'expansion-context 'internal-definition)))]
      
      ;; Body forms of let-values and letrec-values are in internal-definition context
      [(~or (let-values ([vars rhs] ...) body ...+)
            (letrec-values ([vars rhs] ...) body ...+))
       (stream-append
        ;; RHS expressions are in expression context
        (for/stream ([rhs-stx (in-list (attribute rhs))])
          (define path (syntax-property rhs-stx 'expansion-path))
          (and path (syntax-property-entry path 'expansion-context 'expression)))
        ;; Body forms are in internal-definition context
        (for/stream ([body-stx (in-list (attribute body))])
          (define path (syntax-property body-stx 'expansion-path))
          (and path (syntax-property-entry path 'expansion-context 'internal-definition))))]
      
      ;; Subforms of #%plain-app (function applications) are in expression context
      [(app-id:id subform ...)
       #:when (free-identifier=? (attribute app-id) #'#%plain-app)
       (stream-filter
        values
        (for/stream ([subform-stx (in-list (attribute subform))])
          (define path (syntax-property subform-stx 'expansion-path))
          (and path (syntax-property-entry path 'expansion-context 'expression))))])))


(define expansion-context-analyzer
  (make-expansion-analyzer
   #:name 'expansion-context-analyzer
   (λ (expanded-stx)
     (define labeled-stx (syntax-label-paths expanded-stx 'expansion-path))
     (transduce (annotate-expansion-contexts labeled-stx)
                #:into into-syntax-property-bundle))))
