#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [function-expression-analyzer expansion-analyzer?]))


(require racket/stream
         rebellion/streaming/transducer
         resyntax/private/analyzer
         resyntax/private/syntax-path
         resyntax/private/syntax-property-bundle
         resyntax/private/syntax-traversal
         syntax/parse)


(module+ test
  (require (submod "..")
           rackunit))


;@----------------------------------------------------------------------------------------------------


(define (annotate-application-subexpressions expanded-stx)
  (let loop ([expanded-stx expanded-stx] [phase 0])
    (syntax-search expanded-stx
      #:literal-sets ([kernel-literals #:phase phase])
      
      ;; Phase mismatch - recurse with correct phase
      [(id:id _ ...)
       #:do [(define id-phase (syntax-property (attribute id) 'phase))]
       #:when (not (equal? id-phase phase))
       (loop this-syntax id-phase)]

      ;; Skip quote-syntax - no function applications inside
      [(quote-syntax _) (stream)]
      
      ;; Function application - annotate function and arguments
      ;; Note: In fully expanded code, we need to match #%plain-app using identifier comparison
      [(app-id:id func arg ...)
       #:when (free-identifier=? (attribute app-id) #'#%plain-app)
       #:do [(define func-path (syntax-property (attribute func) 'expansion-path))]
       #:when func-path
       (define func-entry (syntax-property-entry func-path 'application-subexpression-kind 'function))
       (define arg-entries
         (for/stream ([arg-stx (in-list (attribute arg))])
           (define arg-path (syntax-property arg-stx 'expansion-path))
           (and arg-path
                (syntax-property-entry arg-path 'application-subexpression-kind 'argument))))
       (stream-cons func-entry (stream-filter values arg-entries))])))


(define function-expression-analyzer
  (make-expansion-analyzer
   #:name 'function-expression-analyzer
   (λ (expanded-stx)
     (define labeled-stx (syntax-label-paths expanded-stx 'expansion-path))
     (transduce (annotate-application-subexpressions labeled-stx)
                #:into into-syntax-property-bundle))))


(module+ test
  (test-case "function-expression-analyzer"

    (test-case "empty module"
      (define stx #'(module foo racket/base))
      (define props (expansion-analyze function-expression-analyzer (expand stx)))
      ;; Even empty modules have configure-runtime calls, so we check that we get some properties
      (check-true (syntax-property-bundle? props)))))
