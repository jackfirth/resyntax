#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [variable-mutability-analyzer expansion-analyzer?]))


(require racket/list
         racket/stream
         resyntax/private/analyzer
         resyntax/private/syntax-path
         resyntax/private/syntax-traversal
         syntax/id-table
         syntax/parse)


;@----------------------------------------------------------------------------------------------------


(define (syntax-label-id-phases expanded-stx)
  (let loop ([stx expanded-stx] [phase 0] [skip? #false])
  (syntax-traverse stx
    #:skip-root? skip?
    #:literal-sets (kernel-literals)
    [:id (syntax-property stx 'phase phase)]
    [(begin-for-syntax _ ...) (loop stx (add1 phase) #true)]
    [((~or module module*) _ ...) (loop stx 0 #true)])))


(define (binding-site-variables expanded-stx)
  (syntax-search expanded-stx
    #:literal-sets (kernel-literals)

    [(define-values (id ...) body)
     (stream-append (attribute id) (binding-site-variables (attribute body)))]

    [((~or let-values letrec-values) ([(id ...) rhs] ...) body ...)
     (define inner-exprs (append (attribute rhs) (attribute body)))
     (define ids (append* (attribute id)))
     (apply stream-append ids (map binding-site-variables inner-exprs))]

    [(#%plain-lambda formals body ...)
     (apply stream-append
            (syntax-search (attribute formals) [:id])
            (map binding-site-variables (attribute body)))]

    [(case-lambda [formals body ...] ...)
     (apply stream-append
            (syntax-search #'(formals ...) [:id])
            (map binding-site-variables (append* (attribute body))))]))


(define (mutated-variables expanded-stx)
  (syntax-search expanded-stx
    #:literal-sets (kernel-literals)
    [(set! id:id expr)
     (stream-cons (attribute id) (mutated-variables (attribute expr)))]))


(define (variable-mutability stx)
  (define labeled-stx (syntax-label-id-phases (syntax-label-paths stx 'expanded-path)))
  (define variable-table (make-hash))
  (for ([id (in-stream (binding-site-variables labeled-stx))])
    (define phase-specific-table
      (hash-ref! variable-table (syntax-property id 'phase) make-free-id-table))
    (free-id-table-set! phase-specific-table id 'immutable))
  (for ([id (in-stream (mutated-variables labeled-stx))])
    (define phase-specific-table (hash-ref variable-table (syntax-property id 'phase)))
    (free-id-table-set! phase-specific-table id 'mutable)))


(define variable-mutability-analyzer
  (make-expansion-analyzer variable-mutability #:name 'variable-mutability-analyzer))
