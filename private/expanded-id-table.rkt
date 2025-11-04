#lang racket/base


(require racket/contract/base)


(provide
 (struct-out expanded-identifier)
 (contract-out
  [expanded-id-table? (-> any/c boolean?)]
  [make-expanded-id-table (-> expanded-id-table?)]
  [expanded-id-table-ref
   (->* (expanded-id-table? expanded-identifier?) (failure-result/c) any/c)]
  [expanded-id-table-set! (-> expanded-id-table? expanded-identifier? any/c void?)]
  [in-expanded-id-table
   (-> expanded-id-table? (sequence/c (entry/c expanded-identifier? any/c)))]))


(require guard
         racket/contract/base
         racket/dict
         racket/match
         racket/sequence
         racket/stream
         rebellion/base/result
         rebellion/collection/entry
         syntax/id-table)


(module+ test
  (require (submod "..")
           rackunit))


;@----------------------------------------------------------------------------------------------------


(struct expanded-identifier (syntax phase)
  #:transparent
  #:guard (struct-guard/c identifier? (or/c exact-nonnegative-integer? #false)))


(struct expanded-id-table (table)
  #:transparent
  #:mutable)


(define (make-expanded-id-table)
  (expanded-id-table (make-hasheq)))


(define (expanded-id-table-ref table id [failure-result (λ () (error 'expanded-id-table-ref "no mapping for ~a" id))])
  (define phase (expanded-identifier-phase id))
  (define stx (expanded-identifier-syntax id))
  (define phase-table (hash-ref (expanded-id-table-table table) phase #false))
  (if phase-table
      (free-id-table-ref phase-table stx failure-result)
      (if (procedure? failure-result)
          (failure-result)
          failure-result)))


(define (expanded-id-table-set! table id value)
  (define phase (expanded-identifier-phase id))
  (define stx (expanded-identifier-syntax id))
  (define phase-table
    (hash-ref! (expanded-id-table-table table)
               phase
               (λ () (make-free-id-table #:phase phase))))
  (free-id-table-set! phase-table stx value))


(define (in-expanded-id-table table)
  (for*/stream ([(phase phase-table) (in-hash (expanded-id-table-table table))]
                [(stx value) (in-free-id-table phase-table)])
    (entry (expanded-identifier stx phase) value)))


;@----------------------------------------------------------------------------------------------------


(module+ test
  (test-case "expanded-id-table"
    
    (test-case "make-expanded-id-table creates empty table"
      (define table (make-expanded-id-table))
      (check-pred expanded-id-table? table))
    
    (test-case "expanded-id-table-set! and expanded-id-table-ref"
      (define table (make-expanded-id-table))
      (define id1 (expanded-identifier #'x 0))
      (expanded-id-table-set! table id1 'foo)
      (check-equal? (expanded-id-table-ref table id1) 'foo))
    
    (test-case "identifiers at different phases are distinct"
      (define table (make-expanded-id-table))
      (define id-phase0 (expanded-identifier #'x 0))
      (define id-phase1 (expanded-identifier #'x 1))
      (expanded-id-table-set! table id-phase0 'phase0-value)
      (expanded-id-table-set! table id-phase1 'phase1-value)
      (check-equal? (expanded-id-table-ref table id-phase0) 'phase0-value)
      (check-equal? (expanded-id-table-ref table id-phase1) 'phase1-value))
    
    (test-case "in-expanded-id-table iterates over entries"
      (define table (make-expanded-id-table))
      (define id1 (expanded-identifier #'x 0))
      (define id2 (expanded-identifier #'y 0))
      (define id3 (expanded-identifier #'z 1))
      (expanded-id-table-set! table id1 'val1)
      (expanded-id-table-set! table id2 'val2)
      (expanded-id-table-set! table id3 'val3)
      (define entries (for/list ([e (in-expanded-id-table table)]) e))
      (check-equal? (length entries) 3))))
