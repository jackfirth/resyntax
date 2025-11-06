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
   (-> expanded-id-table? (sequence/c (entry/c expanded-identifier? any/c)))]
  [in-expanded-id-table-phase
   (-> expanded-id-table? (or/c exact-nonnegative-integer? #false) (sequence/c (entry/c expanded-identifier? any/c)))]
  [syntax-label-id-phases (-> syntax? syntax?)]
  [fully-expanded-syntax-id-table (-> syntax? expanded-id-table?)]))


(require guard
         racket/contract/base
         racket/dict
         racket/list
         racket/match
         racket/sequence
         racket/stream
         rebellion/base/result
         rebellion/collection/entry
         resyntax/private/syntax-traversal
         syntax/id-table
         syntax/parse)


(module+ test
  (require (submod "..")
           rackunit))


;@----------------------------------------------------------------------------------------------------


(struct expanded-identifier (syntax phase)
  #:transparent
  #:guard (struct-guard/c identifier? (or/c exact-nonnegative-integer? #false)))


(struct expanded-id-table (table))


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


(define (in-expanded-id-table-phase table phase)
  (define phase-table (hash-ref (expanded-id-table-table table) phase #false))
  (if phase-table
      (for/stream ([(stx value) (in-free-id-table phase-table)])
        (entry (expanded-identifier stx phase) value))
      (stream)))


;@----------------------------------------------------------------------------------------------------


;; Label syntax with phase information
(define (syntax-label-id-phases expanded-stx)
  (let loop ([expanded-stx expanded-stx] [phase 0] [skip? #false])
    (syntax-traverse expanded-stx
      #:skip-root? skip?
      #:literal-sets ([kernel-literals #:phase phase])

      [:id (syntax-property this-syntax 'phase phase)]
      [(begin-for-syntax _ ...) (loop this-syntax (add1 phase) #true)]

      [(define-syntaxes-id:define-syntaxes ids expr)
       (define new-define-syntaxes (loop (attribute define-syntaxes-id) phase #false))
       (define new-ids (loop (attribute ids) phase #true))
       (define new-expr (loop (attribute expr) (add1 phase) #false))
       (define new-datum (list new-define-syntaxes new-ids new-expr))
       (datum->syntax this-syntax new-datum this-syntax this-syntax)]

      [((~or module module*) _ ...) (loop this-syntax 0 #true)]

      #:parent-context-modifier (λ (stx) stx)
      #:parent-srcloc-modifier (λ (stx) stx)
      #:parent-props-modifier (λ (stx) stx))))


;; Find all binding sites and return them as a stream of identifiers
(define (binding-site-identifiers expanded-stx)
  (let loop ([expanded-stx expanded-stx] [phase 0])
    (define (recur stx)
      (loop stx phase))
    (syntax-search expanded-stx
      #:literal-sets ([kernel-literals #:phase phase])

      [(id:id _ ...)
       #:do [(define id-phase (syntax-property (attribute id) 'phase))]
       #:when (not (equal? id-phase phase))
       (loop this-syntax id-phase)]

      [(quote-syntax _ ...) (stream)]

      [(define-values (id ...) body)
       (stream-append (attribute id) (recur (attribute body)))]

      [(define-syntaxes (id ...) body)
       (stream-append (attribute id) (loop (attribute body) (add1 phase)))]

      [((~or let-values letrec-values) ([(id ...) rhs] ...) body ...)
       (define inner-exprs (append (attribute rhs) (attribute body)))
       (define ids (append* (attribute id)))
       (apply stream-append ids (map recur inner-exprs))]

      [(#%plain-lambda formals body ...)
       (apply stream-append
              (syntax-search (attribute formals) [:id])
              (map recur (attribute body)))]

      [(case-lambda [formals body ...] ...)
       (apply stream-append
              (syntax-search #'(formals ...) [:id])
              (map recur (append* (attribute body))))])))


(define (fully-expanded-syntax-id-table stx)
  ;; stx is expected to already have phase labels via syntax-label-id-phases
  (define table (make-expanded-id-table))
  (for ([id (in-stream (binding-site-identifiers stx))])
    (define id-phase (syntax-property id 'phase))
    (expanded-id-table-set! table (expanded-identifier id id-phase) '()))
  table)


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
      (check-equal? (length entries) 3)))
  
  (test-case "fully-expanded-syntax-id-table"
    
    (test-case "creates table with binding sites from expanded module"
      (define stx #'(module test racket/base (define x 1) (define y 2)))
      (define expanded-stx (expand stx))
      (define labeled-stx (syntax-label-id-phases expanded-stx))
      (define table (fully-expanded-syntax-id-table labeled-stx))
      (check-pred expanded-id-table? table)
      ;; The table should contain bindings
      (define entries (for/list ([e (in-expanded-id-table table)]) e))
      (check > (length entries) 0))
    
    (test-case "creates table with phase 0 bindings"
      (define stx #'(module test racket/base (define a 1)))
      (define expanded-stx (expand stx))
      (define labeled-stx (syntax-label-id-phases expanded-stx))
      (define table (fully-expanded-syntax-id-table labeled-stx))
      (define phase0-entries (for/list ([e (in-expanded-id-table-phase table 0)]) e))
      (check > (length phase0-entries) 0))
    
    (test-case "creates table with phase 1 bindings"
      (define stx #'(module test racket/base
                      (require (for-syntax racket/base))
                      (begin-for-syntax (define a 1))))
      (define expanded-stx (expand stx))
      (define labeled-stx (syntax-label-id-phases expanded-stx))
      (define table (fully-expanded-syntax-id-table labeled-stx))
      (define phase1-entries (for/list ([e (in-expanded-id-table-phase table 1)]) e))
      (check > (length phase1-entries) 0))))
