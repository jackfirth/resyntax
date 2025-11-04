#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [identifier-usage-analyzer expansion-analyzer?]))


(require racket/list
         racket/stream
         rebellion/collection/entry
         rebellion/streaming/transducer
         resyntax/private/analyzer
         resyntax/private/expanded-id-table
         resyntax/private/syntax-path
         resyntax/private/syntax-property-bundle
         resyntax/private/syntax-traversal
         syntax/id-table
         syntax/parse)


(module+ test
  (require (submod "..")
           rackunit
           rebellion/collection/sorted-map
           rebellion/collection/sorted-set))


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


;; Find all identifier usage sites (not binding sites)
(define (usage-site-identifiers expanded-stx)
  (let loop ([expanded-stx expanded-stx] [phase 0])
    (define (recur stx)
      (loop stx phase))
    (syntax-search expanded-stx
      #:literal-sets ([kernel-literals #:phase phase])
      
      ;; Phase mismatch - recurse with correct phase
      [(id:id _ ...)
       #:do [(define id-phase (syntax-property (attribute id) 'phase))]
       #:when (not (equal? id-phase phase))
       (loop this-syntax id-phase)]

      ;; Skip quote-syntax - no identifier usages inside
      [(quote-syntax _ ...) (stream)]
      
      ;; define-values: recurse into RHS only (LHS is bindings)
      [(define-values (_ ...) rhs)
       (recur (attribute rhs))]
      
      ;; define-syntaxes: recurse into RHS at phase+1 (LHS is bindings)
      [(define-syntaxes (_ ...) rhs)
       (loop (attribute rhs) (add1 phase))]
      
      ;; let-values/letrec-values: recurse into RHS and body (binding ids excluded by pattern)
      [((~or let-values letrec-values) ([(_ ...) rhs] ...) body ...)
       (apply stream-append (append (map recur (attribute rhs))
                                    (map recur (attribute body))))]
      
      ;; lambda: formals are bindings, recurse into body only
      [(#%plain-lambda _ body ...)
       (apply stream-append (map recur (attribute body)))]
      
      ;; case-lambda: formals are bindings, recurse into bodies only
      [(case-lambda [_ body ...] ...)
       (apply stream-append (map recur (append* (attribute body))))]
      
      ;; set!: the identifier is used, and recurse into RHS
      [(set! id:id rhs)
       (stream-cons (attribute id) (recur (attribute rhs)))]
      
      ;; #%top: the identifier is used
      [(#%top . id:id)
       (stream (attribute id))]
      
      ;; #%variable-reference with identifier
      [(#%variable-reference id:id)
       (stream (attribute id))]
      
      ;; #%variable-reference with #%top
      [(#%variable-reference (#%top . id:id))
       (stream (attribute id))]
      
      ;; Standalone identifier - this is a usage!
      [id:id
       #:when (identifier? this-syntax)
       (stream (attribute id))])))


(define (fully-expanded-syntax-binding-table stx)
  (define labeled-stx (syntax-label-id-phases (syntax-label-paths stx 'expanded-path)))
  
  ;; Create expanded-id-table to track bound identifiers with empty usage lists
  (define table (make-expanded-id-table))
  
  ;; Initialize all bound identifiers with empty usage lists
  (for ([id (in-stream (binding-site-identifiers labeled-stx))])
    (define id-phase (syntax-property id 'phase))
    (expanded-id-table-set! table (expanded-identifier id id-phase) '()))
  
  ;; For each usage, find its binding and add it to the usage list
  (for ([used-id (in-stream (usage-site-identifiers labeled-stx))])
    (define used-phase (syntax-property used-id 'phase))
    (for ([bound-entry (in-expanded-id-table table)])
      (define bound-expanded-id (entry-key bound-entry))
      (define bound-id (expanded-identifier-syntax bound-expanded-id))
      (define bound-phase (expanded-identifier-phase bound-expanded-id))
      (when (and (equal? bound-phase used-phase)
                 (free-identifier=? bound-id used-id))
        (define current-usages (entry-value bound-entry))
        (expanded-id-table-set! table bound-expanded-id (cons used-id current-usages)))))
  
  table)


(define identifier-usage-analyzer
  (make-expansion-analyzer
   #:name 'identifier-usage-analyzer
   (λ (expanded-stx)
     (define table (fully-expanded-syntax-binding-table expanded-stx))
     (transduce (in-expanded-id-table table)
                (mapping
                 (λ (entry)
                   (define expanded-id (entry-key entry))
                   (define usages (entry-value entry))
                   (define bound-id (expanded-identifier-syntax expanded-id))
                   (define exp-path (syntax-property bound-id 'expanded-path))
                   (syntax-property-entry exp-path 'usage-count (length usages))))
                #:into into-syntax-property-bundle))))
