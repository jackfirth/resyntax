#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [identifier-usage-analyzer expansion-analyzer?]))


(require racket/list
         racket/stream
         rebellion/collection/entry
         rebellion/streaming/transducer
         resyntax/default-recommendations/analyzers/private/expanded-id-table
         resyntax/private/analyzer
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


;; Extract identifiers from the 'origin syntax property
;; The 'origin property can be either:
;; - A single syntax object
;; - A list of syntax objects and/or pairs
;; - Pairs can contain syntax objects or lists of syntax objects
;; We extract all identifiers from it recursively and label them with the given phase
(define (origin-property-identifiers stx phase)
  (define origin (syntax-property stx 'origin))
  
  (define (extract-ids obj)
    (cond
      [(not obj) (stream)]
      [(identifier? obj) 
       ;; Add the phase property to the identifier so it matches correctly
       (stream (syntax-property obj 'phase phase))]
      [(syntax? obj) (stream)]  ; syntax but not identifier
      [(pair? obj)
       (stream-append (extract-ids (car obj))
                      (extract-ids (cdr obj)))]
      [(list? obj)
       (apply stream-append (map extract-ids obj))]
      [else (stream)]))
  
  (extract-ids origin))


;; Extract identifiers from the 'disappeared-use syntax property
;; The 'disappeared-use property can be either:
;; - A single identifier
;; - A list of identifiers
;; We extract all identifiers and label them with the given phase
(define (disappeared-use-property-identifiers stx phase)
  (define disappeared (syntax-property stx 'disappeared-use))
  
  (define (extract-ids obj)
    (cond
      [(not obj) (stream)]
      [(identifier? obj)
       ;; Add the phase property to the identifier so it matches correctly
       (stream (syntax-property obj 'phase phase))]
      [(list? obj)
       (apply stream-append (map extract-ids obj))]
      [else (stream)]))
  
  (extract-ids disappeared))


;; Find all identifier usage sites (not binding sites)
(define (usage-site-identifiers expanded-stx)
  (let loop ([expanded-stx expanded-stx] [phase 0])
    (define (recur stx)
      (loop stx phase))
    
    ;; Collect identifiers from origin properties of all syntax objects
    (define origin-ids
      (apply stream-append
             (for/list ([stx-node (in-stream (syntax-search-everything expanded-stx))])
               (origin-property-identifiers stx-node phase))))
    
    ;; Collect identifiers from disappeared-use properties of all syntax objects
    (define disappeared-ids
      (apply stream-append
             (for/list ([stx-node (in-stream (syntax-search-everything expanded-stx))])
               (disappeared-use-property-identifiers stx-node phase))))
    
    ;; Collect identifiers from the expanded syntax tree
    (define expanded-ids
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
         (stream (attribute id))]))
    
    (stream-append origin-ids disappeared-ids expanded-ids)))


(define (fully-expanded-syntax-binding-table stx)
  (define labeled-stx (syntax-label-id-phases (syntax-label-paths stx 'expanded-path)))
  
  ;; Create expanded-id-table with all bound identifiers initialized to empty usage lists
  (define table (fully-expanded-syntax-id-table labeled-stx))
  
  ;; For each usage, find its binding within the same phase and add it to the usage list
  (for ([used-id (in-stream (usage-site-identifiers labeled-stx))])
    (define used-phase (syntax-property used-id 'phase))
    (for ([bound-entry (in-expanded-id-table-phase table used-phase)])
      (define bound-expanded-id (entry-key bound-entry))
      (define bound-id (expanded-identifier-syntax bound-expanded-id))
      (when (free-identifier=? bound-id used-id)
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
