#lang racket/base


(require racket/contract/base)


(provide
 (struct-out expansion-identifier)
 (contract-out
  [expanded-syntax-identifiers (-> syntax? (treelist/c expansion-identifier?))]))


(require racket/list
         racket/sequence
         racket/stream
         racket/treelist
         resyntax/private/syntax-path
         resyntax/private/syntax-traversal
         syntax/parse)


;@----------------------------------------------------------------------------------------------------


(struct expansion-identifier (syntax path phase enclosing-modules kind)
  #:transparent
  #:guard (struct-guard/c identifier?
                          syntax-path?
                          exact-nonnegative-integer?
                          (listof (and/c symbol? symbol-interned?))
                          (or/c 'binding 'usage)))


(define (in-expanded-syntax-identifiers orig-expanded-stx)
  (define labeled-stx (syntax-label-paths orig-expanded-stx 'expanded-path))
  (let loop ([expanded-stx labeled-stx] [phase 0] [skip? #false])

    (define (recur stx #:phase [phase phase] #:skip-root? [skip? #false])
      (loop stx phase skip?))

    (define (make-expanded-identifier id-stx kind)
      (define path (syntax-property id-stx 'expanded-path))
      (expansion-identifier (syntax-ref orig-expanded-stx path) path phase '() kind))

    (syntax-search expanded-stx
      #:skip-root? skip?
      #:literal-sets ([kernel-literals #:phase phase])

      [id:id (stream (make-expanded-identifier (attribute id) 'usage))]

      [(begin-for-syntax _ ...) (recur this-syntax #:phase (add1 phase) #:skip-root? #true)]

      [((~or module module*) _ ...) (recur this-syntax #:phase 0 #:skip-root? #true)]

      [(quote-syntax _ ...) (stream)]
      
      [(define-values-id:define-values (id ...) body)
       (define exp-ids
         (cons (make-expanded-identifier (attribute define-values-id) 'usage)
               (for/list ([id-stx (in-list (attribute id))])
                 (make-expanded-identifier id-stx 'binding))))
       (stream-append exp-ids (recur (attribute body)))]

      [(define-syntaxes-id:define-syntaxes (id ...) body)
       (define exp-ids
         (cons (make-expanded-identifier (attribute define-syntaxes-id) 'usage)
               (for/list ([id-stx (in-list (attribute id))])
                 (make-expanded-identifier id-stx 'binding))))
       (stream-append exp-ids (recur (attribute body) #:phase (add1 phase)))]

      [((~or let-id:let-values let-id:letrec-values) ([(id ...) rhs] ...) body ...)
       (define exp-let-id (make-expanded-identifier (attribute let-id) 'usage))
       (define exp-ids
         (for*/list ([id-list (in-list (attribute id))]
                     [id-stx (in-list id-list)])
           (make-expanded-identifier id-stx 'binding)))
       (define inner-exprs (append (attribute rhs) (attribute body)))
       (apply stream-append (stream exp-let-id) exp-ids (map recur inner-exprs))]

      [(#%plain-lambda formals body ...)
       (apply stream-append
              (syntax-search (attribute formals)
                [id:id (stream (make-expanded-identifier (attribute id) 'binding))])
              (map recur (attribute body)))]

      [(case-lambda [formals body ...] ...)
       (apply stream-append
              (syntax-search #'(formals ...)
                [id:id (stream (make-expanded-identifier (attribute id) 'binding))])
              (map recur (append* (attribute body))))])))


(define (expanded-syntax-identifiers orig-expanded-stx)
  (sequence->treelist (in-expanded-syntax-identifiers orig-expanded-stx)))
