#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [source? predicate/c]
  [source->string (-> source? immutable-string?)]
  [source-directory (-> source? (or/c path? #false))]
  [source-read-syntax (-> source? syntax?)]
  [source-produced-syntax? (-> source? syntax? boolean?)]
  [source-analyze (->* (source?) (#:lines range-set?) source-code-analysis?)]
  [file-source? predicate/c]
  [file-source (-> path-string? file-source?)]
  [file-source-path (-> file-source? path?)]
  [string-source? predicate/c]
  [string-source (-> string? string-source?)]
  [string-source-contents (-> string-source? immutable-string?)]
  [source-code-analysis? predicate/c]
  [source-code-analysis-code (-> source-code-analysis? source?)]
  [source-code-analysis-visited-forms (-> source-code-analysis? (listof syntax?))]
  [syntax-source-location (-> syntax? source-location?)]
  [with-input-from-source (-> source? (-> any) any)]))


(require guard
         racket/file
         racket/hash
         racket/match
         racket/path
         racket/port
         rebellion/base/comparator
         rebellion/base/immutable-string
         rebellion/base/range
         rebellion/collection/list
         rebellion/collection/range-set
         rebellion/collection/vector/builder
         rebellion/streaming/reducer
         rebellion/streaming/transducer
         rebellion/type/record
         resyntax/private/linemap
         syntax/modread
         syntax/parse)


;@----------------------------------------------------------------------------------------------------


(struct source () #:transparent)


(struct file-source source (path) #:transparent
  #:guard (λ (path _) (simple-form-path path)))


(struct string-source source (contents) #:transparent
  #:guard (λ (contents _) (string->immutable-string contents)))


(define-record-type source-code-analysis (code visited-forms))
(define-record-type source-location (source line column position span))


(define (with-input-from-source code proc)

  (define (call-proc-with-reencoded-input in)
    (define reencoded-in (reencode-input-port in "UTF-8" #false #false (object-name in) #true))
    (parameterize ([current-input-port reencoded-in])
      (proc)))

  (match code
    [(file-source path) (call-with-input-file path call-proc-with-reencoded-input)]
    [(string-source contents) (call-proc-with-reencoded-input (open-input-string contents))]))


(define (source->string code)
  (string->immutable-string (with-input-from-source code port->string)))


(define (source-read-syntax code)
  (define (read-from-input)
    (port-count-lines! (current-input-port))
    (with-module-reading-parameterization read-syntax))
  (with-input-from-source code read-from-input))


(define/guard (source-directory code)
  (guard-match (file-source path) code #:else #false)
  (path-only path))


(define (source-analyze code #:lines [lines (range-set (unbounded-range #:comparator natural<=>))])
  (parameterize ([current-directory (or (source-directory code) (current-directory))])
    (define code-linemap (string-linemap (source->string code)))
    (define program-stx (source-read-syntax code))
    (define program-source-name (syntax-source program-stx))
    (define current-expand-observe (dynamic-require ''#%expobs 'current-expand-observe))
    (define original-visits (make-vector-builder))
    (define expanded-originals-by-location (make-hash))

    (define (add-all-original-subforms! stx)
      (when (resyntax-should-analyze-syntax? stx)
        (hash-set! expanded-originals-by-location (syntax-source-location stx) stx))
      (syntax-parse stx
        [(subform ...) (for-each add-all-original-subforms! (attribute subform))]
        [(subform ...+ . tail-form)
         (for-each add-all-original-subforms! (attribute subform))
         (add-all-original-subforms! #'tail-form)]
        [_ (void)]))

    (define (resyntax-should-analyze-syntax? stx)
      (and (syntax-original? stx)
           ;; Some macros are able to bend hygiene and syntax properties in such a way that they
           ;; introduce syntax objects into the program that are syntax-original?, but from a
           ;; different file than the one being expanded. So in addition to checking for
           ;; originality, we also check that they come from the same source as the main program
           ;; syntax object. The (open ...) clause of the define-signature macro bends hygiene
           ;; in this way, and is what originally motivated the addition of this check.
           (equal? (syntax-source stx) program-source-name)
           (range-set-overlaps? lines (syntax-line-range stx #:linemap code-linemap))))
    
    (define/match (observe-event! sig val)
      [('visit (? syntax? visited))
       (when (resyntax-should-analyze-syntax? visited)
         (vector-builder-add original-visits visited)
         (add-all-original-subforms! visited))]
      [(_ _) (void)])

    (define expanded
      (parameterize ([current-expand-observe observe-event!])
        (expand program-stx)))
    (add-all-original-subforms! expanded)

    (define (enrich stx)
      (define new-context
        (or (hash-ref expanded-originals-by-location (syntax-source-location stx) #false) stx))
      (syntax-parse stx
        [(subform ...)
         (datum->syntax new-context
                        (map enrich (attribute subform))
                        new-context
                        new-context)]
        [(subform ...+ . tail-form)
         (datum->syntax new-context
                        (append (map enrich (attribute subform)) (enrich #'tail-form))
                        new-context
                        new-context)]
        [_ new-context]))
         
    
    (define visited
      (transduce (build-vector original-visits)
                 (deduplicating #:key syntax-source-location)
                 (mapping enrich)
                 (sorting syntax-source-location<=> #:key syntax-source-location)
                 #:into into-list))

    (source-code-analysis #:code code #:visited-forms visited)))


(define (syntax-source-location stx)
  (source-location
   #:source (syntax-source stx)
   #:line (syntax-line stx)
   #:column (syntax-column stx)
   #:position (syntax-position stx)
   #:span (syntax-span stx)))


(define syntax-source-location<=>
  (comparator-chain (comparator-map real<=> source-location-position)
                    (comparator-map (comparator-reverse real<=>) source-location-span)))


(define/guard (source-produced-syntax? code stx)
  (guard (syntax-original? stx) #:else #false)
  (guard-match (file-source path) code #:else #false)
  (equal? path (syntax-source stx)))
