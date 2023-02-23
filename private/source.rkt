#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [source? predicate/c]
  [source->string (-> source? immutable-string?)]
  [source-directory (-> source? (or/c path? #false))]
  [source-read-syntax (-> source? syntax?)]
  [source-produced-syntax? (-> source? syntax? boolean?)]
  [source-analyze (-> source? #:lines range-set? source-code-analysis?)]
  [file-source? predicate/c]
  [file-source (-> path-string? file-source?)]
  [file-source-path (-> file-source? path?)]
  [string-source? predicate/c]
  [string-source (-> string? string-source?)]
  [string-source-contents (-> string-source? immutable-string?)]
  [source-code-analysis? predicate/c]
  [source-code-analysis-code (-> source-code-analysis? source?)]
  [source-code-analysis-visited-forms (-> source-code-analysis? (listof syntax?))]
  [source-code-analysis-scopes-by-location
   (-> source-code-analysis? (hash/c source-location? syntax? #:immutable #true))]
  [syntax-source-location (-> syntax? source-location?)]))


(require racket/file
         racket/hash
         racket/match
         racket/path
         racket/port
         rebellion/base/comparator
         rebellion/base/immutable-string
         rebellion/collection/list
         rebellion/collection/range-set
         rebellion/private/guarded-block
         rebellion/streaming/reducer
         rebellion/streaming/transducer
         rebellion/type/record
         resyntax/private/linemap
         syntax/modread)


;@----------------------------------------------------------------------------------------------------


(struct source () #:transparent)


(struct file-source source (path) #:transparent
  #:guard (λ (path _) (simple-form-path path)))


(struct string-source source (contents) #:transparent
  #:guard (λ (contents _) (string->immutable-string contents)))


;; source-code-analysis has fields:
;;  * code: source, the input of the analysis
;;  * visited-forms: (Listof Syntax), sorted by source location, containing
;;      forms visited by the expander, with scopes put on them by expansion
;;      steps in the surrounding context, but not yet expanded themselves
;;  * scopes-by-location: (ImmHashOf source-location Syntax), containing
;;      syntax objects that have the scopes from their surrounding context
;;    For example, in `(let ([x a]) b)`, the expander expands it to
;;    `(let-values ([(x) a]) b)` and adds letX-renames scopes to `x` and `b`.
;;    The `scopes-by-location` table contains the versions of `x` and `b`
;;    with those scopes.
(define-record-type source-code-analysis (code visited-forms scopes-by-location))
(define-record-type source-location (source line column position span))


(define (source->string code)
  (match code
    [(file-source path) (string->immutable-string (file->string path #:mode 'text))]
    [(string-source contents) contents]))


(define (source-read-syntax code)
  (define (read-from-input)
    (port-count-lines! (current-input-port))
    (with-module-reading-parameterization read-syntax))
  (match code
    [(file-source path) (with-input-from-file path #:mode 'text read-from-input)]
    [(string-source contents) (with-input-from-string contents read-from-input)]))


(define/guard (source-directory code)
  (guard-match (file-source path) code else
    #false)
  (path-only path))


(define (source-analyze code #:lines lines)
  (parameterize ([current-directory (or (source-directory code) (current-directory))])
    (define code-linemap (string-linemap (source->string code)))
    (define stx (source-read-syntax code))
    (define current-expand-observe (dynamic-require ''#%expobs 'current-expand-observe))
    (define visits-by-location (make-hash))
    (define others-by-location (make-hash))
    
    (define (add-original-location! hsh stx)
      (when (and (syntax? stx)
                 (syntax-original? stx)
                 (range-set-intersects? lines (syntax-line-range stx #:linemap code-linemap)))
        (define loc (syntax-source-location stx))
        (unless (hash-has-key? hsh loc)
          (hash-set! hsh loc stx))))
    
    (define/match (observe-event! sig val)
      [('visit val)
       (add-original-location! visits-by-location val)]
      ;; For more information on `letX-renames`, see the `make-let-values-form`
      ;; function in the Racket Expander where it logs `letX-renames` events:
      ;; https://github.com/racket/racket/blob/b4a85f54c20cc246d521a4cc7ea4d8c2b52a7e59/racket/src/expander/expand/expr.rkt#L248
      [('letX-renames (list-rest trans-idss _ val-idss _))
       ;; When the expander adds scopes to `let-syntax`, it uses `trans-idss`.
       (for* ([trans-ids (in-list trans-idss)]
              [trans-id (in-list trans-ids)])
         (add-original-location! others-by-location trans-id))
       ;; When the expander adds scopes to `let-values`, it uses `val-idss`.
       (for* ([val-ids (in-list val-idss)]
              [val-id (in-list val-ids)])
         (add-original-location! others-by-location val-id))]
      [(_ _) (void)])
    
    (parameterize ([current-expand-observe observe-event!])
      (expand stx))
    (define scopes-by-location
      (hash-union (hash) visits-by-location others-by-location
                  #:combine (λ (a b) a)))
    (define visited
      (transduce (in-hash-pairs visits-by-location)
                 (sorting syntax-source-location<=> #:key car)
                 #:into (reducer-map into-list #:domain cdr)))
    (source-code-analysis #:code code
                          #:visited-forms visited
                          #:scopes-by-location scopes-by-location)))


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
  (guard (syntax-original? stx) else
    #false)
  (guard-match (file-source path) code else
    #false)
  (equal? path (syntax-source stx)))

