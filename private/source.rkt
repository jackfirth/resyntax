#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [source? predicate/c]
  [source->string (-> source? immutable-string?)]
  [source-directory (-> source? (or/c path? #false))]
  [source-read-syntax (-> source? syntax?)]
  [source-produced-syntax? (-> source? syntax? boolean?)]
  [source-analyze (-> source? source-code-analysis?)]
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
   (-> source-code-analysis? (-> syntax? (or/c #f syntax?)))]
  [scopes-by-location (-> syntax? (or/c #f syntax?))]
  [current-scopes-by-location (parameter/c (-> syntax? (or/c #f syntax?)))]))


(require racket/file
         racket/match
         racket/path
         racket/port
         racket/pretty
         rebellion/base/comparator
         rebellion/base/immutable-string
         rebellion/collection/list
         rebellion/private/guarded-block
         rebellion/streaming/reducer
         rebellion/streaming/transducer
         rebellion/type/record
         syntax/modread)


;@----------------------------------------------------------------------------------------------------


(struct source () #:transparent)


(struct file-source source (path) #:transparent
  #:guard (λ (path _) (simple-form-path path)))


(struct string-source source (contents) #:transparent
  #:guard (λ (contents _) (string->immutable-string contents)))


(define-record-type source-code-analysis (code visited-forms scopes-by-location))
(define-record-type source-location (source line column position span))


(define (scopes-by-location stx)
  ((current-scopes-by-location) stx))
(define current-scopes-by-location
  (let ([scopes-by-location (λ (stx) #f)])
    (make-parameter scopes-by-location)))


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


(define (source-analyze code)
  (parameterize ([current-directory (or (source-directory code) (current-directory))])
    (define stx (source-read-syntax code))
    (define current-expand-observe (dynamic-require ''#%expobs 'current-expand-observe))
    (define visits-by-location (make-hash))
    (define others-by-location (make-hash))
    (define/guard (add-original-location! hsh stx)
      (guard (and (syntax? stx) (syntax-original? stx)) else (void))
      (define loc (syntax-source-location stx))
      (guard (hash-has-key? hsh loc) then (void))
      (hash-set! hsh loc stx))
    (define/match (observe-event! sig val)
      [('visit val)
       (add-original-location! visits-by-location val)]
      [('letX-renames (list-rest trans-idss _ val-idss _))
       (for* ([trans-ids (in-list trans-idss)] [trans-id (in-list trans-ids)])
         (add-original-location! others-by-location trans-id))
       (for* ([val-ids (in-list val-idss)] [val-id (in-list val-ids)])
         (add-original-location! others-by-location val-id))]
      [(_ _) (void)])
    (parameterize ([current-expand-observe observe-event!])
      (expand stx))
    (define (scopes-by-location stx)
      (define loc (syntax-source-location stx))
      (hash-ref visits-by-location loc (λ () (hash-ref others-by-location loc #f))))
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

