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
  [source-code-analysis-visited-forms (-> source-code-analysis? (listof syntax?))]))


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


(define-record-type source-code-analysis (code visited-forms))
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


(define (source-analyze code)
  (parameterize ([current-directory (or (source-directory code) (current-directory))])
    (define stx (source-read-syntax code))
    (define current-expand-observe (dynamic-require ''#%expobs 'current-expand-observe))
    (define visits-by-location (make-hash))
    (define/guard (observe-event! sig val)
      (guard (and (equal? sig 'visit) (syntax? val) (syntax-original? val)) else
        (void))
      (define loc (syntax-source-location val))
      (guard (hash-has-key? visits-by-location loc) then
        (void))
      (hash-set! visits-by-location loc val))
    (parameterize ([current-expand-observe observe-event!])
      (expand stx))
    (define visited
      (transduce (in-hash-pairs visits-by-location)
                 (sorting syntax-source-location<=> #:key car)
                 #:into (reducer-map into-list #:domain cdr)))
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
  (guard (syntax-original? stx) else
    #false)
  (guard-match (file-source path) code else
    #false)
  (equal? path (syntax-source stx)))

