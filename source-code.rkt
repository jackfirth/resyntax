#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [source-code? predicate/c]
  [source-code-directory (-> source-code? path?)]
  [source-code-read-string (-> source-code? immutable-string?)]
  [source-code-read-syntax (-> source-code? syntax?)]
  [source-code-produced-syntax? (-> source-code? syntax? boolean?)]
  [file-source-code? predicate/c]
  [file-source-code (-> path-string? file-source-code?)]
  [file-source-code-path (-> file-source-code? path?)]
  [string-source-code? predicate/c]
  [string-source-code (-> string? string-source-code?)]
  [string-source-code-contents (-> string-source-code? immutable-string?)]
  [source-code-analyze (-> source-code? source-code-analysis?)]
  [source-code-analysis? predicate/c]
  [source-code-analysis-code (-> source-code-analysis? source-code?)]
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

(struct source-code () #:transparent)

(struct file-source-code source-code (path) #:transparent
  #:guard (λ (path _) (simple-form-path path)))

(struct string-source-code source-code (contents) #:transparent
  #:guard (λ (contents _) (string->immutable-string contents)))

(define-record-type source-code-analysis (code visited-forms))
(define-record-type source-location (source line column position span))

(define (source-code-read-string code)
  (match code
    [(file-source-code path) (string->immutable-string (file->string path #:mode 'text))]
    [(string-source-code contents) contents]))

(define (source-code-read-syntax code)
  (define (read-from-input)
    (port-count-lines! (current-input-port))
    (with-module-reading-parameterization read-syntax))
  (match code
    [(file-source-code path) (with-input-from-file path #:mode 'text read-from-input)]
    [(string-source-code contents) (with-input-from-string contents read-from-input)]))

(define/guard (source-code-directory code)
  (guard-match (file-source-code path) code else
    #false)
  (path-only path))

(define (source-code-analyze code)
  (parameterize ([current-directory (or (source-code-directory code) (current-directory))])
    (define stx (source-code-read-syntax code))
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

(define (source-code-analyze! code)
  (define analysis
    (parameterize ([current-namespace (make-base-namespace)])
      (source-code-analyze code)))
  (transduce (source-code-analysis-visited-forms analysis)
             (mapping syntax->datum)
             #:into (into-for-each (λ (v) (pretty-display v) (newline)))))

(define/guard (source-code-produced-syntax? code stx)
  (guard (syntax-original? stx) else
    #false)
  (guard-match (file-source-code path) code else
    #false)
  (equal? path (syntax-source stx)))

