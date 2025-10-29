#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [source? (-> any/c boolean?)]
  [unmodified-source? (-> any/c boolean?)]
  [source->string (-> source? immutable-string?)]
  [source-name (-> source? (or/c path? symbol?))]
  [source-path (-> source? (or/c path? #false))]
  [source-directory (-> source? (or/c path? #false))]
  [source-original (-> source? unmodified-source?)]
  [source-read-syntax (-> source? syntax?)]
  [source-read-language (-> source? (or/c module-path? #false))]
  [source-text-of (-> source? syntax? immutable-string?)]
  [file-source? (-> any/c boolean?)]
  [file-source (-> path-string? file-source?)]
  [file-source-path (-> file-source? path?)]
  [string-source? (-> any/c boolean?)]
  [string-source (-> string? string-source?)]
  [string-source-contents (-> string-source? immutable-string?)]
  [modified-source? (-> any/c boolean?)]
  [modified-source (-> unmodified-source? string? modified-source?)]
  [modified-source-contents (-> modified-source? immutable-string?)]
  [modified-source-original (-> modified-source? unmodified-source?)]
  [with-input-from-source (-> source? (-> any) any)]))


(require guard
         racket/match
         racket/path
         racket/port
         rebellion/base/immutable-string
         resyntax/private/syntax-neighbors
         syntax/modread
         syntax/parse)


(module+ test
  (require (submod "..")
           rackunit))


;@----------------------------------------------------------------------------------------------------


(struct source () #:transparent)


(struct unmodified-source source () #:transparent)


(struct file-source unmodified-source (path)
  #:transparent
  #:guard (λ (path _) (simple-form-path path)))


(struct string-source unmodified-source (contents)
  #:transparent
  #:guard (λ (contents _) (string->immutable-string contents)))


(struct modified-source source (original contents)
  #:transparent
  #:guard (λ (original contents _) (values original (string->immutable-string contents))))


(define (source-name src)
  (match src
    [(or (file-source path) (modified-source (file-source path) _)) path]
    [(or (string-source _) (modified-source (string-source _) _)) 'string]))


(define (with-input-from-source code proc)

  (define (call-proc-with-reencoded-input in)
    (define reencoded-in (reencode-input-port in "UTF-8" #false #false (object-name in) #true))
    (parameterize ([current-input-port reencoded-in])
      (proc)))

  (match code
    [(file-source path) (call-with-input-file path call-proc-with-reencoded-input)]
    [(string-source contents) (call-proc-with-reencoded-input (open-input-string contents))]
    [(modified-source (file-source path) contents)
     (call-proc-with-reencoded-input (open-input-string contents path))]
    [(modified-source (? string-source?) contents)
     (call-proc-with-reencoded-input (open-input-string contents))]))


(define (source->string code)
  (string->immutable-string (with-input-from-source code port->string)))


(define (source-read-syntax code)
  (define (read-from-input)
    (port-count-lines! (current-input-port))
    (with-module-reading-parameterization read-syntax))
  (syntax-label-original-paths (with-input-from-source code read-from-input)))


(define (source-read-language code)
  (define (read-lang-from-input)
    (port-count-lines! (current-input-port))
    (with-module-reading-parameterization
      (λ ()
        (call/ec
         (λ (escape)
           (parameterize ([current-reader-guard escape])
             (read-syntax))
           #false)))))
  (define detected-lang (with-input-from-source code read-lang-from-input))
  (match detected-lang
    [(list 'submod path 'reader) path]
    [_ #false]))


(module+ test
  (test-case "source-read-language"
    (check-equal? (source-read-language (string-source "#lang racket")) 'racket)
    (check-equal? (source-read-language (string-source "#lang at-exp racket")) 'at-exp)
    (check-equal? (source-read-language (string-source "#lang scribble/manual")) 'scribble/manual)
    (check-equal? (source-read-language (string-source "#lang info")) 'info)
    (check-equal? (source-read-language (string-source "#lang setup/infotab")) 'setup/infotab)
    (check-equal? (source-read-language (string-source "(void)")) #false)))


(define/guard (source-path code)
  (guard-match (or (file-source path) (modified-source (file-source path) _)) code #:else #false)
  path)


(define/guard (source-directory code)
  (define path (source-path code))
  (and path (path-only path)))


(define (source-original code)
  (if (unmodified-source? code)
      code
      (modified-source-original code)))


(define (source-text-of code stx)
  (unless (and (syntax-position stx) (syntax-span stx))
    (raise-arguments-error 'source-text-of "syntax object does not have source location information"
                           "syntax" stx))
  (define start (sub1 (syntax-position stx)))
  (define end (+ start (syntax-span stx)))
  (string->immutable-string (substring (source->string code) start end)))
