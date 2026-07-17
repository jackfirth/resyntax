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
  [source-expand (-> source? syntax?)]
  [source-can-expand? (-> source? boolean?)]
  [source-text-of (-> source? syntax? immutable-string?)]
  [source-comment-locations (-> source? immutable-range-set?)]
  [file-source? (-> any/c boolean?)]
  [file-source (-> path-string? file-source?)]
  [string-source? (-> any/c boolean?)]
  [string-source (-> string? string-source?)]
  [modified-source? (-> any/c boolean?)]
  [modified-source (-> unmodified-source? string? modified-source?)]
  [with-input-from-source (-> source? (-> any) any)]))


(require guard
         racket/match
         racket/path
         racket/port
         rebellion/base/immutable-string
         resyntax/private/syntax-neighbors
         syntax/modread
         rebellion/base/comparator
         rebellion/base/range
         rebellion/collection/range-set
         rebellion/collection/vector/builder
         rebellion/streaming/transducer
         syntax-color/lexer-contract
         syntax-color/module-lexer)


(module+ test
  (require (submod "..")
           racket/file
           rackunit))


;@----------------------------------------------------------------------------------------------------


;; All source text flows through this port wrapper, which decodes it as UTF-8 and converts
;; newline sequences like \r\n into single \n characters.
(define (reencoded-source-input-port in)
  (reencode-input-port in "UTF-8" #false #false (object-name in) #true))


(define (string-normalize-newlines str)
  (port->string (reencoded-source-input-port (open-input-string str))))


(struct source () #:transparent)


(struct unmodified-source source () #:transparent)


(struct file-source unmodified-source (path)
  #:transparent
  #:guard (λ (path _) (simple-form-path path)))


(struct string-source unmodified-source (contents)
  #:transparent
  #:guard (λ (contents _) (string->immutable-string (string-normalize-newlines contents))))


(struct modified-source source (original contents)
  #:transparent
  #:guard (λ (original contents _)
            (values original (string->immutable-string (string-normalize-newlines contents)))))


(define (source-name src)
  (match src
    [(or (file-source path) (modified-source (file-source path) _)) path]
    [(or (string-source _) (modified-source (string-source _) _)) 'string]))


(define (with-input-from-source code proc)

  (define (call-proc-with-reencoded-input in)
    (parameterize ([current-input-port (reencoded-source-input-port in)])
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


;; Parameterizes the current directory to the source's parent directory (for file-based sources)
;; while calling proc, so that reading and expanding sources can resolve relative module paths
;; regardless of what the current directory was beforehand.
(define (call-with-source-directory code proc)
  (define dir (source-directory code))
  (if dir
      (parameterize ([current-directory dir])
        (proc))
      (proc)))


(define (source-read-syntax code)
  (define (read-from-input)
    (port-count-lines! (current-input-port))
    (with-module-reading-parameterization read-syntax))
  (syntax-label-original-paths
   (call-with-source-directory code (λ () (with-input-from-source code read-from-input)))))


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
  (define detected-lang
    (call-with-source-directory code (λ () (with-input-from-source code read-lang-from-input))))
  (match detected-lang
    [(list 'submod path 'reader) path]
    [_ #false]))


(module+ test
  (test-case "string and modified sources normalize newlines upon construction"
    (define cr (string #\return))
    (define lf (string #\newline))
    (define nel (string (integer->char #x85)))
    (define ls (string (integer->char #x2028)))
    (define normalized (string-source (string-append "a" lf "b" lf "c")))
    (check-equal? (string-source (string-append "a" cr lf "b" cr lf "c")) normalized)
    (check-equal? (string-source (string-append "a" cr "b" cr "c")) normalized)
    (check-equal? (string-source (string-append "a" nel "b" cr nel "c")) normalized)
    (check-equal? (string-source (string-append "a" ls "b" ls "c")) normalized)
    (define base (string-source "base"))
    (check-equal? (modified-source base (string-append "a" cr lf "b"))
                  (modified-source base (string-append "a" lf "b"))))

  (test-case "source-read-language"
    (check-equal? (source-read-language (string-source "#lang racket")) 'racket)
    (check-equal? (source-read-language (string-source "#lang at-exp racket")) 'at-exp)
    (check-equal? (source-read-language (string-source "#lang scribble/manual")) 'scribble/manual)
    (check-equal? (source-read-language (string-source "#lang info")) 'info)
    (check-equal? (source-read-language (string-source "#lang setup/infotab")) 'setup/infotab)
    (check-equal? (source-read-language (string-source "(void)")) #false))
  
  (test-case "source-can-expand?"
    ;; Valid racket code should expand successfully
    (check-true (source-can-expand? (string-source "#lang racket/base\n(define x 42)")))
    (check-true (source-can-expand? (string-source "#lang racket\n(or 1 2 3)")))
    
    ;; Invalid racket code should not expand
    (check-false (source-can-expand? (string-source "#lang racket/base\n(if)")))
    (check-false (source-can-expand? (string-source "#lang racket/base\n(define)")))
    
    ;; Modified sources should also be testable
    (define orig (string-source "#lang racket/base\n(define foo 42)"))
    (define valid-mod (modified-source orig "#lang racket/base\n(define foo 43)"))
    (define invalid-mod (modified-source orig "#lang racket/base\n(if)"))
    (check-true (source-can-expand? valid-mod))
    (check-false (source-can-expand? invalid-mod)))

  (test-case "source-expand parameterizes the current directory for file sources"
    ;; Expanding a file source with a relative import should succeed no matter what the current
    ;; directory is.
    (define dir (make-temporary-directory))
    (display-to-file "#lang racket/base\n(provide x)\n(define x 42)\n" (build-path dir "helper.rkt"))
    (define program-path (build-path dir "program.rkt"))
    (display-to-file "#lang racket/base\n(require \"helper.rkt\")\nx\n" program-path)
    (define program-file-source (file-source program-path))
    (define program-modified-source
      (modified-source program-file-source "#lang racket/base\n(require \"helper.rkt\")\n(void x)\n"))
    (parameterize ([current-directory (find-system-path 'temp-dir)])
      (check-true (source-can-expand? program-file-source))
      (check-true (source-can-expand? program-modified-source)))
    (delete-directory/files dir)))


(define (source-expand code)
  (call-with-source-directory code (λ () (expand (source-read-syntax code)))))


(define (source-can-expand? code)
  (with-handlers ([exn:fail? (λ (_) #false)])
    (parameterize ([current-namespace (make-base-namespace)])
      (source-expand code))
    #true))


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


(define (source-comment-locations src)
  (transduce (source-tokens src)
             (filtering lexical-token-comment?)
             (mapping lexical-token-location)
             #:into (into-range-set natural<=>)))


(struct lexical-token (text start end type delimiter-kind attributes) #:transparent)


(define (source-tokens src)
  (with-input-from-source src
    (λ ()
      (port-count-lines! (current-input-port))
      (define tokens (make-vector-builder))
      (let loop ([offset 0] [mode #false])
        (define-values (text raw-attributes delimiter-kind start end _ new-mode)
          (module-lexer* (current-input-port) offset mode))
        (unless (eof-object? text)
          (define type
            (if (symbol? raw-attributes)
                raw-attributes
                (hash-ref raw-attributes 'type)))
          (define attributes
            (if (symbol? raw-attributes)
                (hasheq)
                (hash-remove raw-attributes 'type)))
          (vector-builder-add tokens (lexical-token text (sub1 start) (sub1 end) type delimiter-kind attributes))
          (loop (sub1 end) (if (dont-stop? new-mode) (dont-stop-val new-mode) new-mode))))
      (build-vector tokens))))


(define (lexical-token-comment? token)
  (define type (lexical-token-type token))
  (or (equal? type 'comment)
      (equal? type 'sexp-comment)
      (hash-ref (lexical-token-attributes token) 'comment? #false)))


(define (lexical-token-location token)
  (closed-open-range (lexical-token-start token) (lexical-token-end token) #:comparator natural<=>))
