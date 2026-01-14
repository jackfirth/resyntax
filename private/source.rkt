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
  [source-syntax-paths (->* (source?) (range-set?) sorted-set?)]
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
         resyntax/private/linemap
         resyntax/private/syntax-neighbors
         resyntax/private/syntax-path
         syntax/modread
         rebellion/base/comparator
         rebellion/base/range
         rebellion/collection/range-set
         rebellion/collection/sorted-set
         rebellion/collection/vector/builder
         rebellion/streaming/transducer
         syntax-color/lexer-contract
         syntax-color/module-lexer)


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
  
  (test-case "source-syntax-paths"
    ;; Test with all lines (unbounded range)
    (define test-source (string-source "#lang racket/base\n(define x 42)\n(define y 99)"))
    (define all-paths (source-syntax-paths test-source))
    (check-true (sorted-set? all-paths))
    ;; Should have multiple paths since there are multiple forms
    (check-true (> (sorted-set-size all-paths) 0))
    
    ;; Test with specific line range - just line 2 which has (define x 42)
    (define line2-range (range-set (closed-range 2 2 #:comparator natural<=>)))
    (define line2-paths (source-syntax-paths test-source line2-range))
    (check-true (sorted-set? line2-paths))
    ;; Should have fewer paths than all-paths
    (check-true (< (sorted-set-size line2-paths) (sorted-set-size all-paths)))
    
    ;; Test with a line range that includes multiple forms
    (define lines23-range (range-set (closed-range 2 3 #:comparator natural<=>)))
    (define lines23-paths (source-syntax-paths test-source lines23-range))
    (check-true (>= (sorted-set-size lines23-paths) (sorted-set-size line2-paths)))
    
    ;; Test with no overlapping lines (e.g., line 100)
    (define no-overlap-range (range-set (closed-range 100 100 #:comparator natural<=>)))
    (define no-overlap-paths (source-syntax-paths test-source no-overlap-range))
    (check-equal? (sorted-set-size no-overlap-paths) 0)))


(define (source-expand code)
  (expand (source-read-syntax code)))


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


(define (source-syntax-paths src [lines (range-set (unbounded-range #:comparator natural<=>))])
  (define program-stx (source-read-syntax src))
  (define linemap (string-linemap (source->string src)))
  (sorted-set->immutable-sorted-set
   (transduce (in-syntax-paths program-stx)
              (filtering
               (λ (path)
                 (define stx (syntax-ref program-stx path))
                 ;; Only include paths with source location information
                 (and (syntax-position stx)
                      (syntax-span stx)
                      (let ([stx-lines (syntax-line-range stx #:linemap linemap)])
                        (range-set-overlaps? lines stx-lines)))))
              #:into (into-sorted-set syntax-path<=>))))


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
