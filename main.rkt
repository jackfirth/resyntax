#lang racket/base


(require fancy-app
         racket/file
         racket/path
         rebellion/base/option
         rebellion/collection/list
         rebellion/private/guarded-block
         rebellion/streaming/transducer
         resyntax/refactoring-rule
         (submod resyntax/refactoring-rule private)
         resyntax/source-code
         resyntax/string-replacement
         resyntax/syntax-rendering)


(module+ test
  (require (submod "..")))


;@----------------------------------------------------------------------------------------------------


(define (refactoring-rules-refactor rules syntax)
  (define (refactor rule) (refactoring-rule-refactor rule syntax))
  (for*/list ([rule rules]
              [result (in-option (refactor rule))])
    (define line (syntax-line (syntax-replacement-original-syntax result)))
    (printf "line ~a: ~a:\n" line (object-name rule))
    result))


(define (refactoring-rules-apply rules code)
  (parameterize ([current-namespace (make-base-namespace)])
    (define code-string (source-code-read-string code))
    (define analysis (source-code-analyze code))
    (transduce (source-code-analysis-visited-forms analysis)
               (append-mapping (refactoring-rules-refactor rules _))
               (mapping syntax-replacement-render)
               (sorting #:key string-replacement-start)
               #:into into-list)))


(define (string-replacements-ranges-after replacements)
  (for/fold ([position-skew 0]
             [ranges '()]
             #:result (reverse ranges))
            ([replacement replacements])
    (define position (string-replacement-start replacement))
    (define span (string-replacement-span replacement))
    (define new-span (string-replacement-new-span replacement))
    (define skewed-start (+ position position-skew))
    (define skewed-end-before (+ skewed-start span))
    (define skewed-end-after (+ skewed-start new-span))
    (define skew-delta (- skewed-end-after skewed-end-before))
    (define range (source-range skewed-start skewed-end-after))
    (values (+ position-skew skew-delta)
            (cons range ranges))))


(define (refactor-source-code code rules)
  (define replacements (refactoring-rules-apply rules code))
  (define/guard (loop [replacements replacements])
    (guard-match (list first second remaining ...) replacements else
      replacements)
    (guard (string-replacement-overlaps? first second) then
      (printf "overlapped at position ~a, rejecting\n" (string-replacement-start second))
      (loop (cons first remaining)))
    (cons first (loop (cons second remaining))))
  (loop))


(define (apply-replacements code-string replacements)
  (define descending-replacements
    (transduce replacements
               (sorting #:key string-replacement-start #:descending? #true)
               #:into into-list))
  (define replaced
    (for/fold ([code-string code-string]) ([replacement descending-replacements])
      (string-apply-replacement code-string replacement)))
  (indent-code replaced (string-replacements-ranges-after replacements)))


(define/guard (refactor code #:rules [rules standard-refactoring-rules])
  (define code-string (source-code-read-string code))
  (define replacements (refactor-source-code code rules))
  (apply-replacements code-string replacements))


(define (refactor-file path #:rules [rules standard-refactoring-rules])
  (refactor-source-code (file-source-code path) rules))


(define (refactor-file! path #:rules [rules standard-refactoring-rules] #:passes [passes 1])
  (for ([n (in-range 1 (add1 passes))])
    (printf "pass ~a\n" n)
    (define replacement-code (refactor (file-source-code path) #:rules rules))
    (display-to-file replacement-code path #:mode 'text #:exists 'replace)))


(define (rkt-path? path) (path-has-extension? path #".rkt"))


(define (refactor-directory! path #:rules [rules standard-refactoring-rules])
  (for ([file (in-directory path)] #:when (rkt-path? file))
    (refactor-file! file #:rules rules)))


(module+ main
  (refactor-file! "/Users/jackfirth/Documents/GitHub/scribble/scribble-lib/scribble/search.rkt"
                  #:passes 10))
