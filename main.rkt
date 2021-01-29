#lang racket/base


(require fancy-app
         racket/file
         racket/path
         rebellion/base/option
         rebellion/private/guarded-block
         rebellion/streaming/transducer
         resyntax/indentation
         resyntax/refactoring-rule
         (submod resyntax/refactoring-rule private)
         resyntax/source-code
         resyntax/string-replacement
         resyntax/syntax-replacement)


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
               #:into union-into-string-replacement)))


(define/guard (refactor code #:rules [rules standard-refactoring-rules])
  (define code-string (source-code-read-string code))
  (define replacement (refactoring-rules-apply rules code))
  (define replaced (string-apply-replacement code-string replacement))
  (indent-code
   replaced (string-replacement-start replacement) (string-replacement-new-end replacement)))


(define (refactor-file! path #:rules [rules standard-refactoring-rules] #:passes [passes 1])
  (for ([n (in-range 1 (add1 passes))])
    (printf "pass ~a\n" n)
    (define replacement-code (refactor (file-source-code path) #:rules rules))
    (display-to-file replacement-code path #:mode 'text #:exists 'replace)))


(define (rkt-path? path) (path-has-extension? path #".rkt"))


(define (refactor-directory! path #:rules [rules standard-refactoring-rules])
  (for ([file (in-directory path)] #:when (rkt-path? file))
    (refactor-file! file #:rules rules)))


;; Empty test submodule to prevent initialization of the GUI framework via resyntax/indentation
(module test racket/base)


(module+ main
  (refactor-file!
   "/Users/jackfirth/Documents/GitHub/scribble/scribble-lib/scribble/html-render.rkt" #:passes 10))
