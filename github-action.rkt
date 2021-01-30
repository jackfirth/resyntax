#lang racket/base


(require fancy-app
         json
         racket/cmdline
         racket/file
         racket/list
         racket/symbol
         rebellion/collection/list
         rebellion/streaming/reducer
         rebellion/streaming/transducer
         resyntax
         resyntax/github-annotation
         resyntax/string-replacement
         resyntax/syntax-replacement)


;@----------------------------------------------------------------------------------------------------


(define (refactoring-result->github-annnotation result)
  (define replacement (syntax-replacement-render (refactoring-result-replacement result)))
  (define original-file-string
    (string->immutable-string (file->string (refactoring-result-path result) #:mode 'text)))
  (define content (string-replacement-render replacement original-file-string))
  (define start-line
    (syntax-line (syntax-replacement-original-syntax (refactoring-result-replacement result))))
  (define end-line (+ start-line (string-line-count content)))
  (define single-line? (equal? start-line end-line))
  (define start-column
    (and single-line?
         (syntax-column
          (syntax-replacement-original-syntax (refactoring-result-replacement result)))))
  (define end-column (and single-line? (+ start-column (string-replacement-span replacement))))
  (github-annotation
   #:path (refactoring-result-path result)
   #:message (refactoring-result-message result)
   #:annotation-level notice
   #:start-line start-line
   #:end-line end-line
   #:start-column start-column
   #:end-column end-column
   #:title (symbol->immutable-string (refactoring-result-rule-name result))
   #:raw-details content))


(define (string-line-count str)
  (add1 (transduce str (filtering (equal? _ #\newline)) #:into into-count)))


(module+ main
  (define package-to-analyze
    (command-line
     #:program "resyntax-github-action"
     #:args (package-name)
     package-name))
  (define results (refactor-package package-to-analyze))
  (define annotations-json
    (transduce results
               (mapping refactoring-result->github-annnotation)
               (mapping github-annotation->jsexpr)
               #:into (reducer-map into-list #:range jsexpr->string)))
  (printf "::set-output name=conclusion::~a\n" (if (empty? results) "success" "failure"))
  (printf "::set-output name=summary::~a\n"
          (if (empty? results) "\"No issues found.\"" "\"Resyntax found potential improvements.\""))
  (printf "::set-output name=annotations::~v\n" annotations-json))
