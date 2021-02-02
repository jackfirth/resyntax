#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [refactor-file (-> path-string? (listof refactoring-result?))]
  [refactor-directory (-> path-string? (listof refactoring-result?))]
  [refactor-package (-> path-string? (listof refactoring-result?))]
  [refactoring-result? predicate/c]
  [refactoring-result
   (-> #:path path-string?
       #:rule-name interned-symbol?
       #:message string?
       #:replacement syntax-replacement?
       refactoring-result?)]
  [refactoring-result-path (-> refactoring-result? path?)]
  [refactoring-result-rule-name (-> refactoring-result? interned-symbol?)]
  [refactoring-result-message (-> refactoring-result? immutable-string?)]
  [refactoring-result-replacement (-> refactoring-result? syntax-replacement?)]
  [refactoring-result-original-code (-> refactoring-result? code-snippet?)]
  [refactoring-result-new-code (-> refactoring-result? code-snippet?)]))


(require fancy-app
         pkg/lib
         racket/file
         racket/path
         racket/sequence
         racket/set
         rebellion/base/immutable-string
         rebellion/base/option
         rebellion/base/symbol
         rebellion/collection/list
         rebellion/private/guarded-block
         rebellion/streaming/transducer
         rebellion/type/record
         resyntax/code-snippet
         resyntax/refactoring-rule
         (submod resyntax/refactoring-rule private)
         #;resyntax/indentation
         resyntax/source-code
         resyntax/string-replacement
         resyntax/syntax-replacement)


;@----------------------------------------------------------------------------------------------------


(define-record-type refactoring-result (path rule-name message replacement)
  #:omit-root-binding)


(define (refactoring-result
         #:path path #:rule-name rule-name #:message message #:replacement replacement)
  (constructor:refactoring-result
   #:path (simple-form-path path)
   #:rule-name rule-name
   #:message (string->immutable-string message)
   #:replacement replacement))


(define (refactoring-result-original-code result)
  (define original (syntax-replacement-original-syntax (refactoring-result-replacement result)))
  (define start (sub1 (syntax-position original)))
  (define end (+ start (syntax-span original)))
  (define start-column (syntax-column original))
  (define raw-text
    (string->immutable-string
     (substring (file->string (refactoring-result-path result)) start end)))
  (code-snippet raw-text start-column (syntax-line original)))


(define (refactoring-result-new-code result)
  (define original (syntax-replacement-original-syntax (refactoring-result-replacement result)))
  (define replacement (syntax-replacement-render (refactoring-result-replacement result)))
  (define file-code (string->immutable-string (file->string (refactoring-result-path result))))
  (define raw-text (string-replacement-render replacement file-code))
  (code-snippet raw-text (syntax-column original) (syntax-line original)))


(define (refactoring-rules-refactor rules syntax)
  (define (refactor rule) (refactoring-rule-refactor rule syntax))
  (for*/list ([rule rules]
              [result (in-option (refactor rule))])
    (define line (syntax-line (syntax-replacement-original-syntax result)))
    (printf "line ~a: ~a\n" line (object-name rule))
    result))


(define (refactoring-rules-refactor* rules syntax path)
  (define (refactor rule)
    (option-map (refactoring-rule-refactor rule syntax)
                (refactoring-result
                 #:path path
                 #:rule-name (object-name rule)
                 #:message (refactoring-rule-description rule)
                 #:replacement _)))
  (falsey->option
   (for*/first ([rule (in-list rules)]
                [result (in-option (refactor rule))])
     result)))


(define (refactor-file path-string #:rules [rules standard-refactoring-rules])
  (define path (simple-form-path path-string))
  (define relpath (find-relative-path (current-directory) path))
  (printf "resyntax: analyzing ~a\n" relpath)
  (define rule-list (sequence->list rules))
  (define code (file-source-code path))

  (define (skip e)
    (printf "resyntax: skipping ~a due to syntax error\n" relpath)
    empty-list)
  
  (with-handlers ([exn:fail:syntax? skip])
    (parameterize ([current-namespace (make-base-namespace)])
      (define analysis (source-code-analyze code))
      (transduce
       (source-code-analysis-visited-forms analysis)
       (append-mapping (λ (stx) (in-option (refactoring-rules-refactor* rule-list stx path))))
       #:into into-list))))


(define (refactor-directory path-string #:rules [rules standard-refactoring-rules])
  (define path (simple-form-path path-string))
  (define rule-list (sequence->list rules))
  (transduce (in-directory path)
             (filtering rkt-path?)
             (append-mapping (refactor-file _ #:rules rule-list))
             #:into into-list))


(define (refactor-package package-name #:rules [rules standard-refactoring-rules])
  (refactor-directory (pkg-directory package-name) #:rules rules))


(define (refactoring-rules-apply rules code)
  (parameterize ([current-namespace (make-base-namespace)])
    (define analysis (source-code-analyze code))
    (transduce (source-code-analysis-visited-forms analysis)
               (append-mapping (refactoring-rules-refactor rules _))
               (mapping syntax-replacement-render)
               (sorting #:key string-replacement-start)
               #:into union-into-string-replacement)))


(define/guard (refactor code #:rules [rules standard-refactoring-rules])
  (define code-string (source-code-read-string code))
  (define replacement (refactoring-rules-apply rules code))
  (string-apply-replacement code-string replacement)
  #;(indent-code
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
