#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [refactor! (-> (sequence/c refactoring-result?) void?)]
  [refactor (->* (string?) (#:suite refactoring-suite?) (listof refactoring-result?))]
  [refactor-file (->* (path-string?) (#:suite refactoring-suite?) (listof refactoring-result?))]))


(require fancy-app
         racket/path
         racket/port
         racket/sequence
         racket/syntax-srcloc
         rebellion/base/immutable-string
         rebellion/base/option
         rebellion/base/symbol
         rebellion/collection/entry
         rebellion/collection/hash
         rebellion/collection/list
         rebellion/streaming/transducer
         resyntax/private/code-snippet
         resyntax/default-recommendations
         resyntax/private/comment-reader
         resyntax/private/refactoring-result
         resyntax/refactoring-rule
         (submod resyntax/refactoring-rule private)
         resyntax/refactoring-suite
         resyntax/private/source
         resyntax/private/string-replacement
         resyntax/private/syntax-replacement)


;@----------------------------------------------------------------------------------------------------


(struct exn:fail:refactoring exn:fail (rule syntax cause)
  #:transparent
  #:property prop:exn:srclocs
  (λ (this) (list (syntax-srcloc (exn:fail:refactoring-syntax this)))))


(define (refactoring-rules-refactor rules syntax source comments)

  (define (refactor rule)
    (with-handlers
        ([exn:fail?
          (λ (e)
            (define message
              (format "~a: refactoring attempt failed\n  syntax: ~e\n  cause: ~e"
                      (object-name rule) syntax e))
            (raise (exn:fail:refactoring message (current-continuation-marks) rule syntax e)))])
      (option-map
       (option-filter
        (option-filter
         (refactoring-rule-refactor rule syntax)
         syntax-replacement-preserves-free-identifiers?)
        (syntax-replacement-preserves-comments? _ comments))
       (refactoring-result
        #:source source
        #:rule-name (object-name rule)
        #:message (refactoring-rule-description rule)
        #:replacement _))))
  
  (falsey->option
   (for*/first ([rule (in-list rules)]
                [result (in-option (refactor rule))])
     result)))


(define (refactor code-string #:suite [suite default-recommendations])
  (define rule-list (refactoring-suite-rules suite))
  (define source (string-source code-string))
  (define comments (with-input-from-string code-string read-comment-locations))
  (parameterize ([current-namespace (make-base-namespace)])
    (define analysis (source-analyze source))
    (parameterize ([current-scopes-by-location
                    (source-code-analysis-scopes-by-location analysis)])
      (transduce
       (source-code-analysis-visited-forms analysis)
       (append-mapping (λ (stx) (in-option (refactoring-rules-refactor rule-list stx source comments))))
       #:into into-list))))


(define (refactor-file path-string #:suite [suite default-recommendations])
  (define path (simple-form-path path-string))
  (printf "resyntax: analyzing ~a\n" path)
  (define rule-list (refactoring-suite-rules suite))
  (define source (file-source path))

  (define (skip e)
    (printf "resyntax: skipping ~a due to syntax error: ~e\n" path (exn-message e))
    empty-list)
  
  (with-handlers ([exn:fail:syntax? skip]
                  [exn:fail:filesystem:missing-module? skip])
    (parameterize ([current-namespace (make-base-namespace)])
      (define analysis (source-analyze source))
      (define comments (with-input-from-file path read-comment-locations))
      (transduce
       (source-code-analysis-visited-forms analysis)
       (append-mapping
        (λ (stx) (in-option (refactoring-rules-refactor rule-list stx source comments))))
       #:into into-list))))


(define (refactor! results)
  (define results-by-path
    (transduce results
               (bisecting
                (λ (result) (file-source-path (refactoring-result-source result)))
                refactoring-result-string-replacement)
               (grouping union-into-string-replacement)
               #:into into-hash))
  (for ([(path replacement) (in-hash results-by-path)])
    (file-apply-string-replacement! path replacement)))
