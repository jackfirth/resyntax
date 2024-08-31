#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [refactor! (-> (sequence/c refactoring-result?) void?)]
  [refactor (->* (string?) (#:suite refactoring-suite?) (listof refactoring-result?))]
  [refactor-file (->* (file-portion?) (#:suite refactoring-suite?) (listof refactoring-result?))]))


(require fancy-app
         guard
         racket/port
         racket/sequence
         racket/syntax-srcloc
         rebellion/base/comparator
         rebellion/base/option
         rebellion/collection/entry
         rebellion/collection/hash
         rebellion/collection/list
         rebellion/collection/range-set
         rebellion/streaming/transducer
         resyntax/default-recommendations
         resyntax/private/comment-reader
         resyntax/private/file-group
         resyntax/private/logger
         resyntax/private/refactoring-result
         resyntax/private/source
         resyntax/private/string-indent
         resyntax/private/string-replacement
         resyntax/private/syntax-range
         resyntax/private/syntax-replacement
         resyntax/refactoring-rule
         (submod resyntax/refactoring-rule private)
         resyntax/refactoring-suite)


;@----------------------------------------------------------------------------------------------------


(struct exn:fail:refactoring exn:fail (rule syntax cause)
  #:transparent
  #:property prop:exn:srclocs
  (λ (this) (list (syntax-srcloc (exn:fail:refactoring-syntax this)))))


(define (refactoring-rules-refactor rules syntax
                                    #:comments comments
                                    #:analysis analysis)

  (define (refactor rule)
    (with-handlers
        ([exn:fail?
          (λ (e)
            (log-resyntax-error "~a: refactoring attempt failed\n  syntax:\n   ~a\n  cause:\n~a"
                                (object-name rule)
                                syntax
                                (string-indent (exn-message e) #:amount 3))
            absent)])
      (guarded-block
        (guard-match (present replacement)
          (refactoring-rule-refactor rule syntax #:analysis analysis)
          #:else absent)
        (guard (syntax-replacement-preserves-free-identifiers? replacement) #:else
          (log-resyntax-debug
           "suggestion from ~a discarded because it does not preserve all free identifiers"
           (object-name rule))
          absent)
        (guard (syntax-replacement-preserves-comments? replacement comments) #:else
          (log-resyntax-debug
           "suggestion from ~a discarded because it does not preserve all comments"
           (object-name rule))
          absent)
        (present
         (refactoring-result
          #:source (source-code-analysis-code analysis)
          #:rule-name (object-name rule)
          #:message (refactoring-rule-description rule)
          #:replacement replacement)))))
  
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
    (refactor-visited-forms #:analysis analysis #:suite suite #:comments comments)))


(define (refactor-file portion #:suite [suite default-recommendations])
  (define path (file-portion-path portion))
  (define lines (file-portion-lines portion))
  (printf "resyntax: analyzing ~a\n" path)
  (define source (file-source path))

  (define (skip e)
    (printf "resyntax: skipping ~a due to syntax error: ~e\n" path (exn-message e))
    empty-list)
  
  (with-handlers ([exn:fail:syntax? skip]
                  [exn:fail:filesystem:missing-module? skip])
    (parameterize ([current-namespace (make-base-namespace)])
      (define analysis (source-analyze source #:lines lines))
      (define comments (with-input-from-file path read-comment-locations))
      (refactor-visited-forms #:analysis analysis #:suite suite #:comments comments))))


(define (refactor-visited-forms #:analysis analysis #:suite suite #:comments comments)
  (define rule-list (refactoring-suite-rules suite))
  (for*/fold ([results '()]
              [modified-positions (range-set #:comparator natural<=>)]
              #:result (reverse results))
             ([stx (in-list (source-code-analysis-visited-forms analysis))]
              #:unless (range-set-intersects? modified-positions (syntax-source-range stx))
              [result
               (in-option
                (refactoring-rules-refactor rule-list stx #:comments comments #:analysis analysis))])
    (values (cons result results)
            (range-set-add modified-positions (refactoring-result-modified-range result)))))


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
