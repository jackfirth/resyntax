#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [refactor! (-> (sequence/c refactoring-result?) void?)]
  [refactor (->* (string?) (#:suite refactoring-suite?) (listof refactoring-result?))]
  [refactor-file (->* (file-portion?) (#:suite refactoring-suite?) (listof refactoring-result?))]))


(require fancy-app
         guard
         racket/match
         racket/port
         racket/sequence
         racket/syntax-srcloc
         rebellion/base/comparator
         rebellion/base/option
         rebellion/base/range
         rebellion/collection/entry
         rebellion/collection/hash
         rebellion/collection/list
         rebellion/collection/range-set
         rebellion/streaming/transducer
         resyntax/base
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
         (submod resyntax/base private))


(module+ test
  (require racket/list
           rackunit
           (submod "..")))


;@----------------------------------------------------------------------------------------------------


(define (refactoring-rules-refactor rules syntax #:comments comments #:analysis analysis)

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
          (refactoring-rule-refactor rule syntax (source-code-analysis-code analysis))
          #:else absent)
        (guard (syntax-replacement-introduces-incorrect-bindings? replacement) #:else
          (log-resyntax-warning
           (string-append
            "~a: suggestion discarded because it introduces identifiers with incorrect bindings\n"
            "  incorrect identifiers: ~a")
           (object-name rule)
           (syntax-replacement-introduced-incorrect-identifiers replacement))
          absent)
        (guard (syntax-replacement-preserves-comments? replacement comments) #:else
          (log-resyntax-warning
           (string-append "~a: suggestion discarded because it does not preserve all comments\n"
                          "  dropped comment locations: ~v\n"
                          "  original syntax:\n"
                          "   ~v\n"
                          "  replacement syntax:\n"
                          "   ~v\n")
           (object-name rule)
           (syntax-replacement-dropped-comment-locations replacement comments)
           (syntax-replacement-original-syntax replacement)
           (syntax-replacement-new-syntax replacement))
          absent)
        (present
         (refactoring-result
          #:rule-name (object-name rule)
          #:message (refactoring-rule-description rule)
          #:syntax-replacement replacement)))))
  
  (falsey->option
   (for*/first ([rule (in-list rules)]
                [result (in-option (refactor rule))])
     result)))


(define (refactor code-string
                  #:suite [suite default-recommendations]
                  #:lines [lines (range-set (unbounded-range #:comparator natural<=>))])
  (define rule-list (refactoring-suite-rules suite))
  (define source (string-source code-string))
  (define comments (with-input-from-source source read-comment-locations))
  (parameterize ([current-namespace (make-base-namespace)])
    (define analysis (source-analyze source #:lines lines))
    (refactor-visited-forms #:analysis analysis #:suite suite #:comments comments #:lines lines)))


(define (refactor-file portion #:suite [suite default-recommendations])
  (define path (file-portion-path portion))
  (define lines (file-portion-lines portion))
  (log-resyntax-info "analyzing ~a" path)
  (define source (file-source path))

  (define (skip e)
    (log-resyntax-error
     "skipping ~a\n encountered an error during macro expansion\n  error:\n~a"
     path
     (string-indent (exn-message e) #:amount 3))
    empty-list)
  
  (with-handlers ([exn:fail:syntax? skip]
                  [exn:fail:filesystem:missing-module? skip])
    (parameterize ([current-namespace (make-base-namespace)])
      (define analysis (source-analyze source #:lines lines))
      (define comments (with-input-from-source source read-comment-locations))
      (define full-source (source->string source))
      (for ([comment (in-range-set comments)])
        (log-resyntax-debug "parsed comment: ~a: ~v"
                            comment
                            (substring-by-range full-source comment)))
      (refactor-visited-forms #:analysis analysis #:suite suite #:comments comments #:lines lines))))


(define (refactor-visited-forms #:analysis analysis #:suite suite #:comments comments #:lines lines)
  (define rule-list (refactoring-suite-rules suite))
  (for*/fold ([results '()]
              [modified-positions (range-set #:comparator natural<=>)]
              #:result (reverse results))
             ([stx (in-list (source-code-analysis-visited-forms analysis))]
              #:unless (range-set-intersects? modified-positions (syntax-source-range stx))
              [result
               (in-option
                (refactoring-rules-refactor rule-list stx #:comments comments #:analysis analysis))]
              #:when (check-lines-enclose-refactoring-result lines result))
    (values (cons result results)
            (range-set-add modified-positions (refactoring-result-modified-range result)))))


(define (check-lines-enclose-refactoring-result lines result)
  (define modified-lines (refactoring-result-modified-line-range result))
  (define enclosed? (range-set-encloses? lines modified-lines))
  (unless enclosed?
    (log-resyntax-info
     (string-append "~a: suggestion discarded because it's outside the analyzed line range\n"
                    "  analyzed lines: ~a\n"
                    "  lines modified by result: ~a\n"
                    "  result: ~a")
     (refactoring-result-rule-name result)
     lines
     modified-lines
     result))
  enclosed?)
     

(define (refactor! results)
  (define results-by-path
    (transduce results
               (bisecting
                (λ (result)
                  (file-source-path
                   (syntax-replacement-source (refactoring-result-syntax-replacement result))))
                refactoring-result-string-replacement)
               (grouping union-into-string-replacement)
               #:into into-hash))
  (for ([(path replacement) (in-hash results-by-path)])
    (file-apply-string-replacement! path replacement)))


(define (substring-by-range str rng)
  (define lower-bound (range-lower-bound rng))
  (define start
    (cond
      [(equal? lower-bound unbounded) 0]
      [(equal? (range-bound-type lower-bound) inclusive)
       (range-bound-endpoint lower-bound)]
      [else
       (max 0 (sub1 (range-bound-endpoint lower-bound)))]))
  (define upper-bound (range-upper-bound rng))
  (define end
    (cond
      [(equal? upper-bound unbounded) (string-length str)]
      [(equal? (range-bound-type upper-bound) inclusive)
       (min (string-length str) (+ (range-bound-endpoint upper-bound) 1))]
      [else (range-bound-endpoint upper-bound)]))
  (substring str start end))


(module+ test
  (test-case "refactor"
    (define results (refactor "#lang racket (or 1 (or 2 3))"))
    (check-equal? (length results) 1)
    (check-equal? (refactoring-result-string-replacement (first results))
                  (string-replacement #:start 13
                                      #:end 28
                                      #:contents (list (inserted-string "(or 1 2 3)"))))))
