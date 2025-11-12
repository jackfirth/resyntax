#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [resyntax-analysis? (-> any/c boolean?)]
  [resyntax-analysis-all-results
   (-> resyntax-analysis?
       (listof (hash/c source? refactoring-result-set? #:flat? #true #:immutable #true)))]
  [resyntax-analysis-final-sources (-> resyntax-analysis? (listof modified-source?))]
  [resyntax-analysis-total-fixes (-> resyntax-analysis? exact-nonnegative-integer?)]
  [resyntax-analysis-total-sources-modified (-> resyntax-analysis? exact-nonnegative-integer?)]
  [resyntax-analysis-rules-applied (-> resyntax-analysis? multiset?)]
  [resyntax-analysis-write-file-changes! (-> resyntax-analysis? void?)]
  [resyntax-analysis-commit-fixes! (-> resyntax-analysis? void?)]
  [resyntax-analyze
   (->* (source?) (#:suite refactoring-suite? #:lines range-set? #:timeout-ms exact-nonnegative-integer?) refactoring-result-set?)]
  [resyntax-analyze-all
   (->* ((hash/c source? range-set? #:flat? #true))
        (#:suite refactoring-suite?
         #:max-fixes (or/c exact-nonnegative-integer? +inf.0)
         #:max-passes exact-nonnegative-integer?
         #:max-modified-sources (or/c exact-nonnegative-integer? +inf.0)
         #:max-modified-lines (or/c exact-nonnegative-integer? +inf.0)
         #:timeout-ms exact-nonnegative-integer?)
        resyntax-analysis?)]
  [reysntax-analyze-for-properties-only
   (->* (source?) (#:suite refactoring-suite? #:timeout-ms exact-nonnegative-integer?) syntax-property-bundle?)]
  [refactor! (-> (sequence/c refactoring-result?) void?)]))


(require fancy-app
         guard
         racket/file
         racket/match
         racket/sequence
         racket/set
         racket/string
         rebellion/base/comparator
         rebellion/base/option
         rebellion/base/range
         rebellion/collection/entry
         rebellion/collection/hash
         rebellion/collection/list
         rebellion/collection/multiset
         resyntax/private/commit
         rebellion/collection/range-set
         rebellion/streaming/reducer
         rebellion/streaming/transducer
         rebellion/type/record
         resyntax/base
         resyntax/default-recommendations
         resyntax/private/analysis
         resyntax/private/git
         resyntax/private/limiting
         resyntax/private/line-replacement
         resyntax/private/logger
         resyntax/private/refactoring-result
         resyntax/private/source
         resyntax/private/string-indent
         resyntax/private/string-replacement
         resyntax/private/syntax-property-bundle
         resyntax/private/syntax-range
         resyntax/private/syntax-replacement
         (except-in racket/list range)
         (submod resyntax/base private))


(module+ test
  (require racket/list
           rackunit
           resyntax/private/analyzer
           (submod "..")))


;@----------------------------------------------------------------------------------------------------


(define-record-type resyntax-analysis (all-results) #:omit-root-binding)


(define (resyntax-analysis #:all-results all-results)
  (constructor:resyntax-analysis #:all-results (sequence->list all-results)))


(define (resyntax-analysis-final-sources analysis)
  (transduce (resyntax-analysis-all-results analysis)
             (append-mapping in-hash-values)
             (mapping refactoring-result-set-updated-source)
             (indexing modified-source-original)
             (grouping nonempty-into-last)
             (mapping entry-value)
             #:into into-list))


(define (resyntax-analysis-total-fixes analysis)
  (for*/sum ([pass-results (in-list (resyntax-analysis-all-results analysis))]
             [result-set (in-hash-values pass-results)])
    (length (refactoring-result-set-results result-set))))


(define/guard (resyntax-analysis-total-sources-modified analysis)
  (define all-results (resyntax-analysis-all-results analysis))
  (guard (not (empty? all-results)) #:else 0)
  (hash-count (first all-results)))


(define (resyntax-analysis-rules-applied analysis)
  (for*/multiset ([pass-results (in-list (resyntax-analysis-all-results analysis))]
                  [result-set (in-hash-values pass-results)]
                  [result (in-list (refactoring-result-set-results result-set))])
    (refactoring-result-rule-name result)))


(define (analysis-pass-fix-commits pass-results)
  (append-map refactoring-result-map-commits pass-results))


(define (resyntax-analysis-fix-commits analysis)
  (append-map refactoring-result-map-commits (resyntax-analysis-all-results analysis)))


(define (resyntax-analysis-write-file-changes! analysis)
  (define sources (resyntax-analysis-final-sources analysis))
  (unless (empty? sources)
    (log-resyntax-info "--- fixing code ---"))
  (for ([source (in-list sources)]
        #:when (source-path source))
    (log-resyntax-info "fixing ~a" (source-path source))
    (display-to-file (modified-source-contents source) (source-path source)
                     #:mode 'text #:exists 'replace)))


(define (resyntax-analysis-commit-fixes! analysis)
  (define commits (resyntax-analysis-fix-commits analysis))
  (unless (empty? commits)
    (log-resyntax-info "--- fixing code ---"))
  (for ([commit (in-list commits)]
        [i (in-naturals 1)])
    (log-resyntax-info "--- commit ~a ---" i)
    (match-define (resyntax-commit message changes) commit)
    (for ([(path new-contents) (in-hash changes)])
      (log-resyntax-info "fixing ~a" path)
      (display-to-file new-contents path #:mode 'text #:exists 'replace))
    (log-resyntax-info "commiting pass fixes")
    (git-commit! message)))


;; Try to dynamically load a refactoring suite from a language module's resyntax submodule
(define (try-load-lang-refactoring-suite lang-name)
  (with-handlers
      ([exn:fail?
        (λ (e)
          (log-resyntax-error
           "could not load language refactoring suite due to error\n  language: ~a\n  error:\n~a"
           lang-name
           (string-indent (exn-message e) #:amount 3))
          #false)])
    (define lang-resyntax-submod `(submod ,lang-name resyntax))
    (dynamic-require lang-resyntax-submod 'refactoring-suite (λ () #false))))

(define allowed-langs (set 'racket 'racket/base 'racket/gui))


(define/guard (resyntax-analyze source
                                #:suite [suite default-recommendations]
                                #:lines [lines (range-set (unbounded-range #:comparator natural<=>))]
                                #:timeout-ms [timeout-ms 10000])
  (define comments (source-comment-locations source))
  (define source-lang (source-read-language source))
  (guard source-lang #:else
    (log-resyntax-warning "skipping ~a because its #lang could not be determined"
                          (or (source-path source) "string source"))
    (refactoring-result-set #:base-source source #:results '()))
  ;; Handle supported languages and try dynamic loading for unsupported ones
  (define effective-suite
    (cond
      [(set-member? allowed-langs source-lang) suite]
      [else
       (define lang-suite (try-load-lang-refactoring-suite source-lang))
       (cond
         [lang-suite
          (log-resyntax-debug "using refactoring suite from #lang ~a resyntax submodule for ~a"
                              source-lang (or (source-path source) "string source"))
          lang-suite]
         [else
          (log-resyntax-warning
           (string-append "skipping ~a because it's written in #lang ~a, which is unsupported and"
                          " has no resyntax submodule")
           (or (source-path source) "string source") source-lang)
          #false])]))
  
  (guard effective-suite #:else
    (refactoring-result-set #:base-source source #:results '()))
  (define full-source (source->string source))
  (log-resyntax-info "analyzing ~a" (or (source-path source) "string source"))
  (for ([comment (in-range-set comments)])
    (log-resyntax-debug "parsed comment: ~a: ~v" comment (substring-by-range full-source comment)))

  (define (skip e)
    (log-resyntax-error
     "skipping ~a\n encountered an error during initial analysis\n  error:\n~a"
     (or (source-path source) "string source")
     (string-indent (exn-message e) #:amount 3))
    empty-list)

  (define results
    (with-handlers ([exn:fail? skip])
      (define analysis (source-analyze source
                                       #:lines lines
                                       #:analyzers (refactoring-suite-analyzers effective-suite)
                                       #:timeout-ms timeout-ms))
      (refactor-visited-forms
       #:analysis analysis #:suite effective-suite #:comments comments #:lines lines)))
  
  (define result-set (refactoring-result-set #:base-source source #:results results))
  
  ;; Filter out result sets that produce non-compiling code
  (cond
    [(and (not (empty? results))
          (not (refactoring-result-set-compiles? result-set)))
     (log-resyntax-warning
      "dropping ~a refactoring suggestion~a for ~a because the modified code does not compile"
      (length results)
      (if (equal? (length results) 1) "" "s")
      (or (source-path source) "string source"))
     (refactoring-result-set #:base-source source #:results '())]
    [else result-set]))


(define/guard (reysntax-analyze-for-properties-only source
                                                    #:suite [suite default-recommendations]
                                                    #:timeout-ms [timeout-ms 10000])
  (define comments (source-comment-locations source))
  (define full-source (source->string source))
  (guard (string-prefix? full-source "#lang racket") #:else
    (log-resyntax-warning "skipping ~a because it does not start with #lang racket"
                          (or (source-path source) "string source"))
    (syntax-property-bundle))
  (log-resyntax-info "analyzing ~a" (or (source-path source) "string source"))
  (for ([comment (in-range-set comments)])
    (log-resyntax-debug "parsed comment: ~a: ~v" comment (substring-by-range full-source comment)))

  (define (skip e)
    (log-resyntax-error
     "skipping ~a\n encountered an error during macro expansion\n  error:\n~a"
     (or (source-path source) "string source")
     (string-indent (exn-message e) #:amount 3))
    (syntax-property-bundle))

  (with-handlers ([exn:fail:syntax? skip]
                  [exn:fail:filesystem:missing-module? skip]
                  [exn:fail:contract:variable? skip])
    (define analysis (source-analyze source
                                     #:analyzers (refactoring-suite-analyzers suite)
                                     #:timeout-ms timeout-ms))
    (source-code-analysis-added-syntax-properties analysis)))


(define (resyntax-analyze-all sources
                              #:suite [suite default-recommendations]
                              #:max-fixes [max-fixes +inf.0]
                              #:max-passes [max-passes 10]
                              #:max-modified-sources [max-modified-sources +inf.0]
                              #:max-modified-lines [max-modified-lines +inf.0]
                              #:timeout-ms [timeout-ms 10000])
  (log-resyntax-info "--- analyzing code ---")
  (for/fold ([pass-result-lists '()]
             [sources sources]
             [max-fixes max-fixes]
             #:result (resyntax-analysis #:all-results (reverse pass-result-lists)))
            ([pass-index (in-range max-passes)]
             #:do [(unless (zero? pass-index)
                     (log-resyntax-info "--- pass ~a ---" (add1 pass-index)))
                   (define pass-results
                     (resyntax-analyze-all-once sources
                                                #:suite suite
                                                #:max-fixes max-fixes
                                                #:max-modified-sources max-modified-sources
                                                #:max-modified-lines max-modified-lines
                                                #:timeout-ms timeout-ms))
                   (define pass-fix-count (count-total-results pass-results))
                   (define new-max-fixes (- max-fixes pass-fix-count))]
             #:break (hash-empty? pass-results)
             #:final (zero? new-max-fixes))
    (define modified-sources (build-modified-source-map pass-results))
    (values (cons pass-results pass-result-lists) modified-sources new-max-fixes)))


(define (count-total-results pass-results)
  (for/sum ([(_ result-set) (in-hash pass-results)])
    (length (refactoring-result-set-results result-set))))


(define (build-modified-source-map pass-results)
  (transduce (in-hash-values pass-results)
             (bisecting refactoring-result-set-updated-source refactoring-result-set-modified-lines)
             #:into into-hash))


(define (resyntax-analyze-all-once sources
                                   #:suite suite
                                   #:max-fixes max-fixes
                                   #:max-modified-sources max-modified-sources
                                   #:max-modified-lines max-modified-lines
                                   #:timeout-ms timeout-ms)
  (transduce (in-hash-entries sources) ; entries with source keys and line range set values

             ;; The following steps perform a kind of layered shuffle: the files to refactor are
             ;; shuffled such that files in the same directory remain together. When combined with
             ;; the #:max-modified-sources argument, this makes Resyntax prefer to refactor closely
             ;; related files instead of selecting arbitrary unrelated files from across an entire
             ;; codebase. This limits potential for merge conflicts and makes changes easier to
             ;; review, since it's more likely the refactored files will have shared context.

             ; key by directory
             (indexing (λ (e) (source-directory (entry-key e))))

             ; group by key and shuffle within each group
             (grouping (into-transduced (shuffling) #:into into-list))

             ; shuffle groups
             (shuffling)

             ; ungroup and throw away directory
             (append-mapping entry-value)

             ;; Now the stream contains exactly what it did before the above steps, but shuffled in
             ;; a convenient manner.
               
             (append-mapping
              (λ (e)
                (match-define (entry source lines) e)
                (define result-set (resyntax-analyze source #:suite suite #:lines lines #:timeout-ms timeout-ms))
                (refactoring-result-set-results result-set)))
             (limiting max-modified-lines
                       #:by (λ (result)
                              (define replacement (refactoring-result-line-replacement result))
                              (add1 (- (line-replacement-original-end-line replacement)
                                       (line-replacement-start-line replacement)))))
             (if (equal? max-fixes +inf.0) (transducer-pipe) (taking max-fixes))
             (if (equal? max-modified-sources +inf.0)
                 (transducer-pipe)
                 (transducer-pipe
                  (indexing
                   (λ (result)
                     (syntax-replacement-source (refactoring-result-syntax-replacement result))))
                  (grouping into-list)
                  (taking max-modified-sources)
                  (append-mapping entry-value)))
             (indexing refactoring-result-source)
             (grouping into-list)
             (mapping
              (λ (e) (refactoring-result-set #:base-source (entry-key e) #:results (entry-value e))))
             (filtering
              (λ (result-set)
                (define compiles? (refactoring-result-set-compiles? result-set))
                (unless compiles?
                  (define source (refactoring-result-set-base-source result-set))
                  (define num-results (length (refactoring-result-set-results result-set)))
                  (log-resyntax-warning
                   "dropping ~a refactoring suggestion~a for ~a because the modified code does not compile"
                   num-results
                   (if (equal? num-results 1) "" "s")
                   (or (source-path source) "string source")))
                compiles?))
             (indexing refactoring-result-set-base-source)
             #:into into-hash))


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
          (parameterize ([current-namespace (source-code-analysis-namespace analysis)])
            (refactoring-rule-refactor rule syntax (source-code-analysis-code analysis)))
          #:else absent)
        (guard (syntax-replacement-introduces-incorrect-bindings? replacement) #:else
          (define bad-ids (syntax-replacement-introduced-incorrect-identifiers replacement))
          (define orig-stx (syntax-replacement-original-syntax replacement))
          (define intro (syntax-replacement-introduction-scope replacement))
          (log-resyntax-warning
           (string-append
            "~a: suggestion discarded because it introduces identifiers with incorrect bindings\n"
            "  incorrect identifiers: ~a\n"
            "  bindings in original context: ~a\n"
            "  bindings in syntax replacement: ~a\n"
            "  replaced syntax: ~a")
           (object-name rule)
           bad-ids
           (for/list ([id (in-list bad-ids)])
             (identifier-binding (datum->syntax orig-stx (syntax->datum id))))
           (for/list ([id (in-list bad-ids)])
             (identifier-binding (intro id 'remove)))
           orig-stx)
          absent)
        (guard (syntax-replacement-preserves-comments? replacement comments) #:else
          (log-resyntax-warning
           (string-append "~a: suggestion discarded because it does not preserve all comments\n"
                          "  dropped comment locations: ~v\n"
                          "  original syntax:\n"
                          "   ~v\n"
                          "  replacement syntax:\n"
                          "   ~v")
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
                    "  lines modified by result: ~a")
     (refactoring-result-rule-name result)
     lines
     modified-lines))
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
  (test-case "resyntax-analyze"
    (define results
      (refactoring-result-set-results
       (resyntax-analyze (string-source "#lang racket (or 1 (or 2 3))"))))
    (check-equal? (length results) 1)
    (check-equal? (refactoring-result-string-replacement (first results))
                  (string-replacement #:start 13
                                      #:end 28
                                      #:contents (list (inserted-string "(or 1 2 3)")))))
  
  (test-case "resyntax-analyze uses suite analyzers"
    (define test-suite default-recommendations)
    (check-true (set? (refactoring-suite-analyzers test-suite)))
    (check-false (set-empty? (refactoring-suite-analyzers test-suite)))
    ;; Verify that all analyzers in the suite are expansion-analyzer?
    (check-true (for/and ([analyzer (in-set (refactoring-suite-analyzers test-suite))])
                  (expansion-analyzer? analyzer))))
  
  (test-case "broken refactoring rules are filtered out"
    ;; Define a refactoring rule that produces code that doesn't compile
    (define-refactoring-rule breaking-rule
      #:description "Breaking refactoring rule"
      #:datum-literals (foo)
      #:literals (define)
      (define foo 42)
      (if))
    
    (define breaking-suite (refactoring-suite #:rules (list breaking-rule)))
    (define test-source (string-source "#lang racket/base\n\n(define foo 42)\n"))
    
    ;; Test with direct analyze
    (define result-set (resyntax-analyze test-source #:suite breaking-suite))
    (check-equal? (length (refactoring-result-set-results result-set)) 0
                  "Breaking suggestions should be filtered from resyntax-analyze")
    
    ;; Test with multipass analyze
    (define analysis
      (resyntax-analyze-all (hash test-source (range-set (unbounded-range #:comparator natural<=>)))
                            #:suite breaking-suite
                            #:timeout-ms 10000))
    (check-equal? (resyntax-analysis-total-fixes analysis) 0
                  "Breaking suggestions should be filtered from resyntax-analyze-all")))
