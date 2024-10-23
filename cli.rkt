#lang racket/base


(require fancy-app
         json
         racket/cmdline
         racket/format
         racket/hash
         racket/logging
         racket/match
         racket/path
         rebellion/base/comparator
         rebellion/base/range
         rebellion/collection/entry
         rebellion/collection/hash
         rebellion/collection/list
         rebellion/collection/range-set
         rebellion/collection/vector/builder
         rebellion/streaming/reducer
         rebellion/streaming/transducer
         rebellion/type/enum
         rebellion/type/record
         resyntax
         resyntax/default-recommendations
         resyntax/private/file-group
         resyntax/private/github
         resyntax/private/limiting
         resyntax/private/line-replacement
         resyntax/private/refactoring-result
         resyntax/private/source
         resyntax/private/string-indent
         resyntax/private/syntax-replacement
         (only-in racket/list append-map empty? shuffle))


;@----------------------------------------------------------------------------------------------------


(define-enum-type resyntax-output-format (plain-text github-pull-request-review git-commit-message))
(define-record-type resyntax-analyze-options (targets suite output-format output-destination))
(define-record-type resyntax-fix-options
  (targets suite output-format max-fixes max-modified-files max-modified-lines max-pass-count))


(define all-lines (range-set (unbounded-range #:comparator natural<=>)))


(define (resyntax-analyze-parse-command-line)
  (define targets (make-vector-builder))
  (define suite default-recommendations)
  (define output-format plain-text)
  (define output-destination 'console)

  (command-line
   #:program "resyntax analyze"

   #:multi

   ("--file"
    filepath
    "A file to analyze."
    (vector-builder-add targets (single-file-group filepath all-lines)))

   ("--directory"
    dirpath
    "A directory to anaylze, including subdirectories."
    (vector-builder-add targets (directory-file-group dirpath)))

   ("--package"
    pkgname
    "An installed package to analyze."
    (vector-builder-add targets (package-file-group pkgname)))

   ("--local-git-repository"
    repopath baseref
    "A Git repository to search for modified files to analyze. The repopath argument is a directory
path to the root of a Git repository, and the baseref argument is a Git reference (in the form \
\"remotename/branchname\") to use as the base state of the repository. Any files that have been \
changed relative to baseref are analyzed."
    (vector-builder-add targets (git-repository-file-group repopath baseref)))

   #:once-each

   ("--refactoring-suite"
    modpath
    suite-name
    "The refactoring suite to analyze code with."
    (define parsed-modpath (read (open-input-string modpath)))
    (define parsed-suite-name (read (open-input-string suite-name)))
    (set! suite (dynamic-require parsed-modpath parsed-suite-name)))

   ("--output-to-file"
    outputpath
    "Store results in a file instead of printing them to the console."
    (set! output-destination (simple-form-path outputpath)))

   ("--output-as-github-review"
    "Report results by leaving a GitHub review on the pull request currently being analyzed, as \
determined by the GITHUB_REPOSITORY and GITHUB_REF environment variables."
    (set! output-format github-pull-request-review)))
  
  (resyntax-analyze-options
   #:targets (build-vector targets)
   #:suite suite
   #:output-format output-format
   #:output-destination output-destination))


(define (resyntax-fix-parse-command-line)
  (define targets (make-vector-builder))
  (define suite default-recommendations)
  (define (add-target! target)
    (vector-builder-add targets target))
  (define output-format plain-text)
  (define max-fixes +inf.0)
  (define max-pass-count 10)
  (define max-modified-files +inf.0)
  (define max-modified-lines +inf.0)

  (command-line
   #:program "resyntax fix"

   #:multi

   ("--file" filepath "A file to fix." (add-target! (single-file-group filepath all-lines)))

   ("--directory"
    dirpath
    "A directory to fix, including subdirectories."
    (add-target! (directory-file-group dirpath)))
   
   ("--package"
    pkgname
    "An installed package to fix."
    (add-target! (package-file-group pkgname)))
   
   ("--output-as-commit-message"
    "Report results in the form of a Git commit message printed to stdout."
    (set! output-format git-commit-message))
   
   ("--local-git-repository"
    repopath baseref
    "A Git repository to search for modified files to fix. The repopath argument is a directory
path to the root of a Git repository, and the baseref argument is a Git reference (in the form \
\"remotename/branchname\") to use as the base state of the repository. Any files that have been \
changed relative to baseref are analyzed and fixed."
    (add-target! (git-repository-file-group repopath baseref)))

   #:once-each

   ("--refactoring-suite"
    modpath
    suite-name
    "The refactoring suite to analyze code with."
    (define parsed-modpath (read (open-input-string modpath)))
    (define parsed-suite-name (read (open-input-string suite-name)))
    (set! suite (dynamic-require parsed-modpath parsed-suite-name)))

   ("--max-pass-count"
    passcount
    "The maximum number of times Resyntax will fix each file. By default, Resyntax runs at most 10 \
passes over each file (or fewer, if no fixes would be made by additional passes). Multiple passes \
are needed when applying a fix unlocks further fixes."
    (set! max-pass-count (string->number passcount)))

   ("--max-fixes"
    fixlimit
    "The maximum number of fixes to apply. If not specified, all fixes found will be applied."
    (set! max-fixes (string->number fixlimit)))

   ("--max-modified-files"
    modifiedlimit
    "The maximum number of files to modify. If not specified, fixes will be applied to all files."
    (set! max-modified-files (string->number modifiedlimit)))

   ("--max-modified-lines"
    modifiedlines
    "The maximum number of lines to modify. If not specified, no line limit is applied."
    (set! max-modified-lines (string->number modifiedlines))))

  (resyntax-fix-options #:targets (build-vector targets)
                        #:suite suite
                        #:output-format output-format
                        #:max-fixes max-fixes
                        #:max-modified-files max-modified-files
                        #:max-modified-lines max-modified-lines
                        #:max-pass-count max-pass-count))


(define (resyntax-run)
  (command-line
   #:program "resyntax"

   #:usage-help
   "\n<command> is one of

\tanalyze
\tfix

For help on these, use 'analyze --help' or 'fix --help'."

   #:ps "\nSee https://docs.racket-lang.org/resyntax/index.html for details."
   #:args (command . leftover-args)
   (define leftover-arg-vector (vector->immutable-vector (list->vector leftover-args)))

   (define (call-command command-thunk)
     (parameterize ([current-command-line-arguments leftover-arg-vector])
       (with-logging-to-port (current-error-port)
         command-thunk
         #:logger (current-logger)
         'info 'resyntax
         'error)))

   (match command
     ["analyze" (call-command resyntax-analyze-run)]
     ["fix" (call-command resyntax-fix-run)])))


(define (resyntax-analyze-run)
  (define options (resyntax-analyze-parse-command-line))
  (define files (file-groups-resolve (resyntax-analyze-options-targets options)))
  (printf "resyntax: --- analyzing code ---\n")
  (define results
    (transduce files
               (append-mapping
                (λ (portion)
                  (resyntax-analyze (file-source (file-portion-path portion))
                                    #:suite (resyntax-analyze-options-suite options)
                                    #:lines (file-portion-lines portion))))
               #:into into-list))

  (define (display-results)
    (match (resyntax-analyze-options-output-format options)
      [(== plain-text)
       (for ([result (in-list results)])
         (define path
           (file-source-path
            (syntax-replacement-source (refactoring-result-syntax-replacement result))))
         (printf "resyntax: ~a [~a]\n" path (refactoring-result-rule-name result))
         (printf "\n\n~a\n" (string-indent (refactoring-result-message result) #:amount 2))
         (define old-code (refactoring-result-original-code result))
         (define new-code (refactoring-result-new-code result))
         (printf "\n\n~a\n\n\n~a\n\n\n"
                 (string-indent (~a old-code) #:amount 2)
                 (string-indent (~a new-code) #:amount 2)))]
      [(== github-pull-request-review)
       (define req (refactoring-results->github-review results #:file-count (length files)))
       (write-json (github-review-request-jsexpr req))]))

  (match (resyntax-analyze-options-output-destination options)
    ['console
     (printf "resyntax: --- displaying results ---\n")
     (display-results)]
    [(? path? output-path)
     (printf "resyntax: --- writing results to file ---\n")
     (with-output-to-file output-path display-results #:mode 'text)]))


(define (resyntax-fix-run)
  (define options (resyntax-fix-parse-command-line))
  (define output-format (resyntax-fix-options-output-format options))
  (match output-format
    [(== git-commit-message)
     (display "This is an automated change generated by Resyntax.\n\n")]
    [_ (void)])
  (define files
    (transduce (file-groups-resolve (resyntax-fix-options-targets options))
               (indexing file-portion-path)
               (grouping into-list)
               #:into into-hash))
  (define max-modified-files (resyntax-fix-options-max-modified-files options))
  (define max-modified-lines (resyntax-fix-options-max-modified-lines options))
  (define results-by-path
    (for/fold ([all-results (hash)]
               [files files]
               [max-fixes (resyntax-fix-options-max-fixes options)]
               [lines-to-analyze-by-file (hash)]
               #:result all-results)
              ([pass-number (in-inclusive-range 1 (resyntax-fix-options-max-pass-count options))]
               #:do [(define pass-results
                       (resyntax-fix-run-one-pass options files
                                                  #:lines lines-to-analyze-by-file
                                                  #:max-fixes max-fixes
                                                  #:max-modified-files max-modified-files
                                                  #:max-modified-lines max-modified-lines
                                                  #:pass-number pass-number))
                     (define pass-fix-count
                       (for/sum ([(_ results) (in-hash pass-results)])
                         (length results)))
                     (define pass-modified-file-count (hash-count pass-results))
                     (define new-max-fixes (- max-fixes pass-fix-count))]
               #:break (hash-empty? pass-results)
               #:final (zero? new-max-fixes))
      (define new-files (hash-filter-keys files (hash-has-key? pass-results _)))
      (define new-lines-to-analyze
        (for/hash ([(path results) (in-hash pass-results)])
          (values path
                  (transduce results
                             (mapping refactoring-result-modified-line-range)
                             (filtering nonempty-range?)
                             #:into (into-range-set natural<=>)))))
      (values (hash-union all-results pass-results #:combine append)
              new-files
              new-max-fixes
              new-lines-to-analyze)))
  (match output-format
    [(== plain-text) (printf "resyntax: --- summary ---\n")]
    [(== git-commit-message) (printf "## Summary\n\n")])
  (define total-fixes
    (for/sum ([(_ results) (in-hash results-by-path)])
      (length results)))
  (define total-files (hash-count results-by-path))
  (define fix-counts-by-rule
    (transduce (hash-values results-by-path)
               (append-mapping values)
               (indexing refactoring-result-rule-name)
               (grouping into-count)
               (sorting #:key entry-value #:descending? #true)
               #:into into-list))
  (define issue-string (if (> total-fixes 1) "issues" "issue"))
  (define file-string (if (> total-files 1) "files" "file"))
  (define summary-message
    (if (zero? total-fixes)
        "No issues found.\n"
        (format "Fixed ~a ~a in ~a ~a.\n\n" total-fixes issue-string total-files file-string)))
  (match output-format
    [(== plain-text) (printf "\n  ~a" summary-message)]
    [(== git-commit-message) (printf summary-message)])
  (for ([rule+count (in-list fix-counts-by-rule)])
    (match-define (entry rule count) rule+count)
    (define occurrence-string (if (> count 1) "occurrences" "occurrence"))
    (define rule-string
      (match output-format
        [(== plain-text) rule]
        [(== git-commit-message) (format "`~a`" rule)]))
    (printf "  * Fixed ~a ~a of ~a\n" count occurrence-string rule-string))
  (unless (zero? total-fixes)
    (newline)))


(define (resyntax-fix-run-one-pass options files
                                   #:lines lines-to-analyze-by-file
                                   #:max-fixes max-fixes
                                   #:max-modified-files max-modified-files
                                   #:max-modified-lines max-modified-lines
                                   #:pass-number pass-number)
  (define output-format (resyntax-fix-options-output-format options))
  (match output-format
    [(== plain-text)
     (unless (equal? pass-number 1)
       (printf "resyntax: --- pass ~a ---\n" pass-number))
     (printf "resyntax: --- analyzing code ---\n")]
    [_ (void)])
  (define all-results
    (transduce (in-hash-entries files) ; entries with file path keys and lists of file-portion? values

               ;; The following steps perform a kind of layered shuffle: the files to refactor are
               ;; shuffled such that files in the same directory remain together. When combined with
               ;; the #:max-modified-files argument, this makes Resyntax prefer to refactor closely
               ;; related files instead of selecting arbitrary unrelated files from across an entire
               ;; codebase. This limits potential for merge conflicts and makes changes easier to
               ;; review, since it's more likely the refactored files will have shared context.

               ; key by directory
               (indexing (λ (e) (simple-form-path (build-path (entry-key e) 'up))))

               ; group by key and shuffle within each group
               (grouping (into-transduced (shuffling) #:into into-list))

               ; shuffle groups
               (shuffling)

               ; ungroup and throw away directory
               (append-mapping entry-value)

               ;; Now the stream contains exactly what it did before the above steps, but shuffled in
               ;; a convenient manner.
               
               (append-mapping entry-value) ; throw away the file path, we don't need it anymore
               (mapping (filter-file-portion _ lines-to-analyze-by-file))
               (append-mapping
                (λ (portion)
                  (resyntax-analyze (file-portion-path portion)
                                    #:suite (resyntax-fix-options-suite options)
                                    #:lines (file-portion-lines portion))))
               (limiting max-modified-lines
                         #:by (λ (result)
                                (define replacement (refactoring-result-line-replacement result))
                                (add1 (- (line-replacement-original-end-line replacement)
                                         (line-replacement-start-line replacement)))))
               (if (equal? max-fixes +inf.0) (transducer-pipe) (taking max-fixes))
               (if (equal? max-modified-files +inf.0)
                   (transducer-pipe)
                   (transducer-pipe
                    (indexing
                     (λ (result)
                       (syntax-replacement-source (refactoring-result-syntax-replacement result))))
                    (grouping into-list)
                    (taking max-modified-files)
                    (append-mapping entry-value)))
               #:into into-list))
  (define results-by-path
    (transduce
     all-results
     (indexing
      (λ (result)
        (file-source-path
         (syntax-replacement-source (refactoring-result-syntax-replacement result)))))
     (grouping (into-transduced (sorting #:key refactoring-result-original-line) #:into into-list))
     #:into into-hash))
  (match output-format
    [(== plain-text) (printf "resyntax: --- fixing code ---\n")]
    [(== git-commit-message)
     (unless (hash-empty? results-by-path)
       (printf "#### Pass ~a\n\n" pass-number))])
  (for ([(path results) (in-hash results-by-path)])
    (define result-count (length results))
    (define fix-string (if (> result-count 1) "fixes" "fix"))
    (match output-format
      [(== plain-text)
       (printf "resyntax: applying ~a ~a to ~a\n\n" result-count fix-string path)]
      [(== git-commit-message)
       ;; For a commit message, we always use a relative path since we're likely running inside
       ;; some CI runner. Additionally, we make the path a link to the corresponding file at HEAD,
       ;; since making file paths clickable is pleasant.
       (define relative-path (find-relative-path (current-directory) path))
       (define repo-head-path (format "../blob/HEAD/~a" relative-path))
       (printf "Applied ~a ~a to [`~a`](~a)\n\n"
               result-count fix-string relative-path repo-head-path)])
    (for ([result (in-list results)])
      (define line (refactoring-result-original-line result))
      (define rule (refactoring-result-rule-name result))
      (define message (refactoring-result-message result))
      (match output-format
        [(== plain-text) (printf "  * [line ~a] ~a: ~a\n" line rule message)]
        [(== git-commit-message) (printf "  * Line ~a, `~a`: ~a\n" line rule message)]))
    (refactor! results)
    (newline))
  results-by-path)


(define (filter-file-portion portion lines-by-path)
  (define path (file-portion-path portion))
  (define lines (file-portion-lines portion))
  (define ranges-to-remove (range-set-complement (hash-ref lines-by-path path all-lines)))
  (file-portion path (range-set-remove-all lines ranges-to-remove)))


(module+ main
  (resyntax-run))
