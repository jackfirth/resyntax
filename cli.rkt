#lang racket/base


(require fancy-app
         json
         racket/cmdline
         racket/format
         racket/logging
         racket/match
         racket/path
         racket/port
         rebellion/base/comparator
         rebellion/base/range
         rebellion/collection/entry
         rebellion/collection/hash
         rebellion/collection/list
         rebellion/collection/multiset
         rebellion/collection/range-set
         rebellion/collection/vector/builder
         rebellion/streaming/reducer
         rebellion/streaming/transducer
         rebellion/type/enum
         rebellion/type/record
         resyntax
         resyntax/base
         resyntax/default-recommendations
         resyntax/private/file-group
         resyntax/private/github
         resyntax/private/refactoring-result
         resyntax/private/source
         resyntax/private/string-indent
         resyntax/private/syntax-replacement)


;@----------------------------------------------------------------------------------------------------


(define-enum-type resyntax-output-format (plain-text github-pull-request-review git-commit-message json))
(define-enum-type resyntax-fix-method (modify-files create-multiple-git-commits))
(define-record-type resyntax-analyze-options (targets suite output-format output-destination))


(define-record-type resyntax-fix-options
  (targets
   suite
   fix-method
   output-format
   max-fixes
   max-modified-files
   max-modified-lines
   max-pass-count))


(define all-lines (range-set (unbounded-range #:comparator natural<=>)))


(define (resyntax-analyze-parse-command-line)
  (define targets (make-vector-builder))
  (define suite default-recommendations)
  (define selected-rule #false)
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

   ("--refactoring-rule"
    rule-name
    "Only use this refactoring rule from the refactoring suite, instead of all of the suite's rules."
    (define rule-sym (string->symbol rule-name))
    (set! selected-rule rule-sym))

   ("--output-to-file"
    outputpath
    "Store results in a file instead of printing them to the console."
    (set! output-destination (simple-form-path outputpath)))

   ("--output-as-github-review"
    "Report results by leaving a GitHub review on the pull request currently being analyzed, as \
determined by the GITHUB_REPOSITORY and GITHUB_REF environment variables."
    (set! output-format github-pull-request-review)))

  (when selected-rule
    (define filtered-suite
      (refactoring-suite
       #:name (object-name suite)
       #:rules (for/list ([rule (in-list (refactoring-suite-rules suite))]
                          #:when (equal? (object-name rule) selected-rule))
                 rule)))
    (set! suite filtered-suite))
  
  (resyntax-analyze-options
   #:targets (build-vector targets)
   #:suite suite
   #:output-format output-format
   #:output-destination output-destination))


(define (resyntax-fix-parse-command-line)
  (define targets (make-vector-builder))
  (define suite default-recommendations)
  (define selected-rule #false)
  (define (add-target! target)
    (vector-builder-add targets target))
  (define fix-method modify-files)
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
   
   ("--local-git-repository"
    repopath baseref
    "A Git repository to search for modified files to fix. The repopath argument is a directory
path to the root of a Git repository, and the baseref argument is a Git reference (in the form \
\"remotename/branchname\") to use as the base state of the repository. Any files that have been \
changed relative to baseref are analyzed and fixed."
    (add-target! (git-repository-file-group repopath baseref)))

   #:once-each

   ("--create-multiple-commits"
    "Modify files by creating a series of individual Git commits."
    (set! fix-method create-multiple-git-commits))

   ("--output-as-commit-message"
    "Report results in the form of a Git commit message printed to stdout."
    (set! output-format git-commit-message))

   ("--output-as-json"
    "Report results in the form of a JSON object printed to stdout."
    (set! output-format json))

   ("--refactoring-suite"
    modpath
    suite-name
    "The refactoring suite to analyze code with."
    (define parsed-modpath (read (open-input-string modpath)))
    (define parsed-suite-name (read (open-input-string suite-name)))
    (set! suite (dynamic-require parsed-modpath parsed-suite-name)))

   ("--refactoring-rule"
    rule-name
    "Only use this refactoring rule from the refactoring suite, instead of all of the suite's rules."
    (define rule-sym (string->symbol rule-name))
    (set! selected-rule rule-sym))

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

  (when selected-rule
    (define filtered-suite
      (refactoring-suite
       #:name (object-name suite)
       #:rules (for/list ([rule (in-list (refactoring-suite-rules suite))]
                          #:when (equal? (object-name rule) selected-rule))
                 rule)))
    (set! suite filtered-suite))

  (resyntax-fix-options #:targets (build-vector targets)
                        #:suite suite
                        #:fix-method fix-method
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
  (define sources (file-groups-resolve (resyntax-analyze-options-targets options)))
  (define analysis
    (resyntax-analyze-all sources
                          #:suite (resyntax-analyze-options-suite options)
                          #:max-passes 1))
  (define results
    (transduce (resyntax-analysis-all-results analysis)
               (append-mapping in-hash-values)
               (append-mapping refactoring-result-set-results)
               #:into into-list))

  (define (display-results)
    (match (resyntax-analyze-options-output-format options)
      [(== plain-text)
       (for ([result (in-list results)])
         (define path
           (file-source-path
            (syntax-replacement-source (refactoring-result-syntax-replacement result))))
         (define line (refactoring-result-original-line result))
         (define column (refactoring-result-original-column result))
         (printf "resyntax: ~a:~a:~a [~a]\n" path line column (refactoring-result-rule-name result))
         (printf "\n\n~a\n" (string-indent (refactoring-result-message result) #:amount 2))
         (define old-code (refactoring-result-original-code result))
         (define new-code (refactoring-result-new-code result))
         (printf "\n\n~a\n\n\n~a\n\n\n"
                 (string-indent (~a old-code) #:amount 2)
                 (string-indent (~a new-code) #:amount 2)))]
      [(== github-pull-request-review)
       (define req (refactoring-results->github-review results #:file-count (hash-count sources)))
       (write-json (github-review-request-jsexpr req))]))

  (match (resyntax-analyze-options-output-destination options)
    ['console
     (displayln "resyntax: --- displaying results ---")
     (display-results)]
    [(? path? output-path)
     (displayln "resyntax: --- writing results to file ---")
     (with-output-to-file output-path display-results #:mode 'text)]))


(define (resyntax-fix-run)
  (define options (resyntax-fix-parse-command-line))
  (define fix-method (resyntax-fix-options-fix-method options))
  (define output-format (resyntax-fix-options-output-format options))
  (define sources (file-groups-resolve (resyntax-fix-options-targets options)))
  (define max-modified-files (resyntax-fix-options-max-modified-files options))
  (define max-modified-lines (resyntax-fix-options-max-modified-lines options))
  (define analysis
    (resyntax-analyze-all sources
                          #:suite (resyntax-fix-options-suite options)
                          #:max-fixes (resyntax-fix-options-max-fixes options)
                          #:max-passes (resyntax-fix-options-max-pass-count options)
                          #:max-modified-sources max-modified-files
                          #:max-modified-lines max-modified-lines))
  (match fix-method
    [(== modify-files)
     (resyntax-analysis-write-file-changes! analysis)]
    [(== create-multiple-git-commits)
     (resyntax-analysis-commit-fixes! analysis)])
  (match output-format
    [(== git-commit-message)
     (resyntax-fix-print-git-commit-message analysis)]
    [(== json)
     (resyntax-fix-print-json analysis)]
    [(== plain-text)
     (resyntax-fix-print-plain-text-summary analysis)]))


(define (resyntax-fix-print-git-commit-message analysis)
  (define total-fixes (resyntax-analysis-total-fixes analysis))
  (define total-files (resyntax-analysis-total-sources-modified analysis))
  (define fix-counts-by-rule
    (transduce (in-hash-entries (multiset-frequencies (resyntax-analysis-rules-applied analysis)))
               (sorting #:key entry-value #:descending? #true)
               #:into into-list))
  (define issue-string (if (> total-fixes 1) "issues" "issue"))
  (define file-string (if (> total-files 1) "files" "file"))
  (if (zero? total-fixes)
      (displayln "Resyntax found no issues.")
      (printf "Resyntax fixed ~a ~a in ~a ~a.\n\n" total-fixes issue-string total-files file-string))
  (for ([rule+count (in-list fix-counts-by-rule)])
    (match-define (entry rule count) rule+count)
    (define occurrence-string (if (> count 1) "occurrences" "occurrence"))
    (printf "  * Fixed ~a ~a of `~a`\n" count occurrence-string rule))
  (unless (zero? total-fixes)
    (newline)))


(define (resyntax-fix-print-plain-text-summary analysis)
  (displayln "resyntax: --- summary ---\n")
  (define total-fixes (resyntax-analysis-total-fixes analysis))
  (define total-files (resyntax-analysis-total-sources-modified analysis))
  (define message
    (cond
      [(zero? total-fixes) "No issues found."]
      [(equal? total-fixes 1) "Fixed 1 issue in 1 file."]
      [(equal? total-files 1) (format "Fixed ~a issues in 1 file." total-fixes)]
      [else (format "Fixed ~a issues in ~a files." total-fixes total-files)]))
  (printf "  ~a\n\n" message)
  (define rules-applied (resyntax-analysis-rules-applied analysis))
  (transduce (in-hash-entries (multiset-frequencies rules-applied))
             (sorting #:key entry-value #:descending? #true)
             (mapping
              (λ (e)
                (match-define (entry rule-name rule-fixes) e)
                (define message
                  (if (equal? rule-fixes 1)
                      (format "Fixed 1 occurrence of ~a" rule-name)
                      (format "Fixed ~a occurrences of ~a" rule-fixes rule-name)))
                (format "  * ~a\n" message)))
             #:into (into-for-each display))
  (when (positive? total-fixes)
    (newline)))


(define (resyntax-fix-print-json analysis)
  (define total-fixes (resyntax-analysis-total-fixes analysis))
  (define total-files (resyntax-analysis-total-sources-modified analysis))
  (define fix-counts-by-rule
    (transduce (in-hash-entries (multiset-frequencies (resyntax-analysis-rules-applied analysis)))
               (sorting #:key entry-value #:descending? #true)
               #:into into-list))
  
  ;; Build commit message
  (define commit-message
    (with-output-to-string
      (λ ()
        (define issue-string (if (> total-fixes 1) "issues" "issue"))
        (define file-string (if (> total-files 1) "files" "file"))
        (if (zero? total-fixes)
            (display "Resyntax found no issues.")
            (printf "Resyntax fixed ~a ~a in ~a ~a." 
                    total-fixes issue-string total-files file-string))
        (unless (zero? total-fixes)
          (newline)
          (for ([rule+count (in-list fix-counts-by-rule)])
            (match-define (entry rule count) rule+count)
            (define occurrence-string (if (> count 1) "occurrences" "occurrence"))
            (printf "\n  * Fixed ~a ~a of `~a`" count occurrence-string rule))))))
  
  ;; Output JSON
  (write-json
   (hasheq 'commit_message_body commit-message
           'fix_count total-fixes))
  (newline))


(module+ main
  (resyntax-run))
