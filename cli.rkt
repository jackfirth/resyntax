#lang racket/base


(require fancy-app
         racket/cmdline
         racket/format
         racket/match
         racket/string
         rebellion/collection/entry
         rebellion/collection/hash
         rebellion/collection/list
         rebellion/collection/vector/builder
         rebellion/streaming/reducer
         rebellion/streaming/transducer
         rebellion/type/enum
         rebellion/type/record
         resyntax
         resyntax/default-recommendations
         resyntax/file-group
         resyntax/private/github
         resyntax/private/string-indent
         resyntax/refactoring-result
         resyntax/source)


;@----------------------------------------------------------------------------------------------------


(define-enum-type resyntax-output-mode (output-to-console output-as-github-review))
(define-record-type resyntax-options (targets suite output-mode))


(define (resyntax-analyze-parse-command-line)
  (define targets (make-vector-builder))
  (define suite default-recommendations)
  (define output-mode output-to-console)
  (command-line
   #:program "resyntax analyze"
   #:multi
   ("--file" filepath "A file to analyze." (vector-builder-add targets (single-file-group filepath)))
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
   ("--output-as-github-review"
    "Report results by leaving a GitHub review on the pull request currently being analyzed, as \
determined by the GITHUB_REPOSITORY and GITHUB_REF environment variables."
    (set! output-mode output-as-github-review)))
  (resyntax-options #:targets (build-vector targets) #:suite suite #:output-mode output-mode))


(define (resyntax-fix-parse-command-line)
  (define targets (make-vector-builder))
  (define suite default-recommendations)
  (define (add-target! target)
    (vector-builder-add targets target))
  (command-line
   #:program "resyntax fix"
   #:multi
   ("--file" filepath "A file to fix." (add-target! (single-file-group filepath)))
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
   ("--refactoring-suite"
    modpath
    suite-name
    "The refactoring suite to analyze code with."
    (define parsed-modpath (read (open-input-string modpath)))
    (define parsed-suite-name (read (open-input-string suite-name)))
    (set! suite (dynamic-require parsed-modpath parsed-suite-name))))
  (resyntax-options #:targets (build-vector targets) #:suite suite #:output-mode output-to-console))


(define (resyntax-run)
  (command-line
   #:program "resyntax"
   #:args (command . leftover-args)
   (define leftover-arg-vector (vector->immutable-vector (list->vector leftover-args)))
   (match command
     ["analyze"
      (parameterize ([current-command-line-arguments leftover-arg-vector])
        (resyntax-analyze-run))]
     ["fix"
      (parameterize ([current-command-line-arguments leftover-arg-vector])
        (resyntax-fix-run))])))


(define (resyntax-analyze-run)
  (define options (resyntax-analyze-parse-command-line))
  (define files (file-groups-resolve (resyntax-options-targets options)))
  (match (resyntax-options-output-mode options)
    [(== output-to-console)
     (printf "resyntax: --- analyzing code ---\n")
     (define results
       (transduce files
                  (append-mapping (refactor-file _ #:suite (resyntax-options-suite options)))
                  #:into into-list))
     (printf "resyntax: --- displaying results ---\n")
     (for ([result (in-list results)])
       (define path (file-source-path (refactoring-result-source result)))
       (printf "resyntax: ~a [~a]\n" path (refactoring-result-rule-name result))
       (printf "\n\n~a\n" (string-indent (refactoring-result-message result) #:amount 2))
       (define old-code (refactoring-result-original-code result))
       (define new-code (refactoring-result-new-code result))
       (printf "\n\n~a\n\n\n~a\n\n\n"
               (string-indent (~a old-code) #:amount 2)
               (string-indent (~a new-code) #:amount 2)))]
    [(== output-as-github-review)
     (define results
       (transduce files
                  (append-mapping (refactor-file _ #:suite (resyntax-options-suite options)))
                  #:into into-list))
     (define req (refactoring-results->github-review results #:file-count (length files)))
     (define resp (github-review-request-send req))
     (printf "Response: ~a\n" resp)]))


(define (resyntax-fix-run)
  (define options (resyntax-fix-parse-command-line))
  (define files (file-groups-resolve (resyntax-options-targets options)))
  (printf "resyntax: --- analyzing code ---\n")
  (define all-results
    (transduce files
               (append-mapping (refactor-file _ #:suite (resyntax-options-suite options)))
               #:into into-list))
  (define results-by-path
    (transduce
     all-results
     (indexing (λ (result) (file-source-path (refactoring-result-source result))))
     (grouping (into-transduced (sorting #:key refactoring-result-original-line) #:into into-list))
     #:into into-hash))
  (printf "resyntax: --- fixing code ---\n")
  (for ([(path results) (in-hash results-by-path)])
    (define result-count (length results))
    (define fix-string (if (> result-count 1) "fixes" "fix"))
    (printf "resyntax: applying ~a ~a to ~a\n\n" result-count fix-string path)
    (for ([result (in-list results)])
      (define line (refactoring-result-original-line result))
      (define message (refactoring-result-message result))
      (printf "  * [line ~a] ~a\n" line message))
    (refactor! results)
    (newline))
  (printf "resyntax: --- summary ---\n")
  (define total-fixes (length all-results))
  (define total-files (hash-count results-by-path))
  (define fix-counts-by-rule
    (transduce all-results
               (indexing refactoring-result-rule-name)
               (grouping into-count)
               (sorting #:key entry-value #:descending? #true)
               #:into into-list))
  (printf "\n  Fixed ~a issues in ~a files.\n\n" total-fixes total-files)
  (for ([rule+count (in-list fix-counts-by-rule)])
    (match-define (entry rule count) rule+count)
    (define occurrence-string (if (> count 1) "occurences" "occurence"))
    (printf "  * Fixed ~a ~a of ~a\n" count occurrence-string rule))
  (newline))


(module+ main
  (resyntax-run))
