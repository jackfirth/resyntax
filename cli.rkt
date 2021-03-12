#lang racket/base


(require fancy-app
         racket/cmdline
         racket/format
         racket/match
         racket/path
         racket/string
         rebellion/base/option
         rebellion/collection/entry
         rebellion/collection/hash
         rebellion/collection/list
         rebellion/collection/vector/builder
         rebellion/streaming/reducer
         rebellion/streaming/transducer
         rebellion/type/record
         rebellion/type/tuple
         resyntax
         resyntax/default-recommendations
         resyntax/file-group
         resyntax/refactoring-suite
         resyntax/source)


;@----------------------------------------------------------------------------------------------------


(define-record-type resyntax-options (targets suite))


(define (resyntax-analyze-parse-command-line)
  (define targets (box (make-vector-builder)))
  (define suite (box default-recommendations))
  (define (add-target! target)
    (set-box! targets (vector-builder-add (unbox targets) target)))
  (command-line
   #:program "resyntax analyze"
   #:multi
   ("--file" filepath "A file to anaylze." (add-target! (single-file-group filepath)))
   ("--directory"
    dirpath
    "A directory to anaylze, including subdirectories."
    (add-target! (directory-file-group dirpath)))
   ("--package"
    pkgname
    "An installed package to analyze."
    (add-target! (package-file-group pkgname)))
   #:once-each
   ("--refactoring-suite"
    modpath
    suite-name
    "The refactoring suite to analyze code with."
    (define parsed-modpath (read (open-input-string modpath)))
    (define parsed-suite-name (read (open-input-string suite-name)))
    (set-box! suite (dynamic-require parsed-modpath parsed-suite-name))))
  (resyntax-options #:targets (build-vector (unbox targets)) #:suite (unbox suite)))


(define (resyntax-fix-parse-command-line)
  (define targets (box (make-vector-builder)))
  (define suite (box default-recommendations))
  (define (add-target! target)
    (set-box! targets (vector-builder-add (unbox targets) target)))
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
   #:once-each
   ("--refactoring-suite"
    modpath
    suite-name
    "The refactoring suite to analyze code with."
    (define parsed-modpath (read (open-input-string modpath)))
    (define parsed-suite-name (read (open-input-string suite-name)))
    (set-box! suite (dynamic-require parsed-modpath parsed-suite-name))))
  (resyntax-options #:targets (build-vector (unbox targets)) #:suite (unbox suite)))


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
  (printf "resyntax: --- analyzing code ---\n")
  (define results
    (transduce files
               (append-mapping (refactor-file _ #:suite (resyntax-options-suite options)))
               #:into into-list))
  (printf "resyntax: --- displaying results ---\n")
  (for ([result (in-list results)])
    (define path (file-source-path (refactoring-result-source result)))
    (printf "resyntax: ~a [~a]\n" path (refactoring-result-rule-name result))
    (printf "\n\n~a\n" (string-indent (refactoring-result-message result) 2))
    (define old-code (refactoring-result-original-code result))
    (define new-code (refactoring-result-new-code result))
    (printf "\n\n~a\n\n\n~a\n\n\n" (string-indent (~a old-code) 2) (string-indent (~a new-code) 2))))


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


(define (string-indent s amount)
  (define lines
    (for/list ([line (in-lines (open-input-string s))])
      (string-append (make-string amount #\space) line)))
  (string-join lines "\n"))


(module+ main
  (resyntax-run))
