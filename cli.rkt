#lang racket/base


(require racket/cmdline
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
         rebellion/type/tuple
         resyntax
         resyntax/source-code)


;@----------------------------------------------------------------------------------------------------


(define-tuple-type file-target (path))
(define-tuple-type directory-target (path))
(define-tuple-type package-target (name))


(define (resyntax-analyze-parse-command-line)
  (define targets (box (make-vector-builder)))
  (define (add-target! target)
    (set-box! targets (vector-builder-add (unbox targets) target)))
  (command-line
   #:program "resyntax analyze"
   #:multi
   ("--file"
    filepath
    "A file to anaylze."
    (define complete-filepath (simplify-path (path->complete-path filepath)))
    (add-target! (file-target complete-filepath)))
   ("--directory"
    dirpath
    "A directory to anaylze, including subdirectories."
    (define complete-dirpath (simplify-path (path->complete-path dirpath)))
    (add-target! (directory-target complete-dirpath)))
   ("--package"
    pkgname
    "A package to analyze."
    (add-target! (package-target pkgname))))
  (build-vector (unbox targets)))


(define (resyntax-fix-parse-command-line)
  (define targets (box (make-vector-builder)))
  (define (add-target! target)
    (set-box! targets (vector-builder-add (unbox targets) target)))
  (command-line
   #:program "resyntax fix"
   #:multi
   ("--file"
    filepath
    "A file to fix."
    (define complete-filepath (simplify-path (path->complete-path filepath)))
    (add-target! (file-target complete-filepath)))
   ("--directory"
    dirpath
    "A directory to fix, including subdirectories."
    (define complete-dirpath (simplify-path (path->complete-path dirpath)))
    (add-target! (directory-target complete-dirpath)))
   ("--package"
    pkgname
    "A package to fix."
    (add-target! (package-target pkgname))))
  (build-vector (unbox targets)))


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


(define (refactor-target target)
  (match target
    [(file-target path) (refactor-file path)]
    [(directory-target path) (refactor-directory path)]
    [(package-target name) (refactor-package name)]))


(define (resyntax-analyze-run)
  (printf "resyntax: --- analyzing code ---\n")
  (define results
    (transduce (resyntax-analyze-parse-command-line)
               (append-mapping refactor-target)
               #:into into-list))
  (printf "resyntax: --- displaying results ---\n")
  (for ([result (in-list results)])
    (define path (file-source-code-path (refactoring-result-source result)))
    (printf "resyntax: ~a [~a]\n" path (refactoring-result-rule-name result))
    (printf "\n\n~a\n" (string-indent (refactoring-result-message result) 2))
    (define old-code (refactoring-result-original-code result))
    (define new-code (refactoring-result-new-code result))
    (printf "\n\n~a\n\n\n~a\n\n\n" (string-indent (~a old-code) 2) (string-indent (~a new-code) 2))))


(define (resyntax-fix-run)
  (printf "resyntax: --- analyzing code ---\n")
  (define all-results
    (transduce
     (resyntax-fix-parse-command-line)
     (append-mapping refactor-target)
     #:into into-list))
  (define results-by-path
    (transduce
     all-results
     (indexing (λ (result) (file-source-code-path (refactoring-result-source result))))
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
               #:into into-hash))
  (printf "\n  Fixed ~a issues in ~a files.\n\n" total-fixes total-files)
  (for ([(rule count) (in-hash fix-counts-by-rule)])
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


;; Empty test submodule to avoid GUI initialization in CI.
(module test racket/base)
