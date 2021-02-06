#lang racket/base


(require racket/cmdline
         racket/format
         racket/match
         racket/path
         racket/string
         rebellion/collection/list
         rebellion/collection/vector/builder
         rebellion/streaming/transducer
         rebellion/type/tuple
         resyntax)


(define-tuple-type file-target (path))
(define-tuple-type directory-target (path))
(define-tuple-type package-target (name))


(define (resyntax-parse-command-line)
  (define targets (box (make-vector-builder)))
  (define (add-target! target)
    (set-box! targets (vector-builder-add (unbox targets) target)))
  (command-line
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


(define (refactor-target target)
  (match target
    [(file-target path) (refactor-file path)]
    [(directory-target path) (refactor-directory path)]
    [(package-target name) (refactor-package name)]))


(define (run)
  (define results
    (transduce (resyntax-parse-command-line)
               (append-mapping refactor-target)
               #:into into-list))
  (for* ([result (in-list results)])
    (define path (find-relative-path (current-directory) (refactoring-result-path result)))
    (printf "resyntax: ~a [~a]\n" path (refactoring-result-rule-name result))
    (printf "\n\n~a\n" (string-indent (refactoring-result-message result) 2))
    (define old-code (refactoring-result-original-code result))
    (define new-code (refactoring-result-new-code result))
    (printf "\n\n~a\n\n\n~a\n\n\n" (string-indent (~a old-code) 2) (string-indent (~a new-code) 2))))


(define (string-indent s amount)
  (define lines
    (for/list ([line (in-lines (open-input-string s))])
      (string-append (make-string amount #\space) line)))
  (string-join lines "\n"))


(module+ main
  (run))


;; Empty test submodule to avoid GUI initialization in CI.
(module test racket/base)
