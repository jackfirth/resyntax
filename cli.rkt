#lang racket/base


(require racket/cmdline
         racket/format
         racket/path
         racket/string
         resyntax)


(define (run)
  (for ([result (refactor-directory (current-directory))])
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
