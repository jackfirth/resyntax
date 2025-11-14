#lang resyntax/test


require: resyntax/default-recommendations file-io-suggestions


header:
----------------------------------------
#lang racket/base
(require racket/file)
----------------------------------------


test: "original code should be refactorable to new code"
--------------------
(define (f path s)
  (call-with-output-file path
    (lambda (out)
      (displayln s out))))
====================
(define (f path s)
  (display-lines-to-file (list s) path))
--------------------


test: "call-with-output-file with display should refactor to display-to-file"
--------------------
(define (f path s)
  (call-with-output-file path
    (lambda (out)
      (display s out))))
====================
(define (f path s)
  (display-to-file s path))
--------------------


test: "call-with-output-file with displayln and λ should refactor"
--------------------
(define (f path s)
  (call-with-output-file path
    (λ (out)
      (displayln s out))))
====================
(define (f path s)
  (display-lines-to-file (list s) path))
--------------------


test: "call-with-output-file with display and λ should refactor"
--------------------
(define (f path s)
  (call-with-output-file path
    (λ (out)
      (display s out))))
====================
(define (f path s)
  (display-to-file s path))
--------------------


no-change-test: "should not refactor when port parameter has different name"
--------------------
(define (f path s different-port)
  (call-with-output-file path
    (lambda (out)
      (displayln s different-port))))
--------------------


no-change-test: "should not refactor when there are multiple expressions in lambda body"
--------------------
(define (f path s1 s2)
  (call-with-output-file path
    (lambda (out)
      (displayln s1 out)
      (displayln s2 out))))
--------------------


no-change-test:
"should not migrate make-temporary-file without 'directory to make-temporary-directory"
- (make-temporary-file #:copy-from #false)
