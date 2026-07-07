#lang resyntax/test


require: resyntax/default-recommendations file-io-suggestions


header:
----------------------------------------
#lang racket/base
(require racket/file)
----------------------------------------


test: "call-with-output-file displaying a value refactorable to display-to-file"
----------------------------------------
(define (f path s)
  (call-with-output-file path
    (lambda (out)
      (display s out))))
========================================
(define (f path s)
  (call-with-output-file path
    (λ (out)
      (display s out))))
========================================
(define (f path s)
  (display-to-file s path))
----------------------------------------


test: "call-with-output-file displaying a line refactorable to display-lines-to-file"
----------------------------------------
(define (f path s)
  (call-with-output-file path
    (lambda (out)
      (displayln s out))))
========================================
(define (f path s)
  (display-lines-to-file (list s) path))
----------------------------------------


test: "call-with-output-file displaying multiple lines refactorable to display-lines-to-file"
----------------------------------------
(define (f path a b)
  (call-with-output-file path
    (lambda (out)
      (displayln a out)
      (displayln b out))))
========================================
(define (f path a b)
  (display-lines-to-file (list a b) path))
----------------------------------------


no-change-test: "call-with-output-file with keyword arguments not refactorable"
----------------------------------------
(define (f path s)
  (call-with-output-file path
    (lambda (out)
      (displayln s out))
    #:exists 'replace))
----------------------------------------


no-change-test: "call-with-output-file displaying to a different port not refactorable"
----------------------------------------
(define (f path s other-port)
  (call-with-output-file path
    (lambda (out)
      (displayln s other-port))))
----------------------------------------


no-change-test: "call-with-output-file with displayed value depending on the port not refactorable"
----------------------------------------
(define (f path)
  (call-with-output-file path
    (lambda (out)
      (displayln (object-name out) out))))
----------------------------------------


no-change-test: "call-with-output-file with non-display body forms not refactorable"
----------------------------------------
(define (f path s)
  (call-with-output-file path
    (lambda (out)
      (displayln s out)
      (flush-output out))))
----------------------------------------


no-change-test:
"should not migrate make-temporary-file without 'directory to make-temporary-directory"
- (make-temporary-file #:copy-from #false)
