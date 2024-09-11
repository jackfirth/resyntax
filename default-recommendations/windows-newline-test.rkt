#lang racket/base


(module+ test
  (require rackunit
           resyntax/default-recommendations
           resyntax/testing/refactoring-test))


;@----------------------------------------------------------------------------------------------------


(module+ test
  (test-case "windows-style newlines should be replaced with regular newlines"
    (define program
      (string-append "#lang racket/base\r\n"
                     "(define (foo)\r\n"
                     "  (let ([x 42])\r\n"
                     "    (* x 2)))\r\n"))
    (define expected-program
      (string-append "#lang racket/base\n"
                     "(define (foo)\n"
                     "  (define x 42)\n"
                     "  (* x 2))\n"))
    (check-suite-refactors default-recommendations program expected-program)))
