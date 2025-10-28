#lang racket/base


(module+ test
  (require rackunit
           resyntax/default-recommendations
           resyntax/test/private/rackunit))


;@----------------------------------------------------------------------------------------------------


(module+ test
  (clear-suites-under-test!)
  (clear-header!)
  (test-case "windows-style newlines should be replaced with regular newlines"
    (parameterize ([current-suite-under-test default-recommendations])
      (define program
        (code-block
         (string-append "#lang racket/base\r\n"
                        "(define (foo)\r\n"
                        "  (let ([x 42])\r\n"
                        "    (* x 2)))\r\n")))
      (define expected-program
        (code-block
         (string-append "#lang racket/base\n"
                        "(define (foo)\n"
                        "  (define x 42)\n"
                        "  (* x 2))\n")))
      (check-suite-refactors program expected-program))))
