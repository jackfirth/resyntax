#lang racket/base


(module+ test
  (require racket/string
           rackunit
           resyntax/default-recommendations/analyzers/identifier-usage
           resyntax/default-recommendations/analyzers/variable-mutability
           resyntax/default-recommendations/analyzers/ignored-result-values
           resyntax/default-recommendations/analyzers/function-expression-analyzer
           resyntax/private/analyzer
           resyntax/private/source)

  ;; Load tests for expansion analyzers to ensure they can handle large files
  ;; within reasonable timeouts. These tests are designed to catch severe
  ;; performance regressions where analyzers would take minutes or hang on
  ;; moderately large files.

  (define timeout-ms 30000) ; 30 second timeout for load tests

  ;; Generate a large test file with many definitions and function calls.
  ;; This creates a file with approximately 300+ lines of code with various
  ;; constructs that analyzers need to process. The file is large enough to
  ;; exercise analyzer performance but small enough to complete in reasonable time.
  (define (generate-large-test-program)
    (string-join
     (append
      (list "#lang racket/base"
            ""
            "(require racket/list racket/string)"
            "")
      ;; Generate many function definitions
      (for/list ([i (in-range 25)])
        (format "(define (func~a x y)
  (let ([a (+ x y)]
        [b (* x y)])
    (if (> a b)
        (+ a b)
        (* a b))))" i))
      (list "")
      ;; Generate many variable definitions
      (for/list ([i (in-range 30)])
        (format "(define var~a ~a)" i i))
      (list "")
      ;; Generate many mutable variables with assignments
      (for/list ([i (in-range 20)])
        (format "(define mut~a 0)
(set! mut~a ~a)" i i i))
      (list "")
      ;; Generate function calls with various patterns
      (for/list ([i (in-range 25)])
        (format "(void (func~a var~a (+ var~a ~a)))"
                (modulo i 25)
                (modulo i 30)
                (modulo (+ i 1) 30)
                i))
      (list "")
      ;; Generate nested expressions with ignored results
      (for/list ([i (in-range 15)])
        (format "(begin
  (+ ~a ~a)
  (void ~a))" i (+ i 1) i)))
     "\n"))

  (define large-test-source (string-source (generate-large-test-program)))

  ;; Expand the test source once and reuse for all tests
  (define expanded-syntax
    (parameterize ([current-namespace (make-base-namespace)])
      (source-expand large-test-source)))

  (test-case "identifier-usage-analyzer load test"
    ;; This test ensures the identifier-usage-analyzer can analyze a large file
    ;; within the timeout period.
    (define-values (result cpu-time real-time gc-time)
      (time-apply expansion-analyze (list identifier-usage-analyzer expanded-syntax)))
    
    ;; Verify it completed within the timeout
    (check-true (< real-time timeout-ms)
                (format "identifier-usage-analyzer took ~a ms, expected < ~a ms"
                        real-time
                        timeout-ms)))

  (test-case "variable-mutability-analyzer load test"
    ;; This test ensures the variable-mutability-analyzer can analyze a large file
    ;; within the timeout period.
    (define-values (result cpu-time real-time gc-time)
      (time-apply expansion-analyze (list variable-mutability-analyzer expanded-syntax)))
    
    ;; Verify it completed within the timeout
    (check-true (< real-time timeout-ms)
                (format "variable-mutability-analyzer took ~a ms, expected < ~a ms"
                        real-time
                        timeout-ms)))

  (test-case "ignored-result-values-analyzer load test"
    ;; This test ensures the ignored-result-values-analyzer can analyze a large file
    ;; within the timeout period.
    (define-values (result cpu-time real-time gc-time)
      (time-apply expansion-analyze (list ignored-result-values-analyzer expanded-syntax)))
    
    ;; Verify it completed within the timeout
    (check-true (< real-time timeout-ms)
                (format "ignored-result-values-analyzer took ~a ms, expected < ~a ms"
                        real-time
                        timeout-ms)))

  (test-case "function-expression-analyzer load test"
    ;; This test ensures the function-expression-analyzer can analyze a large file
    ;; within the timeout period.
    (define-values (result cpu-time real-time gc-time)
      (time-apply expansion-analyze (list function-expression-analyzer expanded-syntax)))
    
    ;; Verify it completed within the timeout
    (check-true (< real-time timeout-ms)
                (format "function-expression-analyzer took ~a ms, expected < ~a ms"
                        real-time
                        timeout-ms))))
