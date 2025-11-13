#lang racket/base

(require racket/contract/base
         racket/string
         rackunit
         resyntax/default-recommendations/analyzers/identifier-usage
         resyntax/default-recommendations/analyzers/variable-mutability
         resyntax/default-recommendations/analyzers/ignored-result-values
         resyntax/default-recommendations/analyzers/function-expression-analyzer
         resyntax/private/analysis
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

(test-case "identifier-usage-analyzer load test"
  ;; This test ensures the identifier-usage-analyzer can analyze a large file
  ;; within the timeout period.
  (define start-time (current-inexact-milliseconds))
  (define analysis
    (source-analyze large-test-source
                    #:analyzers (list identifier-usage-analyzer)
                    #:timeout-ms timeout-ms))
  (define elapsed-ms (- (current-inexact-milliseconds) start-time))
  
  ;; Verify the analysis completed successfully
  (check-true (source-code-analysis? analysis))
  
  ;; Verify it completed within the timeout (with some margin)
  (check-true (< elapsed-ms timeout-ms)
              (format "identifier-usage-analyzer took ~a ms, expected < ~a ms"
                      elapsed-ms
                      timeout-ms)))

(test-case "variable-mutability-analyzer load test"
  ;; This test ensures the variable-mutability-analyzer can analyze a large file
  ;; within the timeout period.
  (define start-time (current-inexact-milliseconds))
  (define analysis
    (source-analyze large-test-source
                    #:analyzers (list variable-mutability-analyzer)
                    #:timeout-ms timeout-ms))
  (define elapsed-ms (- (current-inexact-milliseconds) start-time))
  
  ;; Verify the analysis completed successfully
  (check-true (source-code-analysis? analysis))
  
  ;; Verify it completed within the timeout (with some margin)
  (check-true (< elapsed-ms timeout-ms)
              (format "variable-mutability-analyzer took ~a ms, expected < ~a ms"
                      elapsed-ms
                      timeout-ms)))

(test-case "ignored-result-values-analyzer load test"
  ;; This test ensures the ignored-result-values-analyzer can analyze a large file
  ;; within the timeout period.
  (define start-time (current-inexact-milliseconds))
  (define analysis
    (source-analyze large-test-source
                    #:analyzers (list ignored-result-values-analyzer)
                    #:timeout-ms timeout-ms))
  (define elapsed-ms (- (current-inexact-milliseconds) start-time))
  
  ;; Verify the analysis completed successfully
  (check-true (source-code-analysis? analysis))
  
  ;; Verify it completed within the timeout (with some margin)
  (check-true (< elapsed-ms timeout-ms)
              (format "ignored-result-values-analyzer took ~a ms, expected < ~a ms"
                      elapsed-ms
                      timeout-ms)))

(test-case "function-expression-analyzer load test"
  ;; This test ensures the function-expression-analyzer can analyze a large file
  ;; within the timeout period.
  (define start-time (current-inexact-milliseconds))
  (define analysis
    (source-analyze large-test-source
                    #:analyzers (list function-expression-analyzer)
                    #:timeout-ms timeout-ms))
  (define elapsed-ms (- (current-inexact-milliseconds) start-time))
  
  ;; Verify the analysis completed successfully
  (check-true (source-code-analysis? analysis))
  
  ;; Verify it completed within the timeout (with some margin)
  (check-true (< elapsed-ms timeout-ms)
              (format "function-expression-analyzer took ~a ms, expected < ~a ms"
                      elapsed-ms
                      timeout-ms)))

(test-case "all analyzers together load test"
  ;; This test ensures all analyzers can work together on a large file
  ;; within the timeout period.
  (define all-analyzers
    (list identifier-usage-analyzer
          variable-mutability-analyzer
          ignored-result-values-analyzer
          function-expression-analyzer))
  
  (define start-time (current-inexact-milliseconds))
  (define analysis
    (source-analyze large-test-source
                    #:analyzers all-analyzers
                    #:timeout-ms timeout-ms))
  (define elapsed-ms (- (current-inexact-milliseconds) start-time))
  
  ;; Verify the analysis completed successfully
  (check-true (source-code-analysis? analysis))
  
  ;; Verify it completed within the timeout (with some margin)
  (check-true (< elapsed-ms timeout-ms)
              (format "all analyzers together took ~a ms, expected < ~a ms"
                      elapsed-ms
                      timeout-ms)))
