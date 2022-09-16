#lang racket/base


(provide (for-syntax #%datum)
         #%datum
         #%module-begin
         refactoring-test
         refactoring-test-import
         refactoring-test-header
         refactoring-test-case)


(require (for-syntax racket/base
                     racket/sequence)
         racket/list
         racket/match
         racket/port
         racket/pretty
         racket/stxparam
         rackunit
         rackunit/private/check-info
         rebellion/collection/entry
         rebellion/collection/hash
         rebellion/collection/list
         rebellion/collection/multiset
         rebellion/streaming/transducer
         rebellion/type/tuple
         resyntax
         resyntax/private/refactoring-result
         resyntax/refactoring-suite
         resyntax/private/string-replacement
         syntax/parse
         syntax/parse/define
         syntax/modread)


;@----------------------------------------------------------------------------------------------------


(begin-for-syntax
  (define-syntax-class refactoring-test-import-statement
    #:attributes (require-statement suite)
    #:literals (refactoring-test-import)
    (pattern (refactoring-test-import module suite)
      #:with require-statement #'(require (only-in module suite))))

  (define-syntax-class refactoring-test-header-statement
    #:attributes (header-block)
    #:literals (refactoring-test-header)
    (pattern (refactoring-test-header header-block)))

  (define (make-constant-transformer constant)
    (syntax-parser [:id #`#,constant])))


(define-simple-macro
  (refactoring-test
   import:refactoring-test-import-statement ...
   (~optional header:refactoring-test-header-statement)
   case ...)
  (begin
    import.require-statement ...
    (define suite
      (refactoring-suite #:rules (append (refactoring-suite-rules import.suite) ...)))
    (syntax-parameterize
        ([refactoring-suite-under-test (make-rename-transformer #'suite)]
         (~? (~@ [implicit-program-header (make-constant-transformer header.header-block)])))
      case ...
      ;; this void expression ensures that it's not an error if no test cases are given
      (void))))


(define-syntax (refactoring-test-import stx)
  (raise-syntax-error #false "can only be used within a refactoring test" stx))


(define-syntax (refactoring-test-header stx)
  (raise-syntax-error #false "can only be used within a refactoring test" stx))


(define (refactoring-results-matched-rules-info results)
  (define matches (transduce results (mapping refactoring-result-rule-name) #:into into-multiset))
  (nested-info
   (transduce (in-hash-entries (multiset-frequencies matches))
              (mapping-values
               (λ (match-count)
                 (string-info (format "~a match~a" match-count (if (= match-count 1) "" "es")))))
              (mapping (λ (e) (check-info (entry-key e) (entry-value e))))
              #:into into-list)))


(define-tuple-type program-output (stdout stderr))


(define (eval-program program)
  (define stdout (open-output-string))
  (define stderr (open-output-string))
  (parameterize ([current-namespace (make-base-namespace)])
    (define (read-from-input)
      (port-count-lines! (current-input-port))
      (with-module-reading-parameterization read-syntax))
    (define stx (with-input-from-string program read-from-input))
    (define module-name
      (syntax-parse stx #:datum-literals (module) [(module name:id _ ...) (syntax-e #'name)]))
    (parameterize ([current-output-port stdout]
                   [current-error-port stderr])
      (eval stx)
      (dynamic-require `',module-name #false)))
  (program-output
   (string->immutable-string (get-output-string stdout))
   (string->immutable-string (get-output-string stderr))))


(module+ test
  (test-case "eval-program"
    (check-equal? (eval-program "#lang racket/base (or 1 2 3)") (program-output "1\n" ""))))


(define-check (check-suite-refactors suite original-program expected-program)
  (define results (refactor original-program #:suite suite))
  (define replacement
    (transduce results
               (mapping refactoring-result-string-replacement)
               #:into union-into-string-replacement))
  (define refactored-program (string-apply-replacement original-program replacement))
  (with-check-info*
      (if (empty? results)
          '()
          (list (check-info 'matched-rules (refactoring-results-matched-rules-info results))))
    (λ ()
      (with-check-info (['actual (string-block refactored-program)]
                        ['expected (string-block expected-program)])
        (when (empty? results)
          (fail-check "no changes were made"))
        (when (equal? refactored-program original-program)
          (fail-check "fixes were made, but they left the program unchanged"))
        (when (not (equal? refactored-program expected-program))
          (with-check-info (['original (string-block original-program)])
            (fail-check "incorrect changes were made"))))
      (match-define (program-output original-stdout original-stderr) (eval-program original-program))
      (match-define (program-output actual-stdout actual-stderr) (eval-program refactored-program))
      (unless (equal? original-stdout actual-stdout)
        (with-check-info (['actual (string-block actual-stdout)]
                          ['original (string-block original-stdout)])
          (fail-check "output to stdout changed")))
      (unless (equal? original-stderr actual-stderr)
        (with-check-info (['actual (string-block actual-stderr)]
                          ['original (string-block original-stderr)])
          (fail-check "output to stderr changed"))))))


(define-check (check-suite-does-not-refactor suite original-program)
  (define results (refactor original-program #:suite suite))
  (define replacement
    (transduce results
               (mapping refactoring-result-string-replacement)
               #:into union-into-string-replacement))
  (define refactored-program (string-apply-replacement original-program replacement))
  (with-check-info*
      (if (empty? results)
          '()
          (list (check-info 'matched-rules (refactoring-results-matched-rules-info results))))
    (λ ()
      (with-check-info (['actual (string-block refactored-program)]
                        ['original (string-block original-program)])
        (unless (equal? refactored-program original-program)
          (fail-check "expected no changes, but changes were made")))
      (with-check-info (['actual (string-block refactored-program)])
        (unless (empty? results)
          (fail-check "the program was not changed, but no-op fixes were suggested"))))))


(define-syntax (refactoring-test-case stx)
  (define (add-header input-stx)
    #`(string-append implicit-program-header #,input-stx))
  (syntax-parse stx
    [(_ name:str input:str)
     #:cut
     #`(test-case name
         #,(quasisyntax/loc this-syntax
             (check-suite-does-not-refactor refactoring-suite-under-test #,(add-header #'input))))]
    [(_ name:str input:str expected:str)
     #:cut
     #`(test-case name
         #,(quasisyntax/loc this-syntax
             (check-suite-refactors
              refactoring-suite-under-test #,(add-header #'input) #,(add-header #'expected))))]
    [(_ name:str input:str ...+ expected:str)
     #:cut
     #:with expected-with-header (add-header #'expected)
     #`(test-case name
         #,@(for/list ([input-stx (in-syntax #'(input ...))])
              (quasisyntax/loc input-stx
                (check-suite-refactors
                 refactoring-suite-under-test #,(add-header input-stx) expected-with-header))))]))


(define-syntax-parameter refactoring-suite-under-test
  (λ (stx) (raise-syntax-error #false "can only be used within a refactoring test case" stx)))


(define-syntax-parameter implicit-program-header
  (syntax-parser #:literals (implicit-program-header) [implicit-program-header #'""]))


(struct string-block (raw-string) #:transparent
  #:guard (λ (raw-string _) (string->immutable-string raw-string))

  #:methods gen:custom-write

  [(define (write-proc this out mode)
     (define raw (string-block-raw-string this))
     (define-values (_line col _pos) (port-next-location out))
     (cond
       [(and (pretty-printing) (integer? (pretty-print-columns)) col)
        (define lead (make-string col #\space))
        (for ([line (in-lines (open-input-string raw))]
              [i (in-naturals)])
          (unless (zero? i)
            (write-string lead out)
            (pretty-print-newline out (pretty-print-columns)))
          (write-string line out))]
       [else
        (for ([line (in-lines (open-input-string raw))]
              [i (in-naturals)])
          (unless (zero? i)
            (newline out))
          (write-string line out))]))])


;@----------------------------------------------------------------------------------------------------


(module reader racket/base


  (require racket/contract/base)


  (provide
   (contract-out
    [read procedure?]
    [read-syntax procedure?]))


  (require resyntax/testing/refactoring-test-parser
           resyntax/testing/refactoring-test-tokenizer)
  

  ;@--------------------------------------------------------------------------------------------------


  (define (read in)
    (read-using-syntax-reader read-syntax in))


  (define (read-syntax source-name in)
    (define parse-tree (parse source-name (make-refactoring-test-tokenizer in)))
    (define module-datum
      `(module refactoring-test racket/base
         (module test resyntax/testing/refactoring-test
           ,parse-tree)))
    (datum->syntax #f module-datum))


  (define (read-using-syntax-reader syntax-reader in)
    (syntax->datum (syntax-reader #false in))))
