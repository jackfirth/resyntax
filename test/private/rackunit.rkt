#lang racket/base


(provide (struct-out code-block)
         current-suite-under-test
         current-header
         current-line-mask
         set-header!
         add-suite-under-test!
         check-suite-refactors
         check-suite-does-not-refactor)


(require racket/logging
         racket/match
         racket/port
         racket/pretty
         racket/string
         rackunit
         rebellion/base/comparator
         rebellion/base/range
         rebellion/collection/entry
         rebellion/collection/hash
         rebellion/collection/list
         rebellion/collection/multiset
         rebellion/collection/range-set
         rebellion/collection/vector/builder
         rebellion/streaming/transducer
         rebellion/type/tuple
         resyntax
         resyntax/base
         resyntax/private/logger
         resyntax/private/refactoring-result
         resyntax/private/source
         resyntax/private/string-replacement
         syntax/modread
         syntax/parse
         (except-in racket/list range))


;@----------------------------------------------------------------------------------------------------


(struct code-block (raw-string) #:transparent
  #:guard (λ (raw-string _) (string->immutable-string raw-string))

  #:methods gen:custom-write

  [(define (write-proc this out mode)
     (define raw (code-block-raw-string this))
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


(define (code-block-append block1 block2)
  (code-block (string-append (code-block-raw-string block1) (code-block-raw-string block2))))


(define current-suite-under-test (make-parameter (refactoring-suite #:rules '())))


(define (add-suite-under-test! suite)
  (define current-rules (refactoring-suite-rules (current-suite-under-test)))
  (define new-rules (append current-rules (refactoring-suite-rules suite)))
  (current-suite-under-test (refactoring-suite #:rules new-rules)))


(define current-header (make-parameter (code-block "")))


(define (set-header! header-code)
  (unless (equal? (current-header) (code-block ""))
    (raise-arguments-error 'header: "the header has already been set"))
  (current-header header-code))


(define current-line-mask (make-parameter (range-set (unbounded-range #:comparator natural<=>))))


(define (range-bound-add bound amount)
  (if (unbounded? bound)
      unbounded
      (range-bound (+ (range-bound-endpoint bound) amount) (range-bound-type bound))))


(define-check (check-suite-refactors original-program expected-program)
  (define suite (current-suite-under-test))
  (set! original-program (code-block-append (current-header) original-program))
  (set! expected-program (code-block-append (current-header) expected-program))
  (define header-line-count
    (count (λ (ch) (equal? ch #\newline)) (string->list (code-block-raw-string (current-header)))))
  (define modified-line-mask
    (for/range-set #:comparator natural<=>
      ([r (in-range-set (current-line-mask))])
      (range (range-bound-add (range-lower-bound r) header-line-count)
             (range-bound-add (range-upper-bound r) header-line-count)
             #:comparator natural<=>)))
  (define logged-messages-builder (make-vector-builder))

  (define (save-log log-entry)
    (vector-builder-add logged-messages-builder (vector-ref log-entry 1)))

  (define (call-with-logs-captured proc)
    (with-intercepted-logging save-log #:logger resyntax-logger proc 'debug 'resyntax))

  (define (build-logs-info)
    (string-info (string-join (vector->list (build-vector logged-messages-builder)) "\n")))

  (define results
    (call-with-logs-captured
     (λ ()
       (resyntax-analyze (string-source (code-block-raw-string original-program))
                         #:suite suite
                         #:lines modified-line-mask))))
  
  (with-check-info*
      (if (empty? results)
          '()
          (list (check-info 'matched-rules (refactoring-results-matched-rules-info results))))
    (λ ()
      (define replacement
        (with-handlers
            ([exn:fail?
              (λ (e)
                (with-check-info (['logs (build-logs-info)]
                                  ['original original-program]
                                  ['expected expected-program]
                                  ['exception e])
                  (fail-check "an error occurred while processing refactoring results")))])
          (call-with-logs-captured
           (λ () (transduce results
                            (mapping refactoring-result-string-replacement)
                            #:into union-into-string-replacement)))))
      (define refactored-program
        (string-apply-replacement (source->string
                                   (string-source (code-block-raw-string original-program)))
                                  replacement))
      (with-check-info (['logs (build-logs-info)]
                        ['actual (code-block refactored-program)]
                        ['expected expected-program])
        (when (empty? results)
          (fail-check "no changes were made"))
        (when (equal? refactored-program (code-block-raw-string original-program))
          (fail-check "fixes were made, but they left the program unchanged"))
        (unless (equal? refactored-program (code-block-raw-string expected-program))
          (with-check-info (['original original-program])
            (fail-check "incorrect changes were made"))))
      (match-define (program-output original-stdout original-stderr)
        (eval-program (code-block-raw-string original-program)))
      (match-define (program-output actual-stdout actual-stderr) (eval-program refactored-program))
      (unless (equal? original-stdout actual-stdout)
        (with-check-info (['logs (build-logs-info)]
                          ['actual (code-block actual-stdout)]
                          ['original (code-block original-stdout)])
          (fail-check "output to stdout changed")))
      (unless (equal? original-stderr actual-stderr)
        (with-check-info (['logs (build-logs-info)]
                          ['actual (code-block actual-stderr)]
                          ['original (code-block original-stderr)])
          (fail-check "output to stderr changed"))))))


(define-check (check-suite-does-not-refactor original-program)
  (define suite (current-suite-under-test))
  (set! original-program (code-block-append (current-header) original-program))
  (define logged-messages-builder (make-vector-builder))

  (define (save-log log-entry)
    (vector-builder-add logged-messages-builder (vector-ref log-entry 1)))

  (define (call-with-logs-captured proc)
    (with-intercepted-logging save-log #:logger resyntax-logger proc 'debug 'resyntax))

  (define (build-logs-info)
    (string-info (string-join (vector->list (build-vector logged-messages-builder)) "\n")))

  (define results
    (call-with-logs-captured
     (λ ()
       (resyntax-analyze (string-source (code-block-raw-string original-program)) #:suite suite))))
  (define replacement
    (transduce results
               (mapping refactoring-result-string-replacement)
               #:into union-into-string-replacement))
  (define refactored-program
    (string-apply-replacement (code-block-raw-string original-program) replacement))
  (with-check-info*
      (if (empty? results)
          '()
          (list (check-info 'matched-rules (refactoring-results-matched-rules-info results))))
    (λ ()
      (with-check-info (['logs (build-logs-info)]
                        ['actual (code-block refactored-program)]
                        ['original original-program])
        (unless (equal? refactored-program (code-block-raw-string original-program))
          (fail-check "expected no changes, but changes were made")))
      (with-check-info (['logs (build-logs-info)]
                        ['actual (code-block refactored-program)])
        (unless (empty? results)
          (fail-check "the program was not changed, but no-op fixes were suggested"))))))


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
