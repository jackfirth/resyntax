#lang racket/base


(provide (struct-out code-block)
         current-suite-under-test
         current-header
         current-line-mask
         clear-header!
         set-header!
         clear-suites-under-test!
         add-suite-under-test!
         check-suite-refactors
         check-suite-does-not-refactor
         check-suite-analysis)


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
         resyntax/private/string-indent
         resyntax/private/string-replacement
         resyntax/private/syntax-path
         resyntax/private/syntax-property-bundle
         resyntax/private/syntax-traversal
         syntax/modread
         syntax/parse
         (except-in racket/list range))


;@----------------------------------------------------------------------------------------------------


(define (string-block-info s)
  (string-info (string-hanging-indent s #:amount 2)))


(struct code-block (raw-string) #:transparent
  #:guard (λ (raw-string _) (string->immutable-string raw-string))

  #:methods gen:custom-write

  [(define (write-proc this out mode)
     (define raw (code-block-raw-string this))
     (define-values (_line col _pos) (port-next-location out))
     (cond
       [(and (pretty-printing) col)
        (define lead (make-string col #\space))
        (for ([line (in-lines (open-input-string raw))]
              [i (in-naturals)])
          (unless (zero? i)
            (write-string lead out)
            (if (integer? (pretty-print-columns))
                (pretty-print-newline out (pretty-print-columns))
                (newline out)))
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


(define (clear-suites-under-test!)
  (current-suite-under-test (refactoring-suite #:rules '())))


(define (add-suite-under-test! suite)
  (define current-rules (refactoring-suite-rules (current-suite-under-test)))
  (define new-rules (append current-rules (refactoring-suite-rules suite)))
  (current-suite-under-test (refactoring-suite #:rules new-rules)))


(define current-header (make-parameter (code-block "")))


(define (clear-header!)
  (current-header (code-block "")))


(define (set-header! header-code)
  (unless (equal? (current-header) (code-block ""))
    (raise-arguments-error 'header: "the header has already been set"))
  (current-header header-code))


(define current-line-mask (make-parameter (range-set (unbounded-range #:comparator natural<=>))))


(define (range-bound-add bound amount)
  (if (unbounded? bound)
      unbounded
      (range-bound (+ (range-bound-endpoint bound) amount) (range-bound-type bound))))


;; Helper function to create logging utilities
(define (make-log-capture-utilities)
  (define logged-messages-builder (make-vector-builder))
  
  (define (save-log log-entry)
    (vector-builder-add logged-messages-builder (vector-ref log-entry 1)))

  (define (call-with-logs-captured proc)
    (with-intercepted-logging save-log #:logger resyntax-logger proc 'debug 'resyntax))

  (define (build-logs-info)
    (string-block-info (string-join (vector->list (build-vector logged-messages-builder)) "\n")))
  
  (values call-with-logs-captured build-logs-info))

;; Helper function to compute the modified line mask based on header
(define (compute-modified-line-mask header-line-count)
  (for/range-set #:comparator natural<=>
    ([r (in-range-set (current-line-mask))])
    (range (range-bound-add (range-lower-bound r) header-line-count)
           (range-bound-add (range-upper-bound r) header-line-count)
           #:comparator natural<=>)))

;; Helper function to create check-info list for matched rules
(define (make-matched-rules-check-info result-set)
  (if (empty? (refactoring-result-set-results result-set))
      '()
      (list (check-info 'matched-rules (refactoring-result-set-matched-rules-info result-set)))))


(define-check (check-suite-refactors original-program expected-program)
  (define suite (current-suite-under-test))
  (set! original-program (code-block-append (current-header) original-program))
  (set! expected-program (code-block-append (current-header) expected-program))
  (define header-line-count
    (count (λ (ch) (equal? ch #\newline)) (string->list (code-block-raw-string (current-header)))))
  (define modified-line-mask (compute-modified-line-mask header-line-count))
  (define-values (call-with-logs-captured build-logs-info) (make-log-capture-utilities))

  (define result-set
    (call-with-logs-captured
     (λ ()
       (resyntax-analyze (string-source (code-block-raw-string original-program))
                         #:suite suite
                         #:lines modified-line-mask))))
  
  (with-check-info* (make-matched-rules-check-info result-set)
    (λ ()
      (define refactored-program
        (with-handlers
            ([exn:fail?
              (λ (e)
                (with-check-info
                    (['logs (build-logs-info)]
                     ['original (string-block-info (code-block-raw-string original-program))]
                     ['expected (string-block-info expected-program)]
                     ['exception e])
                  (fail-check "an error occurred while processing refactoring results")))])
          (call-with-logs-captured
           (λ () (modified-source-contents (refactoring-result-set-updated-source result-set))))))
      (with-check-info (['logs (build-logs-info)]
                        ['actual (string-block-info refactored-program)]
                        ['expected (string-block-info (code-block-raw-string expected-program))])
        (when (empty? (refactoring-result-set-results result-set))
          (fail-check "no changes were made"))
        (when (equal? refactored-program (code-block-raw-string original-program))
          (fail-check "fixes were made, but they left the program unchanged"))
        (unless (equal? refactored-program (code-block-raw-string expected-program))
          (with-check-info (['original (string-block-info (code-block-raw-string original-program))])
            (fail-check "incorrect changes were made"))))
      (match-define (program-output original-stdout original-stderr)
        (eval-program (code-block-raw-string original-program)))
      (match-define (program-output actual-stdout actual-stderr) (eval-program refactored-program))
      (unless (equal? original-stdout actual-stdout)
        (with-check-info (['logs (build-logs-info)]
                          ['actual (string-block-info actual-stdout)]
                          ['original (string-block-info original-stdout)])
          (fail-check "output to stdout changed")))
      (unless (equal? original-stderr actual-stderr)
        (with-check-info (['logs (build-logs-info)]
                          ['actual (string-block-info actual-stderr)]
                          ['original (string-block-info original-stderr)])
          (fail-check "output to stderr changed"))))))


(define-check (check-suite-does-not-refactor original-program)
  (define suite (current-suite-under-test))
  (set! original-program (code-block-append (current-header) original-program))
  (define-values (call-with-logs-captured build-logs-info) (make-log-capture-utilities))

  (define result-set
    (call-with-logs-captured
     (λ ()
       (resyntax-analyze (string-source (code-block-raw-string original-program)) #:suite suite))))
  (define refactored-program
    (modified-source-contents (refactoring-result-set-updated-source result-set)))
  (with-check-info* (make-matched-rules-check-info result-set)
    (λ ()
      (with-check-info (['logs (build-logs-info)]
                        ['actual (string-block-info refactored-program)]
                        ['original (string-block-info (code-block-raw-string original-program))])
        (unless (equal? refactored-program (code-block-raw-string original-program))
          (fail-check "expected no changes, but changes were made")))
      (with-check-info (['logs (build-logs-info)]
                        ['actual (string-block-info refactored-program)])
        (unless (empty? (refactoring-result-set-results result-set))
          (fail-check "the program was not changed, but no-op fixes were suggested"))))))


(define-check (check-suite-analysis program context-list target property-key expected-value)
  (define suite (current-suite-under-test))
  (set! program (code-block-append (current-header) program))
  (define program-src (string-source (code-block-raw-string program)))
  (define-values (call-with-logs-captured build-logs-info) (make-log-capture-utilities))

  (define actual-props
    (call-with-logs-captured
     (λ () (reysntax-analyze-for-properties-only program-src))))

  (define target-src (string-source (string-trim (code-block-raw-string target))))
  (define context-src-list
    (for/list ([ctx (in-list context-list)])
      (string-source (string-trim (code-block-raw-string ctx)))))

  (define target-path (source-find-path-of program-src target-src #:contexts context-src-list))

  (unless target-path
    (with-check-info (['logs (build-logs-info)]
                      ['program (string-block-info (string-source-contents program-src))]
                      ['target (string-block-info (string-source-contents target-src))])
      (fail-check "could not locate target subform within the given program")))

  (define (fail-property-lookup)
    (define target-properties
      (syntax-property-bundle-get-immediate-properties actual-props target-path))
    (with-check-info (['logs (build-logs-info)]
                      ['program (string-block-info (string-source-contents program-src))]
                      ['target (string-block-info (string-source-contents target-src))]
                      ['target-path target-path]
                      ['target-properties target-properties]
                      ['property-key property-key])
      (fail-check "analysis did not assign a value for the given syntax property key")))

  (define actual-value
    (syntax-property-bundle-get-property actual-props target-path property-key fail-property-lookup))

  (unless (equal? actual-value expected-value)
    (with-check-info (['logs (build-logs-info)]
                      ['program (string-block-info (string-source-contents program-src))]
                      ['target (string-block-info (string-source-contents target-src))]
                      ['target-path target-path]
                      ['property-key property-key]
                      ['actual actual-value]
                      ['expected expected-value])
      (fail-check "analysis assigned an incorrect value for the given syntax property key"))))


(define (source-find-path-of src target-src #:contexts [context-srcs '()])
  (define stx (syntax-label-paths (source-read-syntax src) 'source-path))
  (define target-as-string (string-source-contents target-src))

  (define target-stx
    (let loop ([stx stx] [context-srcs context-srcs])
      (match context-srcs
        ['()
         (syntax-find-first stx subform
            #:when (equal? (source-text-of src (attribute subform)) target-as-string))]
        [(cons next-context remaining-contexts)
         (define next-as-string (string-source-contents next-context))
         (define substx
           (syntax-find-first stx subform
             #:when (equal? (source-text-of src (attribute subform)) next-as-string)))
         (and substx (loop substx remaining-contexts))])))
                  
  (and target-stx (syntax-property target-stx 'source-path)))


(module+ test
  (test-case "source-find-path-of"

    (test-case "no #lang"
      (define src (string-source "(+ a b c)"))
      (define target (string-source "b"))
      (check-equal? (source-find-path-of src target) (syntax-path (list 2))))

    (test-case "simple #lang"
      (define src (string-source "#lang racket (define a 1)"))
      (define target (string-source "a"))
      (check-equal? (source-find-path-of src target) (syntax-path (list 3 1 1))))

    (test-case "single context"
      (define src (string-source "(list (+ a) (* a))"))
      (define target (string-source "a"))
      (define contexts (list (string-source "(* a)")))
      (check-equal? (source-find-path-of src target #:contexts contexts) (syntax-path (list 2 1))))

    (test-case "multiple contexts"
      (define src (string-source "(+ a (+ a (+ a (+ a))))"))
      (define target (string-source "a"))
      (define contexts
        (list (string-source "(+ a (+ a (+ a)))")
              (string-source "(+ a (+ a))")
              (string-source "(+ a)")))

      (define actual-path (source-find-path-of src target #:contexts contexts))

      (check-equal? actual-path (syntax-path (list 2 2 2 1))))))


(define (refactoring-result-set-matched-rules-info result-set)
  (define matches
    (transduce (refactoring-result-set-results result-set)
               (mapping refactoring-result-rule-name)
               #:into into-multiset))
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
