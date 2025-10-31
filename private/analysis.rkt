#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [source-analyze (->* (source?)
                       (#:lines range-set?
                        #:analyzers (listof expansion-analyzer?))
                       source-code-analysis?)]
  [source-code-analysis? (-> any/c boolean?)]
  [source-code-analysis-code (-> source-code-analysis? source?)]
  [source-code-analysis-visited-forms (-> source-code-analysis? (listof syntax?))]
  [source-code-analysis-expansion-time-output (-> source-code-analysis? immutable-string?)]
  [source-code-analysis-namespace (-> source-code-analysis? namespace?)]
  [source-code-analysis-added-syntax-properties (-> source-code-analysis? syntax-property-bundle?)]))


(require guard
         racket/match
         racket/port
         racket/pretty
         racket/sequence
         racket/stream
         rebellion/base/comparator
         rebellion/base/immutable-string
         rebellion/base/option
         rebellion/base/range
         rebellion/collection/entry
         rebellion/collection/list
         rebellion/collection/range-set
         rebellion/collection/sorted-map
         rebellion/collection/sorted-set
         rebellion/collection/vector/builder
         rebellion/streaming/transducer
         rebellion/type/record
         resyntax/default-recommendations/analyzers/identifier-usage
         resyntax/default-recommendations/analyzers/ignored-result-values
         resyntax/default-recommendations/analyzers/variable-mutability
         resyntax/private/analyzer
         resyntax/private/linemap
         resyntax/private/logger
         resyntax/private/source
         resyntax/private/string-indent
         resyntax/private/syntax-movement
         resyntax/private/syntax-neighbors
         resyntax/private/syntax-path
         resyntax/private/syntax-property-bundle
         resyntax/private/syntax-traversal
         syntax/parse)


;@----------------------------------------------------------------------------------------------------


(define-record-type source-code-analysis
  (code visited-forms expansion-time-output namespace added-syntax-properties))


(define (source-analyze code
                        #:lines [lines (range-set (unbounded-range #:comparator natural<=>))]
                        #:analyzers [analyzers (list identifier-usage-analyzer
                                                     ignored-result-values-analyzer
                                                     variable-mutability-analyzer)])
  (define ns (make-base-namespace))
  (parameterize ([current-directory (or (source-directory code) (current-directory))]
                 [current-namespace ns])
    (define code-linemap (string-linemap (source->string code)))
    (define program-stx (source-read-syntax code))
    (define program-source-name (syntax-source program-stx))
    (unless program-source-name
      (raise-arguments-error
       'source-analyze
       "cannot refactor given source code, the reader returned a syntax object without a source name"
       "source" code
       "reader-produced syntax object" program-stx))
    (log-resyntax-debug "original source name: ~a" program-source-name)
    (log-resyntax-debug "original syntax:\n  ~a" program-stx)
    (define current-expand-observe (dynamic-require ''#%expobs 'current-expand-observe))
    (define original-visits (make-vector-builder))
    (define most-recent-visits-by-original-path (make-hash))

    (define/guard (resyntax-should-analyze-syntax? stx #:as-visit? [as-visit? #true])
      (guard (syntax-original-and-from-source? stx program-source-name) #:else #false)
      (guard as-visit? #:else #true)
      (define stx-lines (syntax-line-range stx #:linemap code-linemap))
      (define overlaps? (range-set-overlaps? lines stx-lines))
      (unless overlaps?
        (log-resyntax-debug
         (string-append "ignoring visited syntax object because it's outside analyzed lines\n"
                        "  analyzed lines: ~a\n"
                        "  syntax lines: ~a\n"
                        "  syntax: ~a")
         lines
         stx-lines
         stx))
      overlaps?)
    
    (define/match (observe-event! sig val)
      [('visit (? syntax? visited))
       (when (resyntax-should-analyze-syntax? visited)
         (vector-builder-add original-visits visited))
       (for ([visit-subform (in-stream (syntax-search-everything visited))]
             #:when (and (resyntax-should-analyze-syntax? visit-subform #:as-visit? #false)
                         (syntax-has-original-path? visit-subform)))
         (define path (syntax-original-path visit-subform))
         (hash-set! most-recent-visits-by-original-path path visit-subform))]
      [(_ _) (void)])

    (define output-port (open-output-string))
    (define expanded
      (parameterize ([current-expand-observe observe-event!]
                     [current-output-port output-port])
        (expand program-stx)))

    ;; We evaluate the module in order to ensure it's declared in the namespace, then we attach it at
    ;; expansion time to ensure the module is visited (but not instantiated). This allows refactoring
    ;; rules to access expansion-time values reflectively via the analysis namespace.
    (eval expanded)
    (namespace-require/expansion-time (extract-module-require-spec expanded))

    (define output (get-output-string output-port))
    (define movement-table (syntax-movement-table expanded))

    (define property-selection-table
      (transduce movement-table
                 (filtering
                  (λ (e)
                    (match-define (entry orig-path exp-paths) e)
                    (match (sorted-set-size exp-paths)
                      [1 #true]
                      [0 #false]
                      [_
                       (log-resyntax-debug
                        (string-append
                         "ignoring expansion analyzer properties for original path ~a because"
                         " multiple expanded forms claim to originate from that path")
                        orig-path)
                       #false])))
                 (mapping-values (λ (exp-paths) (present-value (sorted-set-least-element exp-paths))))
                 #:into (into-sorted-map syntax-path<=>)))

    (define expansion-analyzer-props
      (transduce analyzers
                 (append-mapping
                  (λ (analyzer)
                    (syntax-property-bundle-entries
                     (expansion-analyze analyzer expanded))))
                 #:into into-syntax-property-bundle))

    (define expansion-analyzer-props-adjusted-for-visits
      (transduce property-selection-table
                 (mapping-values
                  (λ (exp-path)
                    (syntax-property-bundle-get-immediate-properties expansion-analyzer-props
                                                                     exp-path)))
                 #:into property-hashes-into-syntax-property-bundle))

    (when (log-level? resyntax-logger 'debug)
      (define props-str
        (string-indent (pretty-format expansion-analyzer-props-adjusted-for-visits) #:amount 2))
      (log-resyntax-debug "syntax properties from expansion analyzers:\n~a" props-str))

    (define (enrich stx #:skip-root? [skip-root? #false])
      (syntax-traverse stx
        #:skip-root? skip-root?
        [child
         #:do [(define child-stx (attribute child))
               (define orig-path (syntax-original-path child-stx))]
         #:when (and orig-path (sorted-map-contains-key? movement-table orig-path))
         #:do [(define expansions
                 (transduce (sorted-map-get movement-table orig-path)
                            (mapping (λ (p) (syntax-ref expanded p)))
                            (filtering syntax-original?)
                            #:into into-list))]
         #:when (equal? (length expansions) 1)
         (match-define (list expanded-child) expansions)
         (log-resyntax-debug "enriching ~a with scopes from expansion" child-stx)
         (enrich (datum->syntax expanded-child (syntax-e child-stx) child-stx child-stx)
                 #:skip-root? #true)]
        [child
         #:do [(define child-stx (attribute child))
               (define orig-path (syntax-original-path child-stx))]
         #:when (and orig-path (hash-has-key? most-recent-visits-by-original-path orig-path))
         #:do [(define visit (hash-ref most-recent-visits-by-original-path orig-path))]
         (log-resyntax-debug "enriching ~a with scopes from visit" child-stx)
         (enrich (datum->syntax visit (syntax-e child-stx) child-stx child-stx) #:skip-root? #true)]
        #:parent-context-modifier values
        #:parent-srcloc-modifier values
        #:parent-props-modifier values))
    
    (define visited
      (transduce (build-vector original-visits)
                 (peeking
                  (λ (visit)
                    (unless (syntax-original-path visit)
                      (raise-arguments-error
                       'source-analyze "pre-enriched visit is missing original path"
                       "visited syntax" visit))))
                 (deduplicating #:key syntax-original-path)
                 (mapping
                  (λ (visit)
                    (define path (syntax-original-path visit))
                    (define visit-props
                      (syntax-property-bundle-get-all-properties
                       expansion-analyzer-props-adjusted-for-visits path))
                    (syntax-add-all-properties visit visit-props)))
                 (mapping enrich)
                 (peeking
                  (λ (visit)
                    (unless (syntax-original-path visit)
                      (raise-arguments-error
                       'source-analyze "post-enriched visit is missing original path"
                       "visited syntax" visit))))
                 (sorting syntax-path<=> #:key syntax-original-path)
                 #:into into-list))

    (log-resyntax-debug "visited ~a forms" (length visited))
    (source-code-analysis #:code code
                          #:visited-forms visited
                          #:expansion-time-output output
                          #:namespace ns
                          #:added-syntax-properties expansion-analyzer-props-adjusted-for-visits)))


(define (syntax-original-and-from-source? stx source-name)
  (and (syntax-original? stx)
       ;; Some macros are able to bend hygiene and syntax properties in such a way that they
       ;; introduce syntax objects into the program that are syntax-original?, but from a
       ;; different file than the one being expanded. So in addition to checking for
       ;; originality, we also check that they come from the same source as the main program
       ;; syntax object. The (open ...) clause of the define-signature macro bends hygiene
       ;; in this way, and is what originally motivated the addition of this check.
       (equal? (syntax-source stx) source-name)))


(define (extract-module-require-spec mod-stx)
  (syntax-parse mod-stx
    [(_ name _ . _) `',(syntax-e #'name)]))


(module+ test
  (require rackunit)

  (test-case "source-analyze with custom analyzers list"
    ;; Test that source-analyze accepts an analyzers parameter
    (define test-source (string-source "#lang racket/base (define x 1)"))
    
    ;; Test with empty analyzers list
    (define analysis-empty (source-analyze test-source #:analyzers '()))
    (check-true (source-code-analysis? analysis-empty))
    
    ;; Test with single analyzer
    (define analysis-single 
      (source-analyze test-source #:analyzers (list identifier-usage-analyzer)))
    (check-true (source-code-analysis? analysis-single))
    
    ;; Test with default analyzers (should match default behavior)
    (define analysis-default
      (source-analyze test-source
                      #:analyzers (list identifier-usage-analyzer
                                        ignored-result-values-analyzer
                                        variable-mutability-analyzer)))
    (check-true (source-code-analysis? analysis-default))))
