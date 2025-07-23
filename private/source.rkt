#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [source? (-> any/c boolean?)]
  [unmodified-source? (-> any/c boolean?)]
  [source->string (-> source? immutable-string?)]
  [source-path (-> source? (or/c path? #false))]
  [source-directory (-> source? (or/c path? #false))]
  [source-original (-> source? unmodified-source?)]
  [source-read-syntax (-> source? syntax?)]
  [source-analyze (->* (source?) (#:lines range-set?) source-code-analysis?)]
  [file-source? (-> any/c boolean?)]
  [file-source (-> path-string? file-source?)]
  [file-source-path (-> file-source? path?)]
  [string-source? (-> any/c boolean?)]
  [string-source (-> string? string-source?)]
  [string-source-contents (-> string-source? immutable-string?)]
  [modified-source? (-> any/c boolean?)]
  [modified-source (-> unmodified-source? string? modified-source?)]
  [modified-source-contents (-> modified-source? immutable-string?)]
  [modified-source-original (-> modified-source? unmodified-source?)]
  [source-code-analysis? (-> any/c boolean?)]
  [source-code-analysis-code (-> source-code-analysis? source?)]
  [source-code-analysis-visited-forms (-> source-code-analysis? (listof syntax?))]
  [source-code-analysis-expansion-time-output (-> source-code-analysis? immutable-string?)]
  [source-code-analysis-namespace (-> source-code-analysis? namespace?)]
  [with-input-from-source (-> source? (-> any) any)]))


(require guard
         racket/match
         racket/path
         racket/port
         racket/stream
         rebellion/base/comparator
         rebellion/base/immutable-string
         rebellion/base/option
         rebellion/base/range
         rebellion/collection/list
         rebellion/collection/range-set
         rebellion/collection/sorted-map
         rebellion/collection/vector/builder
         rebellion/streaming/reducer
         rebellion/streaming/transducer
         rebellion/type/record
         resyntax/private/fully-expanded-syntax
         resyntax/private/linemap
         resyntax/private/logger
         resyntax/private/syntax-movement
         resyntax/private/syntax-neighbors
         resyntax/private/syntax-path
         resyntax/private/syntax-traversal
         syntax/id-table
         syntax/modread
         syntax/parse)


;@----------------------------------------------------------------------------------------------------


(struct source () #:transparent)


(struct unmodified-source source () #:transparent)


(struct file-source unmodified-source (path)
  #:transparent
  #:guard (λ (path _) (simple-form-path path)))


(struct string-source unmodified-source (contents)
  #:transparent
  #:guard (λ (contents _) (string->immutable-string contents)))


(struct modified-source source (original contents)
  #:transparent
  #:guard (λ (original contents _) (values original (string->immutable-string contents))))


(define-record-type source-code-analysis (code visited-forms expansion-time-output namespace))


(define (with-input-from-source code proc)

  (define (call-proc-with-reencoded-input in)
    (define reencoded-in (reencode-input-port in "UTF-8" #false #false (object-name in) #true))
    (parameterize ([current-input-port reencoded-in])
      (proc)))

  (match code
    [(file-source path) (call-with-input-file path call-proc-with-reencoded-input)]
    [(string-source contents) (call-proc-with-reencoded-input (open-input-string contents))]
    [(modified-source (file-source path) contents)
     (call-proc-with-reencoded-input (open-input-string contents path))]
    [(modified-source (? string-source?) contents)
     (call-proc-with-reencoded-input (open-input-string contents))]))


(define (source->string code)
  (string->immutable-string (with-input-from-source code port->string)))


(define (source-read-syntax code)
  (define (read-from-input)
    (port-count-lines! (current-input-port))
    (with-module-reading-parameterization read-syntax))
  (syntax-label-original-paths (with-input-from-source code read-from-input)))


(define/guard (source-path code)
  (guard-match (or (file-source path) (modified-source (file-source path) _)) code #:else #false)
  path)


(define/guard (source-directory code)
  (define path (source-path code))
  (and path (path-only path)))


(define (source-original code)
  (if (unmodified-source? code)
      code
      (modified-source-original code)))


(define (source-analyze code #:lines [lines (range-set (unbounded-range #:comparator natural<=>))])
  (define ns (make-base-namespace))
  (parameterize ([current-directory (or (source-directory code) (current-directory))]
                 [current-namespace ns])
    (define code-linemap (string-linemap (source->string code)))
    (define program-stx (source-read-syntax code))
    (log-resyntax-debug "original syntax:\n  ~a" program-stx)
    (define program-source-name (syntax-source program-stx))
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
    (define binding-table (fully-expanded-syntax-binding-table expanded))
    (define original-binding-table-by-path
      (for*/fold ([table (hash)])
                 ([phase-table (in-hash-values binding-table)]
                  [(id uses) (in-free-id-table phase-table)]
                  #:when (syntax-original-and-from-source? id program-source-name)
                  [use (in-list uses)])
        (hash-update table (syntax-original-path id) (λ (previous) (cons use previous)) '())))

    (define expanded-with-properties
      (syntax-traverse expanded
        [id:id
         #:do [(define path (syntax-original-path (attribute id)))]
         #:when path
         (define usages (hash-ref original-binding-table-by-path path '()))
         (syntax-property this-syntax 'identifier-usages usages)]
        #:parent-context-modifier values
        #:parent-srcloc-modifier values
        #:parent-props-modifier values))
    
    (define (enrich stx #:skip-root? [skip-root? #false])
      (syntax-traverse stx
        #:skip-root? skip-root?
        [child
         #:do [(define child-stx (attribute child))
               (define orig-path (syntax-original-path child-stx))]
         #:when (and orig-path (sorted-map-contains-key? movement-table orig-path))
         #:do [(define expansion
                 (transduce (sorted-map-get movement-table orig-path)
                            (mapping (λ (p) (syntax-ref expanded-with-properties p)))
                            (filtering syntax-original?)
                            #:into into-first))]
         #:when (present? expansion)
         (match-define (present expanded-child) expansion)
         (log-resyntax-debug "enriching ~a with scopes and properties from expansion" child-stx)
         (enrich (datum->syntax expanded-child (syntax-e child-stx) child-stx expanded-child)
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
                 (mapping enrich)
                 (peeking
                  (λ (visit)
                    (unless (syntax-original-path visit)
                      (raise-arguments-error
                       'source-analyze "post-enriched visit is missing original path"
                       "visited syntax" visit))))
                 (sorting syntax-path<=> #:key syntax-original-path)
                 #:into into-list))

    (source-code-analysis #:code code
                          #:visited-forms visited
                          #:expansion-time-output output
                          #:namespace ns)))


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
