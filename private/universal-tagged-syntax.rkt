#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [check-universal-tagged-syntax (-> syntax? syntax?)]))


(require racket/list
         racket/match
         racket/mutability
         racket/port
         resyntax/private/source)


(module+ test
  (require (submod "..")
           racket/string
           rackunit))


;@----------------------------------------------------------------------------------------------------


(define (check-universal-tagged-syntax stx)
  (let loop ([stx stx] [top? #true])
    (match (syntax-e stx)
      [(? atom?)
       (define content (syntax-property stx 'uts-content))
       (unless content
         (raise-arguments-error 'check-universal-tagged-syntax
                                "atom is missing a 'uts-content syntax property"
                                "atom" stx))]
      [(list children ...)
       (when (empty? children)
         (raise-arguments-error 'check-universal-tagged-syntax
                                "empty compound forms are not allowed"
                                "form" stx))
       (define shape-tag (first children))
       (if top?
           (unless (equal? (syntax-e shape-tag) 'module)
             (raise-arguments-error 'check-universal-tagged-syntax
                                    "the only legal top-level compound form is (module ...)"
                                    "top-level form" stx))
           (unless (keyword? (syntax-e shape-tag))
             (raise-arguments-error
              'check-universal-tagged-syntax
              "every non-top-level compound form must start with a shape tag keyword"
              "form" stx)))
       (define separators (syntax-property shape-tag 'uts-separators))
       (unless separators
         (raise-arguments-error 'check-universal-tagged-syntax
                                "shape tag is missing a 'uts-separators syntax property"
                                "shape tag" shape-tag
                                "form" stx))
       (for ([child (in-list (rest children))])
         (loop child #false))]
      [other
       (raise-arguments-error 'check-universal-tagged-syntax
                              "every form must be either an atom or a proper list"
                              "form" stx)]))
  stx)


; TODO: check source locations more thoroughly, check uts property strings match contents in
; sourceloc, check source location relationships between adjacent nodes and parent-child nodes
(define (check-source-universal-tagged-syntax source)
  (define stx (source-read-syntax source))
  (define expected-source-name (source-name source))
  (check-universal-tagged-syntax stx)
  (let loop ([stx stx])
    (unless (syntax-original? stx)
      (raise-arguments-error 'check-source-universal-tagged-syntax
                             "source produced a syntax object that is not syntax-original?"
                             "syntax object" stx
                             "source" source))
    (unless (syntax-source stx)
      (raise-arguments-error
       'check-source-universal-tagged-syntax
       "source produced a syntax object with no source name in its source location information"
       "syntax object" stx
       "source" source))
    (unless (equal? (syntax-source stx) expected-source-name)
      (raise-arguments-error
       'check-source-universal-tagged-syntax
       "source produced a syntax object with an incorrect source name"
       "syntax object" stx
       "incorrect source name" (syntax-source stx)
       "expected source name" expected-source-name))
    (match (syntax-e stx)
      [(? atom?)
       (void)]
      [(list shape-tag children ...)
       (for ([child (in-list children)])
         (loop child))])))


(define (atom? v)
  (or (symbol? v)
      (keyword? v)
      (number? v)
      (boolean? v)
      (char? v)
      (immutable-string? v)
      (immutable-bytes? v)
      (regexp? v)
      (pregexp? v)
      (byte-regexp? v)
      (byte-pregexp? v)))


(define (write-universal-tagged-syntax stx)
  (match (syntax-e stx)
    [(? atom?)
     (write-string (syntax-property stx 'uts-content))
     (void)]
    [(list tag children ...)
     (define seps (syntax-property tag 'uts-separators))
     (write-string (first seps))
     (for ([child (in-list children)]
           [suffix (in-list (rest seps))])
       (write-universal-tagged-syntax child)
       (write-string suffix))]))


(define (universal-tagged-syntax->string stx)
  (with-output-to-string (λ () (write-universal-tagged-syntax stx))))


(module+ test
  (test-case "universal-tagged-syntax->string"
    (define src
      (string-source
       (string-join
        (list
         "#lang resyntax/test"
         "require: resyntax/default-recommendations boolean-shortcuts"
         ""
         "header:"
         "- #lang racket"
         ""
         "test: \"foo\""
         "- (and a (and b c))"
         "- (and a b c)")
        "\n"
        #:after-last "\n")))
    (check-source-universal-tagged-syntax src)

    (define stx (source-read-syntax src))
    (define written-form (universal-tagged-syntax->string stx))

    (check-equal? written-form (string-source-contents src))))
