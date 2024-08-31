#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [read-comment-locations (->* () (input-port?) range-set?)]))


(require br-parser-tools/lex
         (prefix-in : br-parser-tools/lex-sre)
         racket/sequence
         rebellion/base/comparator
         rebellion/base/range
         rebellion/collection/list
         rebellion/collection/range-set
         rebellion/streaming/reducer
         rebellion/streaming/transducer
         resyntax/private/syntax-traversal)


(module+ test
  (require (submod "..")
           rackunit))


;@----------------------------------------------------------------------------------------------------


(struct comment-set (comment-list)
  #:transparent
  #:guard (λ (comments _) (sequence->list comments))
  #:property prop:sequence (λ (this) (comment-set-comment-list this)))


(define into-comment-set (reducer-map into-list #:range comment-set))


(struct comment (text location)
  #:transparent
  #:guard (λ (text location _) (values (string->immutable-string text) location)))


(define (comment-range comment)
  (define loc (comment-location comment))
  (define start (srcloc-position loc))
  (define end (+ start (srcloc-span loc)))
  (closed-open-range start end #:comparator natural<=>))


(define (comment-subset comments position-range)
  (transduce comments
             (filtering (λ (c) (range-encloses? position-range (comment-range c))))
             #:into into-comment-set))


(define (syntax-source-range stx)
  (define start (syntax-position stx))
  (define end (+ start (syntax-span stx)))
  (closed-open-range start end #:comparator natural<=>))


(define (syntax-original-locations stx)
  (transduce (leaves-in-syntax stx syntax-original?)
             (mapping syntax-source-range)
             #:into into-list))


(define (read-comment-locations [in (current-input-port)])
  (port-count-lines! in)
  (define next! (λ () (comment-lexer in)))
  (transduce (in-producer next! eof)
             (mapping srcloc-token-srcloc)
             (mapping srcloc-range)
             #:into (into-range-set natural<=>)))


(define (srcloc-range srcloc)
  (define start (sub1 (srcloc-position srcloc)))
  (define end (+ start (srcloc-span srcloc)))
  (closed-open-range start end #:comparator natural<=>))


(define-tokens racket-tokens (LINE-COMMENT BLOCK-COMMENT))


(define-lex-abbrev racket-line-comment
  (concatenation ";" (complement (:: any-string "\n" any-string)) "\n"))


(define (build-racket-line-comment lexeme)
  (token-LINE-COMMENT (string->immutable-string lexeme)))


;; Technically not correct because block comments can be nested.
(define-lex-abbrev racket-block-comment
  (concatenation "#|" (complement (:: any-string (:or "#|" "#|") any-string)) "|#"))


(define (build-racket-block-comment lexeme)
  (token-BLOCK-COMMENT (string->immutable-string lexeme)))


;; This lexer should also read string literals and discard them, so that comment-starting characters
;; inside string literals are ignored.
(define comment-lexer
  (lexer-srcloc
   [racket-line-comment (build-racket-line-comment lexeme)]
   [racket-block-comment (build-racket-block-comment lexeme)]
   [any-char (return-without-srcloc (comment-lexer input-port))]))


(module+ test
  (test-case "comment-lexer"

    (define (natural-range start end)
      (closed-open-range start end #:comparator natural<=>))

    (define (read-comments-for-test test-program)
      (read-comment-locations (open-input-string test-program 'test-program)))

    (test-case "line comments"
      (define input "; This is a comment\n")
      (define expected (range-set (natural-range 0 20)))
      (check-equal? (read-comments-for-test input) expected))

    (test-case "double semicolon line comments"
      (define input ";; This is a comment\n")
      (define expected (range-set (natural-range 0 21)))
      (check-equal? (read-comments-for-test input) expected))

    (test-case "line comments after expressions"
      (define input "(void) ; This is a comment\n")
      (define expected (range-set (natural-range 7 27)))
      (check-equal? (read-comments-for-test input) expected))

    (test-case "line comments above expressions"
      (define input "; This is a comment\n(void)\n")
      (define expected (range-set (natural-range 0 20)))
      (check-equal? (read-comments-for-test input) expected))

    (test-case "line comments with non-ASCII characters"
      (define input "; λλλλλ\n")
      (define expected (range-set (natural-range 0 8)))
      (check-equal? (read-comments-for-test input) expected))

    (test-case "block comments"
      (define input "#|\nThis is a block comment\n|#\n")
      (define expected (range-set (natural-range 0 29)))
      (check-equal? (read-comments-for-test input) expected))

    (test-case "block comments below expressions"
      (define input "(void)\n#|\nThis is a block comment\n|#\n")
      (define expected (range-set (natural-range 7 36)))
      (check-equal? (read-comments-for-test input) expected))

    (test-case "block comments above expressions"
      (define input "#|\nThis is a block comment\n|#\n(void)\n")
      (define expected (range-set (natural-range 0 29)))
      (check-equal? (read-comments-for-test input) expected))

    (test-case "multiple line comments"
      (define input "; Line 1\n; Line 2\n; Line 3\n")
      (define expected (range-set (natural-range 0 27)))
      (check-equal? (read-comments-for-test input) expected))))
