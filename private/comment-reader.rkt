#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [read-comment-locations (->* () (input-port?) range-set?)]))


(require racket/sequence
         rebellion/base/comparator
         rebellion/base/range
         rebellion/collection/range-set
         syntax-color/module-lexer)


(module+ test
  (require rackunit
           (submod "..")))


;@----------------------------------------------------------------------------------------------------


(define (read-comment-locations [in (current-input-port)])
  (port-count-lines! in)
  (let loop ([ranges '()])
    (define-values (lexeme type paren start end backup mode) (module-lexer in 0 #f))
    (cond
      [(eof-object? lexeme)
       (apply range-set ranges)]
      [(equal? type 'comment)
       ;; Convert from 1-indexed positions to 0-indexed
       (define comment-start (sub1 start))
       (define comment-end-base (sub1 end))
       ;; For line comments (non-empty lexeme), include the trailing newline
       ;; For block comments (empty lexeme), don't
       (define is-line-comment? (not (equal? lexeme "")))
       (define pos-before-peek (file-position in))
       (define-values (next-lexeme next-type next-paren next-start next-end next-backup next-mode)
         (module-lexer in 0 mode))
       (define comment-end
         (cond
           [(and is-line-comment?
                 (equal? next-type 'white-space)
                 (equal? next-lexeme "\n"))
            ;; Include the trailing newline for line comments
            (sub1 next-end)]
           [else
            ;; Put the port position back and use the original end
            (file-position in pos-before-peek)
            comment-end-base]))
       (loop (cons (closed-open-range comment-start comment-end #:comparator natural<=>) ranges))]
      [(equal? type 'sexp-comment)
       ;; For expression comments, we need to skip the following s-expression
       (define sexp-start (sub1 start))
       (define-values (expr-start expr-end) (skip-one-sexp in))
       ;; Convert expr-end from 1-indexed to 0-indexed
       (define comment-end (if expr-end (sub1 expr-end) (sub1 end)))
       (loop (cons (closed-open-range sexp-start comment-end #:comparator natural<=>) ranges))]
      [else
       (loop ranges)])))


;; Helper to skip one s-expression worth of tokens after a #; comment
(define (skip-one-sexp in)
  (let loop ([depth 0]
             [seen-non-whitespace? #f]
             [start-pos #f]
             [end-pos #f])
    (define-values (lexeme type paren start end backup mode) (module-lexer in 0 #f))
    (cond
      [(eof-object? lexeme) (values start-pos end-pos)]
      [(equal? type 'white-space) (loop depth seen-non-whitespace? start-pos end-pos)]
      [(equal? type 'sexp-comment) 
       ;; Another sexp-comment; recursively skip its expression
       (define-values (nested-start nested-end) (skip-one-sexp in))
       (loop depth #t (or start-pos start) nested-end)]
      [else
       (define is-opener? (and paren (memq paren '(|[| |(| |{|))))
       (define is-closer? (and paren (memq paren '(|]| |)| |}|))))
       (define new-depth
         (cond
           [is-opener? (add1 depth)]
           [is-closer? (sub1 depth)]
           [else depth]))
       (define new-start (or start-pos start))
       (cond
         ;; If this is a non-paren token and we haven't seen anything yet, consume just this token
         [(and (not seen-non-whitespace?) (= depth 0) (not paren))
          (values new-start end)]
         ;; If we just closed all parens (depth went from 1 to 0), we're done
         [(and (= new-depth 0) is-closer? (= depth 1))
          (values new-start end)]
         ;; Otherwise, continue
         [else (loop new-depth #t new-start end)])])))


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
      (check-equal? (read-comments-for-test input) expected))

    (test-case "expression comments - simple"
      (define input "#;(foo)\n")
      (define expected (range-set (natural-range 0 7)))
      (check-equal? (read-comments-for-test input) expected))

    (test-case "expression comments - nested"
      (define input "#;(foo (bar baz))\n")
      (define expected (range-set (natural-range 0 17)))
      (check-equal? (read-comments-for-test input) expected))

    (test-case "expression comments - atom"
      (define input "#;atom\n")
      (define expected (range-set (natural-range 0 6)))
      (check-equal? (read-comments-for-test input) expected))

    (test-case "expression comments - double"
      (define input "#; #; (foo) (bar)\n")
      (define expected (range-set (natural-range 0 17)))
      (check-equal? (read-comments-for-test input) expected))

    (test-case "expression comments with code"
      (define input "(define x 1) #;(unused) (define y 2)\n")
      (define expected (range-set (natural-range 13 23)))
      (check-equal? (read-comments-for-test input) expected))))
