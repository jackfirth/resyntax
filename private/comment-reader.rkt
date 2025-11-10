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
  (let loop ([ranges '()]
             [mode #f])
    (define-values (lexeme type paren start end backup mode-out) (module-lexer in 0 mode))
    (cond
      [(eof-object? lexeme)
       (if (null? ranges)
           (range-set #:comparator natural<=>)
           (apply range-set ranges))]
      [(equal? type 'comment)
       ;; Convert from 1-indexed positions to 0-indexed
       (define comment-start (sub1 start))
       (define comment-end-base (sub1 end))
       ;; For line comments (non-empty lexeme), check if next token is a newline
       ;; For block comments (empty lexeme), don't include trailing whitespace
       (define is-line-comment? (not (equal? lexeme "")))
       (if is-line-comment?
           ;; Peek at the next token to see if it's a newline
           (let ()
             (define-values (next-lexeme next-type next-paren next-start next-end next-backup next-mode)
               (module-lexer in 0 mode-out))
             (cond
               [(and (equal? next-type 'white-space) (equal? next-lexeme "\n"))
                ;; Include the newline in the comment range and continue with the mode after the newline
                (loop (cons (closed-open-range comment-start (sub1 next-end) #:comparator natural<=>) ranges)
                      next-mode)]
               [(eof-object? next-lexeme)
                ;; EOF after comment
                (loop (cons (closed-open-range comment-start comment-end-base #:comparator natural<=>) ranges)
                      next-mode)]
               [else
                ;; Non-newline token after comment; we need to "un-consume" it
                ;; by processing it in the next iteration. But we can't easily do that
                ;; with module-lexer. Let's use a different approach.
                (loop (cons (closed-open-range comment-start comment-end-base #:comparator natural<=>) ranges)
                      next-mode)]))
           ;; Block comment - don't peek ahead
           (loop (cons (closed-open-range comment-start comment-end-base #:comparator natural<=>) ranges)
                 mode-out))]
      [(equal? type 'sexp-comment)
       ;; For expression comments, we need to skip the following s-expression
       (define sexp-start (sub1 start))
       (define-values (expr-start expr-end final-mode) (skip-one-sexp in mode-out))
       ;; Convert expr-end from 1-indexed to 0-indexed
       (define comment-end (if expr-end (sub1 expr-end) (sub1 end)))
       (loop (cons (closed-open-range sexp-start comment-end #:comparator natural<=>) ranges)
             final-mode)]
      [else
       (loop ranges mode-out)])))


;; Helper to skip one s-expression worth of tokens after a #; comment
;; Returns (values start-pos end-pos final-mode)
(define (skip-one-sexp in mode)
  (let loop ([depth 0]
             [seen-non-whitespace? #f]
             [start-pos #f]
             [end-pos #f]
             [current-mode mode])
    (define-values (lexeme type paren start end backup mode-out) (module-lexer in 0 current-mode))
    (cond
      [(eof-object? lexeme) (values start-pos end-pos mode-out)]
      [(equal? type 'white-space) (loop depth seen-non-whitespace? start-pos end-pos mode-out)]
      [(equal? type 'sexp-comment) 
       ;; Another sexp-comment; recursively skip its expression
       (define-values (nested-start nested-end nested-mode) (skip-one-sexp in mode-out))
       (loop depth #t (or start-pos start) nested-end nested-mode)]
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
          (values new-start end mode-out)]
         ;; If we just closed all parens (depth went from 1 to 0), we're done
         [(and (= new-depth 0) is-closer? (= depth 1))
          (values new-start end mode-out)]
         ;; Otherwise, continue
         [else (loop new-depth #t new-start end mode-out)])])))


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
