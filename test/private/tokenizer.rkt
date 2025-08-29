#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [make-refactoring-test-tokenizer (-> input-port? (-> (or/c position-token? eof-object?)))]))


(require br-parser-tools/lex
         racket/block
         racket/list
         racket/string)


(module+ test
  (require brag/support
           rackunit
           (submod "..")))


;@----------------------------------------------------------------------------------------------------


(define-lex-abbrev rest-of-line
  (concatenation (complement (concatenation any-string #\newline any-string)) #\newline))

(define-lex-abbrev dash-line (concatenation (repetition 3 +inf.0 #\-) #\newline))
(define-lex-abbrev equals-line (concatenation (repetition 3 +inf.0 #\=) #\newline))
(define-lex-abbrev pipe-dash-line (concatenation "|" dash-line))
(define-lex-abbrev pipe-equals-line (concatenation "|" equals-line))


(define-lex-abbrev refactoring-test-code-block
  (concatenation refactoring-test-separator-line
                 (complement (concatenation any-string #\newline (union "-" "=") any-string))
                 #\newline
                 refactoring-test-separator-line))


(define-lex-abbrev refactoring-test-code-line
  (concatenation #\- #\space (complement (concatenation any-string #\newline any-string)) #\newline))


(define-lex-abbrev refactoring-test-literal-string
  (concatenation #\" (complement (concatenation any-string #\" any-string)) #\"))


(define-lex-abbrev refactoring-test-literal-integer
  (repetition 1 +inf.0 numeric))


(define-lex-abbrev refactoring-test-identifier
  (concatenation alphabetic
                 (repetition 0 +inf.0 (union alphabetic numeric (char-set "-/")))))


(define-tokens refactoring-test-tokens (IDENTIFIER LITERAL-STRING LITERAL-INTEGER CODE-LINE))


(define-empty-tokens empty-refactoring-test-tokens
  (COLON
   AT-SIGN
   DOUBLE-DOT
   COMMA
   SINGLE-DASH
   DASH-LINE
   EQUALS-LINE
   PIPE-DASH-LINE
   PIPE-EQUALS-LINE
   PIPE-SPACE))


(define (string-lines str)
  (for/list ([line (in-lines (open-input-string str))])
    (string->immutable-string line)))


(define (make-refactoring-test-tokenizer port)

  (define initial-lexer
    (lexer-src-pos
     [whitespace (return-without-pos (initial-lexer input-port))]
     [":" (token-COLON)]
     ["@" (token-AT-SIGN)]
     [".." (token-DOUBLE-DOT)]
     ["," (token-COMMA)]
     ["- "
      (let ()
        (set! active-lexer single-code-line-lexer)
        (token-SINGLE-DASH))]
     [dash-line
      (let ()
        (set! active-lexer multi-code-line-lexer)
        (token-DASH-LINE))]
     [pipe-dash-line
      (let ()
        (set! active-lexer pipe-prefix-lexer)
        (token-PIPE-DASH-LINE))]
     [refactoring-test-literal-string
      (token-LITERAL-STRING
       (string->immutable-string (substring lexeme 1 (sub1 (string-length lexeme)))))]
     [refactoring-test-literal-integer (token-LITERAL-INTEGER (string->number lexeme))]
     [refactoring-test-identifier (token-IDENTIFIER (string->symbol lexeme))]))

  (define single-code-line-lexer
    (lexer-src-pos
     [rest-of-line
      (let ()
        (set! active-lexer initial-lexer)
        (token-CODE-LINE lexeme))]))

  (define multi-code-line-lexer
    (lexer-src-pos
     [(concatenation (intersection any-char (complement (char-set "-="))) rest-of-line)
      (token-CODE-LINE lexeme)]
     [dash-line
      (let ()
        (set! active-lexer initial-lexer)
        (token-DASH-LINE))]
     [equals-line (token-EQUALS-LINE)]))

  (define pipe-prefix-lexer
    (lexer-src-pos
     ["| "
      (let ()
        (set! active-lexer pipe-code-line-lexer)
        (token-PIPE-SPACE))]
     [pipe-dash-line
      (let ()
        (set! active-lexer initial-lexer)
        (token-PIPE-DASH-LINE))]
     [pipe-equals-line (token-PIPE-EQUALS-LINE)]))

  (define pipe-code-line-lexer
    (lexer-src-pos
     [rest-of-line
      (let ()
        (set! active-lexer pipe-prefix-lexer)
        (token-CODE-LINE lexeme))]))

  (define active-lexer initial-lexer)

  (λ () (active-lexer port)))


(module+ test

  (define (tokenize-until-eof tokenizer)
    (for/list ([t (in-producer tokenizer eof)])
      t))

  (test-case "make-refactoring-test-tokenizer"

    (test-case "statements"
      (define input (open-input-string "header:\n- #lang racket\n"))
      (port-count-lines! input)
      (define tokenizer (make-refactoring-test-tokenizer input))
      (define expected-tokens
        (list
         (position-token (token-IDENTIFIER 'header) (position 1 1 0) (position 7 1 6))
         (position-token (token-COLON) (position 7 1 6) (position 8 1 7))
         (position-token (token-SINGLE-DASH) (position 9 2 0) (position 11 2 2))
         (position-token (token-CODE-LINE "#lang racket\n") (position 11 2 2) (position 24 3 0))))
      (check-equal? (tokenize-until-eof tokenizer) expected-tokens))

    (test-case "code blocks"
      (define input (open-input-string "---\n#lang racket/base\n(void)\n---\n"))
      (port-count-lines! input)
      (define tokenizer (make-refactoring-test-tokenizer input))
      (define expected-tokens
        (list
         (position-token (token-DASH-LINE) (position 1 1 0) (position 5 2 0))
         (position-token (token-CODE-LINE "#lang racket/base\n") (position 5 2 0) (position 23 3 0))
         (position-token (token-CODE-LINE "(void)\n") (position 23 3 0) (position 30 4 0))
         (position-token (token-DASH-LINE) (position 30 4 0) (position 34 5 0))))
      (check-equal? (tokenize-until-eof tokenizer) expected-tokens))

    (test-case "empty code blocks"
      (define input (open-input-string "---\n---\n"))
      (port-count-lines! input)
      (define tokenizer (make-refactoring-test-tokenizer input))
      (define expected-tokens
        (list
         (position-token (token-DASH-LINE) (position 1 1 0) (position 5 2 0))
         (position-token (token-DASH-LINE) (position 5 2 0) (position 9 3 0))))
      (check-equal? (tokenize-until-eof tokenizer) expected-tokens))

    (test-case "code lines"
      (define input (open-input-string "- #lang racket/base (void)\n"))
      (port-count-lines! input)
      (define tokenizer (make-refactoring-test-tokenizer input))
      (define expected-tokens
        (list
         (position-token (token-SINGLE-DASH) (position 1 1 0) (position 3 1 2))
         (position-token
          (token-CODE-LINE "#lang racket/base (void)\n") (position 3 1 2) (position 28 2 0))))
      (check-equal? (tokenize-until-eof tokenizer) expected-tokens))

    (test-case "multiple code lines"
      (define input (open-input-string "- #lang racket/base (f)\n- #lang racket/base (g)\n"))
      (port-count-lines! input)
      (define tokenizer (make-refactoring-test-tokenizer input))
      (define expected-tokens
        (list
         (position-token (token-SINGLE-DASH) (position 1 1 0) (position 3 1 2))
         (position-token
          (token-CODE-LINE "#lang racket/base (f)\n") (position 3 1 2) (position 25 2 0))
         (position-token (token-SINGLE-DASH) (position 25 2 0) (position 27 2 2))
         (position-token
          (token-CODE-LINE "#lang racket/base (g)\n") (position 27 2 2) (position 49 3 0))))
      (check-equal? (tokenize-until-eof tokenizer) expected-tokens))))
