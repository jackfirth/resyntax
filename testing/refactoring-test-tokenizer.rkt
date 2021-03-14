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
  (require (submod "..")
           brag/support
           rackunit))


;@----------------------------------------------------------------------------------------------------


(define-lex-abbrev refactoring-test-separator-line
  (concatenation (repetition 3 +inf.0 #\-)))


(define-lex-abbrev refactoring-test-code-block
  (concatenation refactoring-test-separator-line
                 (complement (concatenation any-string #\newline "-" any-string))
                 #\newline
                 refactoring-test-separator-line))


(define-lex-abbrev refactoring-test-code-line
  (concatenation #\- #\space (complement (concatenation any-string #\newline any-string)) #\newline))


(define-lex-abbrev refactoring-test-string-literal
  (concatenation #\" (complement (concatenation any-string #\" any-string)) #\"))


(define-lex-abbrev refactoring-test-identifier
  (repetition 1 +inf.0 (union alphabetic symbolic numeric (char-set "-/"))))


(define-tokens refactoring-test-tokens (IDENTIFIER STRING-LITERAL CODE-BLOCK))
(define-empty-tokens empty-refactoring-test-tokens (REQUIRE-KEYWORD TEST-KEYWORD SEPARATOR-LINE))


(define (string-lines str)
  (for/list ([line (in-lines (open-input-string str))])
    (string->immutable-string line)))


(define refactoring-test-lexer
  (lexer-src-pos
   [whitespace (refactoring-test-lexer input-port)]
   ["require:" (token-REQUIRE-KEYWORD)]
   ["test:" (token-TEST-KEYWORD)]
   [refactoring-test-separator-line (token-SEPARATOR-LINE)]
   [refactoring-test-code-line (token-CODE-BLOCK (string->immutable-string (substring lexeme 2)))]
   [refactoring-test-code-block
    (block
     (define lines (drop-right (drop (string-lines lexeme) 1) 1))
     (token-CODE-BLOCK
      (if (empty? lines) "" (string->immutable-string (string-join lines "\n" #:after-last "\n")))))]
   [refactoring-test-string-literal
    (token-STRING-LITERAL
     (string->immutable-string (substring lexeme 1 (sub1 (string-length lexeme)))))]
   [refactoring-test-identifier (token-IDENTIFIER (string->symbol lexeme))]))


(define ((make-refactoring-test-tokenizer port))
  (refactoring-test-lexer port))


(module+ test
  (test-case "make-refactoring-test-tokenizer"

    (test-case "code blocks"
      (define input (open-input-string "---\n#lang racket/base\n(void)\n---"))
      (port-count-lines! input)
      (define tokenizer (make-refactoring-test-tokenizer input))
      (define expected-token
        (position-token
         (token-CODE-BLOCK "#lang racket/base\n(void)\n")
         (position 1 1 0)
         (position 33 4 3)))
      (check-equal? (tokenizer) expected-token))

    (test-case "empty code blocks"
      (define input (open-input-string "---\n---"))
      (port-count-lines! input)
      (define tokenizer (make-refactoring-test-tokenizer input))
      (define expected-token
        (position-token
         (token-CODE-BLOCK "")
         (position 1 1 0)
         (position 8 2 3)))
      (check-equal? (tokenizer) expected-token))

    (test-case "code lines"
      (define input (open-input-string "- #lang racket/base (void)\n"))
      (port-count-lines! input)
      (define tokenizer (make-refactoring-test-tokenizer input))
      (define expected-token
        (position-token
         (token-CODE-BLOCK "#lang racket/base (void)\n")
         (position 1 1 0)
         (position 28 2 0)))
      (check-equal? (tokenizer) expected-token))

    (test-case "multiple code lines"
      (define input (open-input-string "- #lang racket/base (f)\n- #lang racket/base (g)\n"))
      (port-count-lines! input)
      (define tokenizer (make-refactoring-test-tokenizer input))
      (define expected-token
        (position-token
         (token-CODE-BLOCK "#lang racket/base (f)\n")
         (position 1 1 0)
         (position 25 2 0)))
      (check-equal? (tokenizer) expected-token))))
