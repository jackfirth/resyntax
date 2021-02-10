#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [make-refactoring-test-tokenizer (-> input-port? (-> (or/c position-token? eof-object?)))]))


(require br-parser-tools/lex)


;@----------------------------------------------------------------------------------------------------


(define-lex-abbrev refactoring-test-separator-line
  (concatenation (repetition 3 +inf.0 #\-)))


(define-lex-abbrev refactoring-test-lang-block
  (concatenation
   "#lang "
   (complement (concatenation any-string #\newline "-" any-string))))


(define-lex-abbrev refactoring-test-string-literal
  (concatenation #\" (complement (concatenation any-string #\" any-string)) #\"))


(define-lex-abbrev refactoring-test-identifier
  (repetition 1 +inf.0 (union alphabetic symbolic numeric (char-set "-"))))


(define-tokens refactoring-test-tokens (IDENTIFIER STRING-LITERAL LANG-BLOCK))
(define-empty-tokens empty-refactoring-test-tokens (REQUIRE-KEYWORD TEST-KEYWORD SEPARATOR-LINE))


(define refactoring-test-lexer
  (lexer-src-pos
   [whitespace (refactoring-test-lexer input-port)]
   ["require:" (token-REQUIRE-KEYWORD)]
   ["test:" (token-TEST-KEYWORD)]
   [refactoring-test-separator-line (token-SEPARATOR-LINE)]
   [refactoring-test-lang-block (token-LANG-BLOCK (string->immutable-string lexeme))]
   [refactoring-test-string-literal
    (token-STRING-LITERAL
     (string->immutable-string (substring lexeme 1 (sub1 (string-length lexeme)))))]
   [refactoring-test-identifier (token-IDENTIFIER (string->symbol lexeme))]))


(define ((make-refactoring-test-tokenizer port))
  (refactoring-test-lexer port))
