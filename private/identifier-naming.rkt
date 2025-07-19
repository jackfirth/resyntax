#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [depluralize-id (-> identifier? identifier?)]))


(require racket/string
         racket/syntax)


;@----------------------------------------------------------------------------------------------------


(define (depluralize-id id)
  (define plural-name (symbol->string (syntax-e id)))
  (define singular-name
    (cond
      [(string-suffix? plural-name "es") (string-trim plural-name "es" #:left? #false)]
      [(string-suffix? plural-name "s") (string-trim plural-name "s" #:left? #false)]
      [else plural-name]))
  (format-id #false "~a" (string->symbol singular-name)))
