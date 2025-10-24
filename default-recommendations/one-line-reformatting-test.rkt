#lang resyntax/test
header: - #lang racket/base


test: "regression test for issue #605"
----------------------------------------
(require racket/match)
(define (f expr)
  (let loop ([expr expr]
             [env '()])
    (match expr
      [`(,(and (or '+ '- '* '/ 'and 'or) op) ,as ..2 ,b) `(,op ,(loop `(,op ,@as) env) ,(loop b env))]
      [_ (void)])))
========================================
(require racket/match)
(define (f expr)
  (let loop ([expr expr]
             [env '()])
    (match expr
      [`(,(and (or '+ '- '* '/ 'and 'or) op) ,as ..2 ,b) (list op
                                                               (loop `(,op ,@as) env)
                                                               (loop b env))]
      [_ (void)])))
----------------------------------------
