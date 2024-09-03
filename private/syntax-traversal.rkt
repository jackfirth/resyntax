#lang racket/base


(require racket/contract/base)


(provide
 syntax-traverse
 (contract-out
  [leaves-in-syntax (->* (syntax?) ((-> syntax? boolean?)) (sequence/c syntax?))]
  [syntax-directly-enclosing-expressions (-> syntax? identifier? (listof syntax?))]))


(require (for-syntax racket/base
                     resyntax/private/more-syntax-parse-classes)
         racket/match
         racket/sequence
         racket/stream
         syntax/parse
         syntax/parse/define)


(module+ test
  (require (submod "..")
           rackunit))


;@----------------------------------------------------------------------------------------------------


(define (leaves-in-syntax stx [leaf? flat-syntax?])
  (stream*
   (match stx
     [(? leaf?) (stream stx)]
     [(app syntax-e (and stx-list (or '() (? pair?)))) (leaves-in-syntax-pair stx-list leaf?)]
     [(app syntax-e (box substx)) (leaves-in-syntax substx leaf?)]
     [else (stream)])))


(define (leaves-in-syntax-pair stx-list [leaf? flat-syntax?])
  (stream*
   (match stx-list
     ['() (stream)]
     [(cons head '()) (leaves-in-syntax head leaf?)]
     [(cons head (? syntax? tail))
      (stream-append (leaves-in-syntax head leaf?) (leaves-in-syntax tail leaf?))]
     [(cons head (? pair? tail))
      (stream-append (leaves-in-syntax head leaf?) (leaves-in-syntax-pair tail leaf?))])))


(define (flat-syntax? stx)
  (define datum (syntax-e stx))
  (or (symbol? datum)
      (number? datum)
      (string? datum)
      (boolean? datum)
      (regexp? datum)
      (keyword? datum)))


(define (syntax-directly-enclosing-expressions stx id)

  (define (directly-encloses? subform)
    (syntax-parse subform
      [(part ...)
       (for/or ([part-stx (in-list (attribute part))])
         (and (identifier? part-stx) (free-identifier=? id part-stx)))]
      [(part ... . tail-part)
       (for/or ([part-stx (in-list (cons #'tail-part (attribute part)))])
         (and (identifier? part-stx) (free-identifier=? id part-stx)))]
      [_ #false]))

  (sequence->list (leaves-in-syntax stx directly-encloses?)))


(define-syntax-parse-rule
  (syntax-traverse (~var stx-expr (expr/c #'syntax?))
    option:syntax-parse-option ...
    [clause-pattern directive:syntax-parse-pattern-directive ... clause-body:expr ...] ...)
  (let ()
    (define-syntax-class traversal-case
      #:attributes (traversed)
      (~@ . option) ...
      (pattern clause-pattern (~@ . directive) ...
        #:attr traversed (let () clause-body ...)) ...)
    (let loop ([stx stx-expr.c])
      (syntax-parse stx
        [(~var matched traversal-case) (attribute matched.traversed)]
        
        [(part (... ...))
         #:cut
         #:with (traversed-part (... ...)) (map loop (attribute part))
         #'(traversed-part (... ...))]
        [(part (... ...+) . tail-part)
         #:cut
         #:with (traversed-part (... ...)) (map loop (attribute part))
         #:with traversed-tail (loop #'tail-part)
         #'(traversed-part (... ...) . traversed-tail)]
        [_ stx]))))


(module+ test
  (test-case "syntax-traverse"
    (define stx
      #'(define (foo)
          (cons x y)
          (define (bar)
            (cons a b))
          (cons c d)))
    (define actual
      (syntax->datum
       (syntax-traverse stx
         #:literals (cons)
         [(cons _ _) #'CONS-EXPRESSION])))
    (define expected
      '(define (foo)
         CONS-EXPRESSION
         (define (bar)
           CONS-EXPRESSION)
         CONS-EXPRESSION))
    (check-equal? actual expected)))
