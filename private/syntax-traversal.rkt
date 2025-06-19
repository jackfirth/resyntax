#lang racket/base


(require racket/contract/base)


(provide atom
         expression-directly-enclosing
         syntax-search
         syntax-traverse)


(require (for-syntax racket/base
                     resyntax/private/more-syntax-parse-classes
                     syntax/parse)
         racket/match
         racket/sequence
         racket/stream
         syntax/parse
         syntax/parse/define)


(module+ test
  (require rackunit
           (submod "..")))


;@----------------------------------------------------------------------------------------------------


(define-syntax-class atom
  (pattern (~or :id :number :str :boolean :regexp :keyword)))


(define-syntax-class (expression-directly-enclosing id)
  (pattern (part ...)
    #:when (for/or ([part-stx (in-list (attribute part))]
                    #:when (identifier? part-stx))
             (free-identifier=? id part-stx)))
  (pattern (part ... . tail-part)
    #:when (for/or ([part-stx (in-list (cons #'tail-part (attribute part)))]
                    #:when (identifier? part-stx))
             (free-identifier=? id part-stx))))


(begin-for-syntax
  (define-syntax-class syntax-search-clause
    #:attributes (syntax-pattern [directive 1] output-stream)
    (pattern [syntax-pattern directive:syntax-parse-pattern-directive ... body:expr ... last-body]
      #:declare last-body (expr/c #'stream?)
      #:with output-stream #'(stream-lazy (let () body ... last-body.c)))
    (pattern [syntax-pattern directive:syntax-parse-pattern-directive ...]
      #:with output-stream #'(stream this-syntax))))


(define-syntax-parse-rule
  (syntax-search stx-expr option:syntax-parse-option ... clause:syntax-search-clause ...)
  #:declare stx-expr (expr/c #'syntax?)
  (let ()
    (define-syntax-class search-case
      #:attributes (output-stream)
      (~@ . option) ...
      (pattern clause.syntax-pattern (~@ . clause.directive) ...
        #:attr output-stream clause.output-stream)
      ...)
    (let loop ([stx stx-expr.c])
      (stream-lazy
       (syntax-parse stx
         [(~var matched search-case) (attribute matched.output-stream)]
         [(part (... ...))
          #:cut
          (apply stream-append
                 (for/list ([part-stx (in-list (attribute part))])
                   (loop part-stx)))]
         [(part (... ...+) . tail-part)
          #:cut
          (stream-append (apply stream-append
                                (for/list ([part-stx (in-list (attribute part))])
                                  (loop part-stx)))
                         (loop #'tail-part))]
         [_ (stream)])))))


(module+ test
  (test-case "syntax-search"
    (define stx
      #'(define (foo)
          (cons x y)
          (define (bar)
            (cons a b))
          (cons c d)))
    (define actual
      (sequence->list
       (syntax-search stx
         #:literals (cons)
         [(cons _ _) (stream (syntax->datum this-syntax))])))
    (define expected '((cons x y) (cons a b) (cons c d)))
    (check-equal? actual expected)))


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
