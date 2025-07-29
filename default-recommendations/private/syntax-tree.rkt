#lang racket/base

;; This module defines a syntax-tree syntax class for recognizing trees of nested expressions. It's
;; primarily intended for implementing various tree-flattening refactoring rules that rewrite forms
;; like (or a b (or c d) e) to (or a b c d e). The syntax class accepts an identifier that determines
;; what subforms count as branches, and it parses out the leaves of the tree. It also makes the rank
;; of the tree (the number of levels in the tree) available as an attribute, so that the flattening
;; refactoring rules can avoid flattening trees that are already flat.

(provide syntax-tree)

(require rebellion/base/option
         rebellion/streaming/reducer
         rebellion/streaming/transducer
         syntax/parse)

(module+ test
  (require rackunit
           (submod "..")))

;@----------------------------------------------------------------------------------------------------

(define-syntax-class (syntax-tree branch-identifier)
  #:attributes ([leaf 1] rank)
  #:commit

  (pattern (id:id subtree ...)
    #:declare subtree (syntax-tree branch-identifier)
    #:when (free-identifier=? #'id branch-identifier)
    #:cut
    #:with (leaf ...) #'(subtree.leaf ... ...)
    #:attr rank (add1 (option-get (transduce (attribute subtree.rank) #:into (into-max)) 0)))

  (pattern other
    #:with (leaf ...) #'(other)
    #:attr rank 0))

(module+ test
  (test-case "expression-tree"

    (define (leaves stx)
      (syntax-parse stx
        [expr
         #:declare expr (syntax-tree #'or)
         (syntax->datum #'(expr.leaf ...))]))

    (define (rank stx)
      (syntax-parse stx
        [expr
         #:declare expr (syntax-tree #'or)
         (attribute expr.rank)]))

    (test-case "no outer form"
      (define stx #'foo)
      (check-equal? (leaves stx) '(foo))
      (check-equal? (rank stx) 0))

    (test-case "unrelated outer form"
      (define stx #'(foo a b c))
      (check-equal? (leaves stx) '((foo a b c)))
      (check-equal? (rank stx) 0))

    (test-case "empty outer form"
      (define stx #'(or))
      (check-equal? (leaves stx) '())
      (check-equal? (rank stx) 1))

    (test-case "flat outer form"
      (define stx #'(or a b c))
      (check-equal? (leaves stx) '(a b c))
      (check-equal? (rank stx) 1))

    (test-case "empty subform in empty outer form"
      (define stx #'(or (or)))
      (check-equal? (leaves stx) '())
      (check-equal? (rank stx) 2))

    (test-case "empty subform"
      (define stx #'(or a (or) b))
      (check-equal? (leaves stx) '(a b))
      (check-equal? (rank stx) 2))

    (test-case "two singleton subforms"
      (define stx #'(or (or a) (or b)))
      (check-equal? (leaves stx) '(a b))
      (check-equal? (rank stx) 2))

    (test-case "two subforms"
      (define stx #'(or (or a b c) (or a b c)))
      (check-equal? (leaves stx) '(a b c a b c))
      (check-equal? (rank stx) 2))

    (test-case "deeply nested subforms"
      (define stx #'(or a (or b (or c (or d)))))
      (check-equal? (leaves stx) '(a b c d))
      (check-equal? (rank stx) 4))))
