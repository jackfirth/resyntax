#lang resyntax/test


require: resyntax/default-recommendations list-shortcuts


header:
- #lang racket/base


test: "car reverse of list not refactorable to last of list, due to imports (see issue #11)"
- (car (reverse (list 1 2 3)))


test: "first reverse of list refactorable to last of list"
------------------------------
(require racket/list)
(first (reverse (list 1 2 3)))
------------------------------
------------------------------
(require racket/list)
(last (list 1 2 3))
------------------------------


test: "comparison to empty list refactorable to use of null? predicate"
- (eq? (list 1 2 3) '())
- (eq? (list 1 2 3) (list))
- (eq? (list 1 2 3) null)
- (eqv? (list 1 2 3) '())
- (eqv? (list 1 2 3) (list))
- (eqv? (list 1 2 3) null)
- (equal? (list 1 2 3) '())
- (equal? (list 1 2 3) (list))
- (equal? (list 1 2 3) null)
- (null? (list 1 2 3))


test: "(append* (map ...)) refactorable to single-pass append-map"
------------------------------
(require racket/list)
(define (f x)
  (list x x x))
(append* (map f (list 1 2 3)))
------------------------------
------------------------------
(require racket/list)
(define (f x)
  (list x x x))
(append-map f (list 1 2 3))
------------------------------


test: "single-list append removable"
- (append (list 1 2 3))
- (list 1 2 3)


test: "filter with andmap and equal? to intersect lists refactorable to remove*"
------------------------------
(define old-ids '(a b c))
(define new-ids '(b c d e))
(filter (λ (id)
          (andmap (λ (id2) (not (equal? id id2)))
                  old-ids))
        new-ids)
------------------------------
------------------------------
(define old-ids '(a b c))
(define new-ids '(b c d e))
(remove* old-ids new-ids)
------------------------------


test: "filter with andmap and eqv? to intersect lists refactorable to remv*"
------------------------------
(define old-ids '(a b c))
(define new-ids '(b c d e))
(filter (λ (id)
          (andmap (λ (id2) (not (eqv? id id2)))
                  old-ids))
        new-ids)
------------------------------
------------------------------
(define old-ids '(a b c))
(define new-ids '(b c d e))
(remv* old-ids new-ids)
------------------------------


test: "filter with andmap and eq? to intersect lists refactorable to remq*"
------------------------------
(define old-ids '(a b c))
(define new-ids '(b c d e))
(filter (λ (id)
          (andmap (λ (id2) (not (eq? id id2)))
                  old-ids))
        new-ids)
------------------------------
------------------------------
(define old-ids '(a b c))
(define new-ids '(b c d e))
(remq* old-ids new-ids)
------------------------------


test: "sort by comparator using key refactorable to sort by key"
------------------------------
(define (f x)
  42)
(sort (list 1 2 3) (λ (a b) (< (f a) (f b))))
------------------------------
------------------------------
(define (f x)
  42)
(sort (list 1 2 3) < #:key f)
------------------------------


test: "unnecessary quasiquotation refactorable to list"
------------------------------
(define (f x y z)
  `(,x ,y ,z))
------------------------------
------------------------------
(define (f x y z)
  (list x y z))
------------------------------


test: "unnecessary quasiquotation with constants refactorable to list"
------------------------------
(define (f x y z)
  `(,x 1 ,y 2 ,z 3))
------------------------------
------------------------------
(define (f x y z)
  (list x 1 y 2 z 3))
------------------------------


test: "unnecessary splicing quasiquotation refactorable to append"
------------------------------
(define (f xs ys zs)
  `(,@xs ,@ys ,@zs))
------------------------------
------------------------------
(define (f xs ys zs)
  (append xs ys zs))
------------------------------


test: "splicing quasiquotation with other subterms not refactorable to append"
------------------------------
(define (f xs ys zs)
  `(a ,@xs b ,@ys c ,@zs d))
------------------------------


test: "ignored map expression refactorable to for-each"
------------------------------
(define (f func xs ys zs)
  ; comment before
  (map func xs ys zs)
  ; comment after
  (displayln "foo"))
------------------------------
------------------------------
(define (f func xs ys zs)
  ; comment before
  (for-each func xs ys zs)
  ; comment after
  (displayln "foo"))
------------------------------


test: "used map expression not refactorable to for-each"
------------------------------
(define (f func xs ys zs)
  (map func xs ys zs))
------------------------------


test: "build-list with const refactorable to make-list"
------------------------------
(require racket/function
         racket/list)
(build-list 5 (const 42))
------------------------------
------------------------------
(require racket/function
         racket/list)
(make-list 5 42)
------------------------------


test: "list of contiguous selections to take and drop"
------------------------------
(require racket/list)
(define vs (list 'foo 'bar 'baz 'blah 'zorp 'zoog 'karp))
(list (list-ref vs 2) (list-ref vs 3) (list-ref vs 4))
------------------------------
------------------------------
(require racket/list)
(define vs (list 'foo 'bar 'baz 'blah 'zorp 'zoog 'karp))
(list (third vs) (fourth vs) (fifth vs))
------------------------------
------------------------------
(require racket/list)
(define vs (list 'foo 'bar 'baz 'blah 'zorp 'zoog 'karp))
(list (caddr vs) (cadddr vs) (list-ref vs 4))
------------------------------
------------------------------
(require racket/list)
(define vs (list 'foo 'bar 'baz 'blah 'zorp 'zoog 'karp))
(take (drop vs 2) 3)
------------------------------


test: "list of contiguous selections starting at first element to take"
------------------------------
(require racket/list)
(define vs (list 'foo 'bar 'baz 'blah 'zorp 'zoog 'karp))
(list (list-ref vs 0) (list-ref vs 1) (list-ref vs 2))
------------------------------
------------------------------
(require racket/list)
(define vs (list 'foo 'bar 'baz 'blah 'zorp 'zoog 'karp))
(list (first vs) (second vs) (third vs))
------------------------------
------------------------------
(require racket/list)
(define vs (list 'foo 'bar 'baz 'blah 'zorp 'zoog 'karp))
(list (car vs) (cadr vs) (caddr vs))
------------------------------
------------------------------
(require racket/list)
(define vs (list 'foo 'bar 'baz 'blah 'zorp 'zoog 'karp))
(take vs 3)
------------------------------


test: "list of only two contiguous selections not refactorable to take and drop"
------------------------------
(require racket/list)
(define vs (list 'foo 'bar 'baz 'blah 'zorp 'zoog 'karp))
(list (list-ref vs 2) (list-ref vs 3))
------------------------------


test: "consing onto static proper list expression can be simplified"
- (cons 1 (list 2 3 4 5))
- (cons 1 (cons 2 (cons 3 (list 4 5))))
- (list* 1 2 3 (list 4 5))
- (list* 1 2 3 4 5 (list))
- (cons 1 (list* 2 3 4 (list 5)))
- (list* 1 2 3 (cons 4 (list 5)))
- (list* 1 2 (list* 3 4 (list 5)))
- (list* (list 1 2 3 4 5))
- (list 1 2 3 4 5)


test: "consing onto static improper list expression can be simplified"
- (cons 1 (list* 2 3 4 5))
- (cons 1 (cons 2 (cons 3 (list* 4 5))))
- (list* 1 2 3 (list* 4 5))
- (cons 1 (list* 2 3 4 (list* 5)))
- (list* 1 2 3 (cons 4 (list* 5)))
- (list* 1 2 (list* 3 4 (list* 5)))
- (list* (list* 1 2 3 4 5))
- (list* 1 2 3 4 5)


test: "comparing length to zero refactorable to empty check"
------------------------------
(require racket/list)
(equal? (length (list 1 2 3)) 0)
------------------------------
------------------------------
(require racket/list)
(eqv? (length (list 1 2 3)) 0)
------------------------------
------------------------------
(require racket/list)
(eq? (length (list 1 2 3)) 0)
------------------------------
------------------------------
(require racket/list)
(= (length (list 1 2 3)) 0)
------------------------------
------------------------------
(require racket/list)
(equal? 0 (length (list 1 2 3)))
------------------------------
------------------------------
(require racket/list)
(eqv? 0 (length (list 1 2 3)))
------------------------------
------------------------------
(require racket/list)
(eq? 0 (length (list 1 2 3)))
------------------------------
------------------------------
(require racket/list)
(= 0 (length (list 1 2 3)))
------------------------------
------------------------------
(require racket/list)
(zero? (length (list 1 2 3)))
------------------------------
------------------------------
(require racket/list)
(empty? (list 1 2 3))
------------------------------
