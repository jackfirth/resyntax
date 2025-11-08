#lang resyntax/test


require: resyntax/default-recommendations let-binding-suggestions


header:
- #lang racket/base
test: "named lets which don't refer to the name are refactorable to unnamed lets"
- (let loop ([x 1]) x)
- (let ([x 1]) x)


no-change-test: "named lets which do refer to the name aren't refactorable to unnamed lets"
------------------------------
(let loop ([x 1])
  (if (zero? x)
      x
      (loop (sub1 x))))
------------------------------


test: "let-values expressions with an immediate call are refactorable to call-with-values"
- (let-values ([(x y z) (values 1 2 3)]) (list x y z))
- (call-with-values (λ () (values 1 2 3)) list)


no-change-test:
"let-values expressions with an immediate call with different order aren't refactorable"
- (let-values ([(x y z) (values 1 2 3)]) (list z y x))
test: "redundant let bindings can be removed"
------------------------------
(define x 1)
(let ([x x])
  (* x 2))
==============================
(define x 1)
(* x 2)
------------------------------
