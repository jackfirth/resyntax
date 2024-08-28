#lang resyntax/testing/refactoring-test


require: resyntax/default-recommendations/gap-preservation gap-preservation-rules


header:
- #lang racket/base


test: "comments preserved in splice when form inserted at front"
-----------------------------------
(define (code insert-foo-first a b)
  (insert-foo-first a
                    ; comment
                    b))
-----------------------------------
-----------------------------------
(define (code insert-foo-first a b)
  ("foo" a
         ; comment
         b))
-----------------------------------


test: "later comments preserved in splice when form inserted after first"
-----------------------------------
(define (code insert-foo-second a b c)
  (insert-foo-second a
                     b
                     ; preserved comment
                     c))
-----------------------------------
-----------------------------------
(define (code insert-foo-second a b c)
  (a "foo"
     b
     ; preserved comment
     c))
-----------------------------------


test: "not refactorable when comment dropped due to inserted form"
-----------------------------------
(define (code insert-foo-second a b c)
  (insert-foo-second a
                     ; dropped comment
                     b
                     c))
-----------------------------------


test: "comments preserved in splice when form inserted at end"
-----------------------------------
(define (code insert-foo-last a b c)
  (insert-foo-last a
                   b
                   ; comment
                   c))
-----------------------------------
-----------------------------------
(define (code insert-foo-last a b c)
  (a b
     ; comment
     c
     "foo"))
-----------------------------------
