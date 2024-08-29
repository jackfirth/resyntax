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


test: "comments preserved in splice when first form replaced"
-----------------------------------
(define (code replace-first-with-foo a b c)
  (replace-first-with-foo a
                          ; buggy comment after (sorawee/fmt#68)
                          b
                          c))
-----------------------------------
-----------------------------------
(define (code replace-first-with-foo a b c)
  ; buggy comment after (sorawee/fmt#68)
("foo" b c))
-----------------------------------


test: "comments preserved in splice when second form replaced"
-----------------------------------
(define (code replace-second-with-foo a b c)
  (replace-second-with-foo a
                           ; buggy comment before (sorawee/fmt#68)
                           b
                           ; comment after
                           c))
-----------------------------------
-----------------------------------
(define (code replace-second-with-foo a b c)
  ; buggy comment before (sorawee/fmt#68)
(a "foo"
   ; comment after
   c))
-----------------------------------


test: "comments preserved in splice when last form replaced"
-----------------------------------
(define (code replace-last-with-foo a b c)
  (replace-last-with-foo a
                         b
                         ; comment before
                         c))
-----------------------------------
-----------------------------------
(define (code replace-last-with-foo a b c)
  (a b
     ; comment before
     "foo"))
-----------------------------------


test: "comments preserved in splice when first and last forms replaced"
-----------------------------------
(define (code replace-first-and-last-with-foo a b c)
  (replace-first-and-last-with-foo a
                                   ; buggy comment after (sorawee/fmt#68)
                                   b
                                   ; comment before
                                   c))
-----------------------------------
-----------------------------------
(define (code replace-first-and-last-with-foo a b c)
  ; buggy comment after (sorawee/fmt#68)
("foo" b
       ; comment before
       "foo"))
-----------------------------------
