#lang resyntax/testing/refactoring-test


require: resyntax/default-recommendations conditional-shortcuts


header:
- #lang racket/base


test: "if without nested ifs not refactorable"
- (if 'cond 'then 'else)


test: "singly-nested ifs refactorable to cond"
- (if 'cond 'then (if 'cond2 'then2 'else))
------------------------------
(cond
  ['cond 'then]
  ['cond2 'then2]
  [else 'else])
------------------------------


test: "nested ifs refactorable to cond"
- (if 'a 'b (if 'c 'd (if 'e 'f (if 'g 'h 'i))))
------------------------------
(if 'a
    'b
    (if 'c
        'd
        (if 'e
            'f
            (if 'g
                'h
                'i))))
------------------------------
------------------------------
(cond
  ['a 'b]
  ['c 'd]
  ['e 'f]
  ['g 'h]
  [else 'i])
------------------------------


test: "if else false can be refactored to an and expression"
- (if 'a (println "true branch") #f)
- (and 'a (println "true branch"))


test: "multi-line if else false can be refactored to a multi-line and expression"
------------------------------
(if 'a
    (println "true branch")
    #f)
------------------------------
------------------------------
(and 'a
     (println "true branch"))
------------------------------


test: "if x else x can be refactored to an and expression"
------------------------------
(define x 'a)
(if x (println "true branch") x)
------------------------------
------------------------------
(define x 'a)
(and x (println "true branch"))
------------------------------


test: "multi-line if x else x can be refactored to a multi-line and expression"
------------------------------
(define x 'a)
(if x
    (println "true branch")
    x)
------------------------------
------------------------------
(define x 'a)
(and x
     (println "true branch"))
------------------------------


test: "if expressions can be refactored to when expressions when equivalent"
------------------------------
(if #true
    (begin
      (println "first line")
      ;; preserved comment
      (println "second line"))
    (void))
------------------------------
------------------------------
(if (not #true)
    (void)
    (begin
      (println "first line")
      ;; preserved comment
      (println "second line")))
------------------------------
------------------------------
(when #true
  (println "first line")
  ;; preserved comment
  (println "second line"))
------------------------------


test: "if expressions can be refactored to unless expressions when equivalent"
------------------------------
(if #false
    (void)
    (begin
      (println "first line")
      ;; preserved comment
      (println "second line")))
------------------------------
------------------------------
(if (not #false)
    (begin
      (println "first line")
      ;; preserved comment
      (println "second line"))
    (void))
------------------------------
------------------------------
(unless #false
  (println "first line")
  ;; preserved comment
  (println "second line"))
------------------------------


test: "if expressions with an always-throwing first branch can be refactored to when"
------------------------------
(define (f c)
  (if c
      (error 'oops)
      (displayln "foo")))
------------------------------
------------------------------
(define (f c)
  (when c
    (error 'oops))
  (displayln "foo"))
------------------------------


test: "negated if expressions with an always-throwing first branch can be refactored to unless"
------------------------------
(define (f c)
  (if (not c)
      (error 'oops)
      (displayln "foo")))
------------------------------
------------------------------
(define (f c)
  (unless c
    (error 'oops))
  (displayln "foo"))
------------------------------


test: "cond expressions with an always-throwing first branch can be refactored to when"
------------------------------
(define (f c)
  (cond
    [c
     (error 'oops)]
    [else
     (displayln "foo")
     (displayln "bar")]))
------------------------------
------------------------------
(define (f c)
  (when c
    (error 'oops))
  (displayln "foo")
  (displayln "bar"))
------------------------------


test: "cond expressions with an always-throwing negated first branch can be refactored to unless"
------------------------------
(define (f c)
  (cond
    [(not c)
     (error 'oops)]
    [else
     (displayln "foo")
     (displayln "bar")]))
------------------------------
------------------------------
(define (f c)
  (unless c
    (error 'oops))
  (displayln "foo")
  (displayln "bar"))
------------------------------


test: "cond expressions with an always-throwing first branch (of multiple) can't be refactored"
------------------------------
(define (f condition1 condition2 condition3)
  (cond
    [condition1
     (error 'oops)]
    [condition2
     1]
    [condition3
     2]))
------------------------------


test: "cond with nested else-cond can be flattened"
------------------------------
(define (f a b)
  (cond
    [a
     (displayln "a")]
    [else
     (cond
       [b
        (displayln "b")]
       [else
        (displayln "else")])]))
------------------------------
------------------------------
(define (f a b)
  (cond
    [a
     (displayln "a")]
    [b
     (displayln "b")]
    [else
     (displayln "else")]))
------------------------------


test: "flattening nested else-cond preserves single-line formatting"
------------------------------
(define (f a b)
  (cond [a 1] [else (cond [b 2] [else 3])]))
------------------------------
------------------------------
(define (f a b)
  (cond [a 1] [b 2] [else 3]))
------------------------------


test: "cond with nested cond in last clause without else can't be flattened"
------------------------------
(define (f a b last-condition)
  (cond
    [a
     (displayln "a")]
    [last-condition
     (cond
       [b
        (displayln "b")]
       [else
        (displayln "else")])]))
------------------------------
