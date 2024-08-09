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
(if 'some-very-long-condition-that-is-so-very-long
    (println "some very long true branch that is so very long")
    #f)
------------------------------
------------------------------
(and 'some-very-long-condition-that-is-so-very-long
     (println "some very long true branch that is so very long"))
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
(define some-variable 'a)
(if some-variable
    (println "some very long true branch that is so very very very very very very very very long")
    some-variable)
------------------------------
------------------------------
(define some-variable 'a)
(and some-variable
     (println "some very long true branch that is so very very very very very very very very long"))
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
    [a (displayln "a")]
    [else
     (cond
       [b (displayln "b")]
       [else (displayln "else")])]))
------------------------------
------------------------------
(define (f a b)
  (cond
    [a (displayln "a")]
    [b (displayln "b")]
    [else (displayln "else")]))
------------------------------


test: "flattening nested else-cond does not preserve single-line formatting"
------------------------------
(define (f a b)
  (cond [a 1] [else (cond [b 2] [else 3])]))
------------------------------
------------------------------
(define (f a b)
  (cond
    [a 1]
    [b 2]
    [else 3]))
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


test: "`if-else-false-to-and` is not refactorable when `and` is shadowed"
------------------------------
(define (and x y) (list x y))
(if 'a (println "true branch") #f)
------------------------------


test: "cond with nested let refactorable to cond with define"
------------------------------
(define (f a b c)
  (cond
    [a
     (let ([x "stuff"])
       b)]
    [else c]))
------------------------------
------------------------------
(define (f a b c)
  (cond
    [a
     (define x "stuff")
     b]
    [else c]))
------------------------------


test: "cond with nested let in else clause refactorable to cond with define"
------------------------------
(define (f a b c)
  (cond
    [a b]
    [else
     (let ([x "stuff"])
       c)]))
------------------------------
------------------------------
(define (f a b c)
  (cond
    [a b]
    [else
     (define x "stuff")
     c]))
------------------------------


test: "if clause with begin in true branch refactorable to cond"
------------------------------
(define (f a b c)
  (if a
      (begin
        (displayln "stuff")
        b)
      c))
------------------------------
------------------------------
(define (f a b c)
  (cond
    [a
     (displayln "stuff")
     b]
    [else c]))
------------------------------


test: "if clause with begin in false branch refactorable to cond"
------------------------------
(define (f a b c)
  (if a
      b
      (begin
        (displayln "stuff")
        c)))
------------------------------
------------------------------
(define (f a b c)
  (cond
    [a b]
    [else
     (displayln "stuff")
     c]))
------------------------------


test: "if clause with begin in both branches refactorable to cond"
------------------------------
(define (f a b c)
  (if a
      (begin
        (displayln "stuff")
        b)
      (begin
        (displayln "stuff")
        c)))
------------------------------
------------------------------
(define (f a b c)
  (cond
    [a
     (displayln "stuff")
     b]
    [else
     (displayln "stuff")
     c]))
------------------------------


test: "if clause with multiline condition and begin in true branch refactorable to cond"
------------------------------
(define (f a b c)
  (if (a 10000000000000000000000000000000000000
         20000000000000000000000000000000000000
         30000000000000000000000000000000000000)
      (begin
        (displayln "stuff")
        b)
      c))
------------------------------
------------------------------
(define (f a b c)
  (cond
    [(a 10000000000000000000000000000000000000
        20000000000000000000000000000000000000
        30000000000000000000000000000000000000)
     (displayln "stuff")
     b]
    [else c]))
------------------------------


test: "if clause with multiline condition and begin in false branch refactorable to cond"
------------------------------
(define (f a b c)
  (if (a 10000000000000000000000000000000000000
         20000000000000000000000000000000000000
         30000000000000000000000000000000000000)
      b
      (begin
        (displayln "stuff")
        c)))
------------------------------
------------------------------
(define (f a b c)
  (cond
    [(a 10000000000000000000000000000000000000
        20000000000000000000000000000000000000
        30000000000000000000000000000000000000)
     b]
    [else
     (displayln "stuff")
     c]))
------------------------------


test: "if clause with multiline condition and begin in both branches refactorable to cond"
------------------------------
(define (f a b c)
  (if (a 10000000000000000000000000000000000000
         20000000000000000000000000000000000000
         30000000000000000000000000000000000000)
      (begin
        (displayln "stuff")
        b)
      (begin
        (displayln "stuff")
        c)))
------------------------------
------------------------------
(define (f a b c)
  (cond
    [(a 10000000000000000000000000000000000000
        20000000000000000000000000000000000000
        30000000000000000000000000000000000000)
     (displayln "stuff")
     b]
    [else
     (displayln "stuff")
     c]))
------------------------------


test: "if clause with begin in commented true branch refactorable to cond"
------------------------------
(define (f a b c)
  (if a
      ;; This is the true case
      (begin
        (displayln "stuff")
        b)
      c))
------------------------------
------------------------------
(define (f a b c)
  (cond
    [a
     ;; This is the true case
     (displayln "stuff")
     b]
    [else c]))
------------------------------


test: "if clause with begin in commented false branch refactorable to cond"
------------------------------
(define (f a b c)
  (if a
      b
      ;; This is the false case
      (begin
        (displayln "stuff")
        c)))
------------------------------
------------------------------
(define (f a b c)
  (cond
    [a b]
    [else
     ;; This is the false case
     (displayln "stuff")
     c]))
------------------------------


test: "if clause with begin in both commented branches refactorable to cond"
------------------------------
(define (f a b c)
  (if a
      ;; This is the true case
      (begin
        (displayln "stuff")
        b)
      ;; This is the false case
      (begin
        (displayln "stuff")
        c)))
------------------------------
------------------------------
(define (f a b c)
  (cond
    [a
     ;; This is the true case
     (displayln "stuff")
     b]
    [else
     ;; This is the false case
     (displayln "stuff")
     c]))
------------------------------



test: "if clause with begin in true branch and commented false branch refactorable to cond"
------------------------------
(define (f a b c)
  (if a
      (begin
        (displayln "stuff")
        b)
      ;; This is the false case
      c))
------------------------------
------------------------------
(define (f a b c)
  (cond
    [a
     (displayln "stuff")
     b]
    ;; This is the false case
    [else c]))
------------------------------


test: "if clause with begin in false branch and commented true branch refactorable to cond"
------------------------------
(define (f a b c)
  (if a
      ;; This is the true case
      b
      (begin
        (displayln "stuff")
        c)))
------------------------------
------------------------------
(define (f a b c)
  (cond
    ;; This is the true case
    [a b]
    [else
     (displayln "stuff")
     c]))
------------------------------


test: "if clause with let in true branch refactorable to cond"
------------------------------
(define (f a b c)
  (if a
      (let ([x 1])
        b)
      c))
------------------------------
------------------------------
(define (f a b c)
  (cond
    [a
     (define x 1)
     b]
    [else c]))
------------------------------


test: "if clause with let in false branch refactorable to cond"
------------------------------
(define (f a b c)
  (if a
      b
      (let ([x 1])
        c)))
------------------------------
------------------------------
(define (f a b c)
  (cond
    [a b]
    [else
     (define x 1)
     c]))
------------------------------


test: "if clause with let in both branches refactorable to cond"
------------------------------
(define (f a b c)
  (if a
      (let ([x 1])
        b)
      (let ([x 1])
        c)))
------------------------------
------------------------------
(define (f a b c)
  (cond
    [a
     (define x 1)
     b]
    [else
     (define x 1)
     c]))
------------------------------


test: "if clause with multiline condition and let in true branch refactorable to cond"
------------------------------
(define (f a b c)
  (if (a 10000000000000000000000000000000000000
         20000000000000000000000000000000000000
         30000000000000000000000000000000000000)
      (let ([x 1])
        b)
      c))
------------------------------
------------------------------
(define (f a b c)
  (cond
    [(a 10000000000000000000000000000000000000
        20000000000000000000000000000000000000
        30000000000000000000000000000000000000)
     (define x 1)
     b]
    [else c]))
------------------------------


test: "if clause with multiline condition and let in false branch refactorable to cond"
------------------------------
(define (f a b c)
  (if (a 10000000000000000000000000000000000000
         20000000000000000000000000000000000000
         30000000000000000000000000000000000000)
      b
      (let ([x 1])
        c)))
------------------------------
------------------------------
(define (f a b c)
  (cond
    [(a 10000000000000000000000000000000000000
        20000000000000000000000000000000000000
        30000000000000000000000000000000000000)
     b]
    [else
     (define x 1)
     c]))
------------------------------


test: "if clause with multiline condition and let in both branches refactorable to cond"
------------------------------
(define (f a b c)
  (if (a 10000000000000000000000000000000000000
         20000000000000000000000000000000000000
         30000000000000000000000000000000000000)
      (let ([x 1])
        b)
      (let ([x 1])
        c)))
------------------------------
------------------------------
(define (f a b c)
  (cond
    [(a 10000000000000000000000000000000000000
        20000000000000000000000000000000000000
        30000000000000000000000000000000000000)
     (define x 1)
     b]
    [else
     (define x 1)
     c]))
------------------------------


test: "if clause with let in commented true branch refactorable to cond"
------------------------------
(define (f a b c)
  (if a
      ;; This is the true case
      (let ([x 1])
        b)
      c))
------------------------------
------------------------------
(define (f a b c)
  (cond
    [a
     ;; This is the true case
     (define x 1)
     b]
    [else c]))
------------------------------


test: "if clause with let in commented false branch refactorable to cond"
------------------------------
(define (f a b c)
  (if a
      b
      ;; This is the false case
      (let ([x 1])
        c)))
------------------------------
------------------------------
(define (f a b c)
  (cond
    [a b]
    [else
     ;; This is the false case
     (define x 1)
     c]))
------------------------------


test: "if clause with let in both commented branches refactorable to cond"
------------------------------
(define (f a b c)
  (if a
      ;; This is the true case
      (let ([x 1])
        b)
      ;; This is the false case
      (let ([x 1])
        c)))
------------------------------
------------------------------
(define (f a b c)
  (cond
    [a
     ;; This is the true case
     (define x 1)
     b]
    [else
     ;; This is the false case
     (define x 1)
     c]))
------------------------------



test: "if clause with let in true branch and commented false branch refactorable to cond"
------------------------------
(define (f a b c)
  (if a
      (let ([x 1])
        b)
      ;; This is the false case
      c))
------------------------------
------------------------------
(define (f a b c)
  (cond
    [a
     (define x 1)
     b]
    ;; This is the false case
    [else c]))
------------------------------


test: "if clause with let in false branch and commented true branch refactorable to cond"
------------------------------
(define (f a b c)
  (if a
      ;; This is the true case
      b
      (let ([x 1])
        c)))
------------------------------
------------------------------
(define (f a b c)
  (cond
    ;; This is the true case
    [a b]
    [else
     (define x 1)
     c]))
------------------------------
