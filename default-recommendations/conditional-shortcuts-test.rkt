#lang resyntax/test


require: resyntax/default-recommendations conditional-shortcuts


header:
- #lang racket/base


no-change-test: "if without nested ifs not refactorable"
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
==============================
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
==============================
(and 'some-very-long-condition-that-is-so-very-long
     (println "some very long true branch that is so very long"))
------------------------------


test: "if and else false can be refactored to a single and expression"
- (if (and 'a 'b) 'c #f)
- (and 'a 'b 'c)


test: "if x else x can be refactored to an and expression"
------------------------------
(define x 'a)
(if x (println "true branch") x)
==============================
(define x 'a)
(and x (println "true branch"))
------------------------------


test: "multi-line if x else x can be refactored to a multi-line and expression"
------------------------------
(define some-variable 'a)
(if some-variable
    (println "some very long true branch that is so very very very very very very very very long")
    some-variable)
==============================
(define some-variable 'a)
(and some-variable
     (println "some very long true branch that is so very very very very very very very very long"))
------------------------------


test: "if x then and expression else x can be refactored to a single and expression"
------------------------------
(define x 'a)
(if x (and 'b 'c 'd) x)
==============================
(define x 'a)
(and x 'b 'c 'd)
------------------------------


test: "if expressions can be refactored to when expressions when equivalent"
------------------------------
(if #true
    (begin
      (println "first line")
      ;; preserved comment
      (println "second line"))
    (void))
==============================
(if (not #true)
    (void)
    (begin
      (println "first line")
      ;; preserved comment
      (println "second line")))
==============================
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
==============================
(if (not #false)
    (begin
      (println "first line")
      ;; preserved comment
      (println "second line"))
    (void))
==============================
(unless #false
  (println "first line")
  ;; preserved comment
  (println "second line"))
------------------------------


test: "cond expressions can be refactored to when expressions when equivalent"
------------------------------
(cond
  [#true
   (begin
     (println "first line")
     ;; preserved comment
     (println "second line"))]
  [else (void)])
==============================
(cond
  [(not #true) (void)]
  [else
   (begin
     (println "first line")
     ;; preserved comment
     (println "second line"))])
==============================
(when #true
  (println "first line")
  ;; preserved comment
  (println "second line"))
------------------------------


test: "cond expressions can be refactored to unless expressions when equivalent"
------------------------------
(cond
  [#false (void)]
  [else
   (begin
      (println "first line")
      ;; preserved comment
      (println "second line"))])
==============================
(cond
  [(not #false)
   (begin
     (println "first line")
     ;; preserved comment
     (println "second line"))]
  [else (void)])
==============================
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
==============================
(define (f c)
  (when c
    (error 'oops))
  (displayln "foo"))
------------------------------


test: "if expressions with an always-throwing second branch can be refactored to unless"
------------------------------
(define (f c)
  (if c
      (displayln "foo")
      (error 'oops)))
==============================
(define (f c)
  (unless c
    (error 'oops))
  (displayln "foo"))
------------------------------


test: "negated if expressions with an always-throwing first branch can be refactored to unless"
------------------------------
(define (f c)
  (if (not c)
      (error 'oops)
      (displayln "foo")))
==============================
(define (f c)
  (unless c
    (error 'oops))
  (displayln "foo"))
------------------------------


test: "negated if expressions with an always-throwing second branch can be refactored to when"
------------------------------
(define (f c)
  (if (not c)
      (displayln "foo")
      (error 'oops)))
==============================
(define (f c)
  (when c
    (error 'oops))
  (displayln "foo"))
------------------------------


test: "refactoring always-throwing if expressions to when doesn't reformat entire context"
------------------------------
(define (f c)

  ( displayln   "foo" )

  (if c
      (error 'oops)
      (displayln "foo")))
==============================
(define (f c)

  ( displayln   "foo" )

  (when c
    (error 'oops))
  (displayln "foo"))
------------------------------


test: "if expressions inside cond with an always-throwing first branch can be refactored to when"
------------------------------
(define (f c1 c2)
  (cond
    [c1
     (if c2
         (error 'oops)
         (displayln "foo"))]
    [else (displayln "else")]))
==============================
(define (f c1 c2)
  (cond
    [c1
     (when c2
       (error 'oops))
     (displayln "foo")]
    [else (displayln "else")]))
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
==============================
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
==============================
(define (f c)
  (unless c
    (error 'oops))
  (displayln "foo")
  (displayln "bar"))
------------------------------


no-change-test:
"cond expressions with an always-throwing first branch (of multiple) can't be refactored"
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


test:
"cond expressions with multiple always-throwing branches and an else branch can be refactored"
------------------------------
(define (f condition1 condition2)
  (cond
    [condition1 (error 'oops1)]
    [condition2 (error 'oops2)]
    [else 2]))
==============================
(define (f condition1 condition2)
  (when condition1
    (error 'oops1))
  (when condition2
    (error 'oops2))
  2)
------------------------------


test: "cond expressions inside cond with an always-throwing first branch can be refactored to when"
------------------------------
(define (f c1 c2)
  (cond
    [c1
     (cond
       [c2
        (error 'oops)]
       [else
        (displayln "foo")
        (displayln "bar")])]
    [else (displayln "else")]))
==============================
(define (f c1 c2)
  (cond
    [c1
     (when c2
       (error 'oops))
     (displayln "foo")
     (displayln "bar")]
    [else (displayln "else")]))
------------------------------


test: "refactoring always-throwing cond expressions to when doesn't reformat entire context"
------------------------------
(define (f c)
  
  ( displayln   "foo" )
  
  (cond
    [c
     (error 'oops)]
    [else
     (displayln "foo")
     (displayln "bar")]))
==============================
(define (f c)
  
  ( displayln   "foo" )
  
  (when c
    (error 'oops))
  (displayln "foo")
  (displayln "bar"))
------------------------------


test: "always-throwing unless before returning condition variable refactorable to or"
--------------------
(define (f s)
  (define x (string->number s))
  (unless x
   (error "string that is not a num"))
  x)
====================
(define (f s)
  (or (string->number s) (error "string that is not a num")))
--------------------


test: "refactoring always-throwing unless to or doesn't reformat surrounding context"
--------------------
(define (f s)

  ( define foo 42 )
  
  (define x (string->number s))
  (unless x
    (error "string that is not a num"))
  x)
====================
(define (f s)

  ( define foo 42 )
  
  (or (string->number s) (error "string that is not a num")))
--------------------


test: "refactoring always-throwing unless to or can reformat when its the only body"
--------------------
(define (f s)
  (λ (v)
    (define x (string->number s))
    (unless x
      (error "string that is not a num"))
    x))
====================
(define (f s)
  (λ (v) (or (string->number s) (error "string that is not a num"))))
--------------------


test: "cond with nested else-cond can be flattened"
------------------------------
(define (f a b)
  (cond
    [a (displayln "a")]
    [else
     (cond
       [b (displayln "b")]
       [else (displayln "else")])]))
==============================
(define (f a b)
  (cond
    [a (displayln "a")]
    [b (displayln "b")]
    [else (displayln "else")]))
------------------------------


test: "cond with commented nested else-cond can be flattened"
------------------------------
(define (f a b)
  (cond
    [a (displayln "a")]
    ; comment
    [else
     (cond
       [b (displayln "b")]
       [else (displayln "else")])]))
==============================
(define (f a b)
  (cond
    [a (displayln "a")]
    ; comment
    [b (displayln "b")]
    [else (displayln "else")]))
------------------------------


test: "cond with nested else-cond with commented second clause can be flattened"
------------------------------
(define (f a b)
  (cond
    [a (displayln "a")]
    [else
     (cond
       [b (displayln "b")]
       ; comment
       [else (displayln "else")])]))
==============================
(define (f a b)
  (cond
    [a (displayln "a")]
    [b (displayln "b")]
    ; comment
    [else (displayln "else")]))
------------------------------


no-change-test: "cond with nested cond in last clause without else can't be flattened"
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


no-change-test: "`if-else-false-to-and` is not refactorable when `and` is shadowed"
------------------------------
(define (and x y) (list x y))
(if 'a (println "true branch") #f)
------------------------------



test: "if clause with begin in true branch refactorable to cond"
------------------------------
(define (f a b c)
  (if a
      (begin
        (displayln "stuff")
        b)
      c))
==============================
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
==============================
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
==============================
(define (f a b c)
  (cond
    [a
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
==============================
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
==============================
(define (f a b c)
  (cond
    [a b]
    ;; This is the false case
    [else
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
==============================
(define (f a b c)
  (cond
    [a
     ;; This is the true case
     (displayln "stuff")
     b]
    ;; This is the false case
    [else
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
==============================
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
==============================
(define (f a b c)
  (cond
    ;; This is the true case
    [a b]
    [else
     (displayln "stuff")
     c]))
------------------------------


test: "immediately-nested when expressions can be merged"
--------------------
(define (f c1 c2)
  (when c1
    (when c2
      (displayln "both passed"))))
====================
(define (f c1 c2)
  (when (and c1 c2)
    (displayln "both passed")))
--------------------


test: "nested when with multiple body expressions can be merged"
--------------------
(define (f c1 c2)
  (when c1
    (when c2
      (displayln "first")
      (displayln "second"))))
====================
(define (f c1 c2)
  (when (and c1 c2)
    (displayln "first")
    (displayln "second")))
--------------------


no-change-test: "when with multiple forms in outer body should not be merged"
--------------------
(define (f c1 c2)
  (when c1
    (displayln "before")
    (when c2
      (displayln "nested"))))
--------------------


test: "ignored and expression refactorable to when expression"
--------------------
(define (f c1)
  (and c1 (displayln "foo"))
  42)
====================
(define (f c1)
  (when c1
    (displayln "foo"))
  42)
--------------------


test: "implicit else in ignored cond refactorable to explicit else void"
--------------------
(define (f c1 c2 result)
  (cond
   [c1 (displayln "condition 1")]
   [c2 (displayln "condition 2")])
  result)
====================
(define (f c1 c2 result)
  (cond
    [c1 (displayln "condition 1")]
    [c2 (displayln "condition 2")]
    [else (void)])
  result)
--------------------


no-change-test: "implicit else in used cond not refactorable to explicit else void"
------------------------------
(define (f c1 c2)
  (cond
   [c1 (displayln "condition 1")]
   [c2 (displayln "condition 2")]))
------------------------------


no-change-test: "cond with existing else clause not refactorable"
------------------------------
(define (f c1 c2)
  (cond
   [c1 (displayln "condition 1")]
   [c2 (displayln "condition 2")]
   [else #f])
  42)
------------------------------


test: "cond in ignored context with single clause refactorable"
--------------------
(define (f c1)
  (cond
   [c1 (displayln "condition 1")])
  42)
====================
(define (f c1)
  (cond
    [c1 (displayln "condition 1")]
    [else (void)])
  42)
--------------------


test: "cond with shared tail expression refactorable to when"
--------------------
(define (f c1)
  (cond
    [c1
     (displayln "condition 1")
     (displayln "shared tail")]
    [else
     (displayln "shared tail")]))
====================
(define (f c1)
  (when c1
    (displayln "condition 1"))
  (displayln "shared tail"))
--------------------


no-change-test: "cond with unshared tail expression not refactorable to when"
--------------------
(define (f c1)
  (cond
    [c1
     (displayln "condition 1")
     (displayln "true tail")]
    [else
     (displayln "false tail")]))
--------------------


test: "if with else cond can be flattened to cond"
- (if 'a 'b (cond ['c 'd] ['e 'f]))
------------------------------
(cond
  ['a 'b]
  ['c 'd]
  ['e 'f])
------------------------------


test: "cond with else-if can be collapsed"
- (cond ['a 'b] ['c 'd] [else (if 'e 'f 'g)])
------------------------------
(cond
  ['a 'b]
  ['c 'd]
  ['e 'f]
  [else 'g])
------------------------------


test: "cond with begin in clause can be simplified"
------------------------------
(cond ['a (begin 'b 'c 'd)])
==============================
(cond
  ['a
   'b
   'c
   'd])
------------------------------


test: "cond with begin in middle clause can be simplified"
------------------------------
(cond ['a 'b] ['c (begin 'd 'e)] ['f 'g])
==============================
(cond
  ['a 'b]
  ['c
   'd
   'e]
  ['f 'g])
------------------------------
