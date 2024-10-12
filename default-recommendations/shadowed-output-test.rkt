#lang resyntax/test


require: resyntax/default-recommendations default-recommendations


header:
------------------------------
#lang racket/base
(require racket/list
         racket/match
         syntax/parse/define)
------------------------------


test: "shadowed `or`: single-line de morgan's law"
------------------------------
(define (or x y z) (append x y z))
(and (not 1) (not 2) (not 3))
------------------------------


test: "shadowed `or`: multi-line de morgan's law"
------------------------------
(define (or x y z) (append x y z))
(and (not 1)
     (not 2)
     (not 3))
------------------------------


test: "shadowed `and`: single-line de morgan's law"
------------------------------
(define (and x y z) (list x y z))
(or (not 1) (not 2) (not 3))
------------------------------


test: "shadowed `and`: multi-line de morgan's law"
------------------------------
(define (and x y z) (list x y z))
(or (not 1)
    (not 2)
    (not 3))
------------------------------


test: "shadowed `not`: if then false else true"
------------------------------
(define (not b) (list 'not b))
(if 4 #false #true)
------------------------------


test: "shadowed `unless`: when not"
------------------------------
(define (unless c b) b)
(when (not 'foo)
  (displayln "not foo"))
------------------------------


test: "shadowed `displayln`: when not -> unless is fine"
------------------------------
(define (displayln s)
  s)
(when (not 'foo)
  (displayln "not foo"))
------------------------------
------------------------------
(define (displayln s)
  s)
(unless 'foo
  (displayln "not foo"))
------------------------------


test: "shadowed `when`: unless not"
------------------------------
(define (when c b) b)
(unless (not 'foo)
  (displayln "foo"))
------------------------------


test: "shadowed `displayln`: unless not -> when is fine"
------------------------------
(define (displayln s)
  s)
(unless (not 'foo)
  (displayln "foo"))
------------------------------
------------------------------
(define (displayln s)
  s)
(when 'foo
  (displayln "foo"))
------------------------------


test: "shadowed `and`: single-line if-else-false-to-and"
------------------------------
(define (and x y) (list x y))
(if 'a (println "true branch") #f)
------------------------------


test: "shadowed `and`: multi-line if-else-false-to-and"
------------------------------
(define (and x y) (list x y))
(if 'a
    (println "true branch")
    #f)
------------------------------


test: "shadowed `println`: single-line if-else-false-to-and is fine"
------------------------------
(define (println v)
  v)
(if 'a (println "true branch") #f)
------------------------------
------------------------------
(define (println v)
  v)
(and 'a (println "true branch"))
------------------------------


test: "shadowed `println`: multi-line if-else-false-to-and is fine"
------------------------------
(define (println v)
  v)
(if 'some-long-condition-expression
    (println "some very long true branch that should stay on its own line")
    #f)
------------------------------
------------------------------
(define (println v)
  v)
(and 'some-long-condition-expression
     (println "some very long true branch that should stay on its own line"))
------------------------------


test: "shadowed `for`: for-each with long single-form body"
------------------------------
(define (for lst f)
  (for-each f lst))
(define some-list (list 1 2 3))
(for-each
 (λ (a-very-very-very-long-variable-name-thats-so-very-long)
   (displayln a-very-very-very-long-variable-name-thats-so-very-long))
 some-list)
------------------------------


test: "shadowed `for`: for-each with multiple body forms"
------------------------------
(define (for lst f)
  (for-each f lst))
(define some-list (list 1 2 3))
(for-each
 (λ (x)
   (displayln x)
   (displayln x))
 some-list)
------------------------------


test: "shadowed `for/vector`: list->vector with for/list"
------------------------------
(define (for/vector f v)
  (list->vector (map f (vector->list v))))
(list->vector
 (for/list ([x (in-range 0 10)])
   (displayln x)
   (* x x)))
------------------------------


test: "shadowed `for*`: nested for forms"
------------------------------
(define (for* lsts f)
  (for-each (λ (args) (apply f args)) (apply cartesian-product lsts)))
(for ([x (in-range 0 5)])
  (for ([y (in-range 0 5)])
    (for ([z (in-range 0 5)])
      (displayln x)
      (displayln y)
      (displayln z))))
------------------------------


test: "shadowed `for/first`: let loop over vector"
------------------------------------------------------------
(define (for/first lst f)
  (f (first lst)))
(define vec (vector 0 1 2 3 4 5))
(let loop ([i 0])
  (and (< i (vector-length vec))
       (let ([x (vector-ref vec i)])
         (if (> x 3)
             (+ x 42)
             (loop (add1 i))))))
------------------------------------------------------------


test: "shadowed `for*`: for-each and append-map"
------------------------------------------------------------
(define (for* lsts f)
  (for-each (λ (args) (apply f args)) (apply cartesian-product lsts)))
(define words (list 'the 'quick 'brown 'fox))
(for-each (λ (c)
            (printf "Letter: ~a\n" c)
            (printf "Letter code: ~a\n\n" (char->integer c)))
          (append-map (λ (word) (string->list (symbol->string word)))
                      words))
------------------------------------------------------------


test: "shadowed `struct`: define-struct without options"
----------------------------------------
(define (struct name n)
  (define-values (struct:name _1 _2 _3 _4) (make-struct-type name #f n 0))
  struct:name)
(define-struct point (x y))
----------------------------------------


test: "shadowed `define`: let forms inside for loop bodies"
------------------------------
(define-values (definitions) (make-hasheq))
(define-values (define)
  (lambda (name value) (hash-set! definitions name value)))
(for ([i (in-range 0 10)])
  (let ([x 1])
    (println x)))
------------------------------


test: "shadowed `call-with-values`: let-values expressions with an immediate call"
------------------------------
(define (call-with-values generator receiver)
  ((compose receiver generator)))
(let-values ([(x y z) (values 1 2 3)]) (list x y z))
------------------------------


test: "shadowed `last`: first reverse of list"
------------------------------
(define (last l) (apply max l))
(first (reverse (list 1 2 3)))
------------------------------


test: "shadowed `append-map`: (append* (map ...))"
------------------------------
(define ((append-map f) lst) (append* (map f lst)))
(define (f x) (list x x x))
(append* (map f (list 1 2 3)))
------------------------------


test: "shadowed `match-define`: single-clause match expressions"
------------------------------
(define-syntax-rule (match-define head clause ...)
  (define/match head clause ...))
(define (foo x)
  (match x
    [(list a b c)
     (displayln "foo!")
     (+ a b c)]))
------------------------------


test: "shadowed `add1`: lambda equivalent to add1"
------------------------------
(define (add1 v) (cons 1 v))
(map (λ (x) (+ x 1)) (list 1 2 3))
------------------------------


test: "shadowed `sub1`: lambda equivalent to sub1"
------------------------------
(define (sub1 v) (rest v))
(map (λ (x) (- x 1)) (list 1 2 3))
------------------------------


test: "shadowed `positive?`: lambda equivalent to positive?"
------------------------------
(define (positive? v) (and (cons? v) (= 1 (first v))))
(filter (λ (x) (< 0 x)) (list -2 -1 0 1 2))
------------------------------


test: "shadowed `negative?`: lambda equivalent to negative?"
------------------------------
(define (negative? v) (and (cons? v) (= -1 (first v))))
(filter (λ (x) (< x 0)) (list -2 -1 0 1 2))
------------------------------


test: "shadowed `define-syntax-parse-rule`: define-simple-macro"
------------------------------
(define-syntax-rule (define-syntax-parse-rule new old)
  (define-simple-macro (new . args) (old . args)))
(define-simple-macro (my-or a:expr b:expr)
  (let ([tmp a])
    (if a a b)))
------------------------------


test: "shadowed `define-syntax-rule`: single-clause syntax-rules macro"
------------------------------
(define-syntax define-syntax-rule
  (syntax-rules ()
    [(define-syntax-rule new old)
     (define-syntax new
       (syntax-rules ()
         [(new . args) (old . args)]))]))
(define-syntax my-or
  (syntax-rules ()
    [(my-or a b)
     (let ([tmp a])
       (if a a b))]))
------------------------------


test: "shadowed `define-syntax-rule` using `define-syntax-parse-rule` after"
------------------------------
(define-syntax my-or
  (syntax-rules ()
    [(my-or a b)
     (let ([tmp a])
       (if a a b))]))
(begin
  (define-syntax-parse-rule (define-syntax-rule new old)
    (define-syntax-parse-rule (new . args) (old . args)))
  (define-syntax-rule def define)
  (def x 5)
  x)
------------------------------


test: "`define-syntax-rule` isn't actually shadowed in this scope"
------------------------------
(define-syntax my-or
  (syntax-rules ()
    [(my-or a b)
     (let ([tmp a]) (if a a b))]))
(let ()
  (define-syntax-parse-rule (define-syntax-rule new old)
    (define-syntax-parse-rule (new . args)
      (old . args)))
  (define-syntax-rule def define)
  (def x 5)
  x)
------------------------------
------------------------------
(define-syntax-rule (my-or a b)
  (let ([tmp a]) (if a a b)))
(let ()
  (define-syntax-parse-rule (define-syntax-rule new old)
    (define-syntax-parse-rule (new . args)
      (old . args)))
  (define-syntax-rule def define)
  (def x 5)
  x)
------------------------------
