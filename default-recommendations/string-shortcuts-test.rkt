#lang resyntax/test


require: resyntax/default-recommendations string-shortcuts


header:
------------------------------
#lang racket/base
(require racket/string
         racket/port)
------------------------------


test: "display newline refactorable to newline"
- (display "\n")
- (displayln "")
- (newline)


test: "display followed by newline refactorable to displayln"
------------------------------
(define (foo)
  (display 42)
  (newline))
==============================
(define (foo)
  (displayln 42))
------------------------------


test: "string-append before string-join refactorable to string-join with #:before-first"
- (string-append "The " (string-join (list "fox" "hen" "dog")))
- (string-join (list "fox" "hen" "dog") #:before-first "The ")


test: "string-append after string-join refactorable to string-join with #:after-last"
- (string-append (string-join (list "fox" "hen" "dog")) " jumped")
- (string-join (list "fox" "hen" "dog") #:after-last " jumped")


test: "string-append around string-join refactorable to string-join with keyword arguments"
- (string-append "The " (string-join (list "fox" "hen" "dog")) " jumped")
- (string-join (list "fox" "hen" "dog") #:before-first "The " #:after-last " jumped")


test: "string-append before string-join with sep refactorable to string-join with #:before-first"
- (string-append "The " (string-join (list "fox" "hen" "dog") ", "))
- (string-join (list "fox" "hen" "dog") ", " #:before-first "The ")


test:
"multiline string-append around string-join refactorable to multiline string-join with keyword\
 arguments"
------------------------------
(string-append
 "The "
 (string-join (list "fox" "hen" "dog" "cat" "horse"))
 " all jumped together")
==============================
(string-join (list "fox" "hen" "dog" "cat" "horse")
             #:before-first "The "
             #:after-last " all jumped together")
------------------------------


test: "string-append with only one string can be removed"
- (string-append "hello")
- "hello"


test: "manual string-join can be replaced with real string-join"
------------------------------
(require racket/list)
(apply string-append (add-between (list "apple" "orange" "banana") ", "))
==============================
(require racket/list)
(string-join (list "apple" "orange" "banana") ", ")
------------------------------


test: "format with only one argument can be removed"
- (format "hello")
- "hello"


no-change-test:
"format with only one argument can't be removed when formatting directives are present"
- (format "hello ~a")


test: "string-append with format expression can be simplified - literals only"
------------------------------
(string-append "Hello "
               (format "~a" 'USERNAME)
               ", how are you today?")
==============================
(format "Hello ~a, how are you today?" 'USERNAME)
------------------------------


test: "string-append with format expression can be simplified - mixed expressions and literals"
------------------------------
(define (f s1 s2 x)
  (string-append s1 (format ", x = ~a, " x) s2))
==============================
(define (f s1 s2 x)
  (format "~a, x = ~a, ~a" s1 x s2))
------------------------------


test: "string-append with only format call can be simplified"
------------------------------
(define value 'test)
(string-append (format "~a" value))
==============================
(define value 'test)
(format "~a" value)
------------------------------


test: "string-append with format containing multiple placeholders"
------------------------------
(define (multi x y)
  (string-append "Result: " (format "~a and ~a" x y) "!"))
==============================
(define (multi x y)
  (format "Result: ~a and ~a!" x y))
------------------------------


test: "string-append with format before string literals"
------------------------------
(define name "Alice")
(string-append (format "~a" name) " is the winner!")
==============================
(define name "Alice")
(format "~a is the winner!" name)
------------------------------


test: "string-append with multiple non-string expressions"
------------------------------
(define (complex a b c d)
  (string-append a (format " = ~a, " b) c))
==============================
(define (complex a b c d)
  (format "~a = ~a, ~a" a b c))
------------------------------


no-change-test: "string-append with multiple format expressions not refactorable"
- (string-append (format "foo: ~a" 42) (format "bar: ~a" 42))


test: "manual with-output-to-string should be refactored to use with-output-to-string"
------------------------------
(define (f)
  (define os (open-output-string))
  (parameterize ([current-output-port os])
    (displayln "foo")
    (get-output-string os)))
==============================
(define (f)
  (with-output-to-string (λ () (displayln "foo"))))
------------------------------


test: "manual with-output-to-string with get-output-string outside parameterize"
------------------------------
(define (f)
  (define out (open-output-string))
  (parameterize ([current-output-port out])
    (displayln "hello world"))
  (get-output-string out))
==============================
(define (f)
  (with-output-to-string (λ () (displayln "hello world"))))
------------------------------


test: "manual with-output-to-string with multiple output operations"
------------------------------
(define (f)
  (define port (open-output-string))
  (parameterize ([current-output-port port])
    (display "Hello")
    (display " ")
    (display "world")
    (get-output-string port)))
==============================
(define (f)
  (with-output-to-string (λ ()
                           (display "Hello")
                           (display " ")
                           (display "world"))))
------------------------------
