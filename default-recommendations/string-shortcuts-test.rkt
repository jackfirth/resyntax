#lang resyntax/testing/refactoring-test


require: resyntax/default-recommendations string-shortcuts


header:
------------------------------
#lang racket/base
(require racket/string)
------------------------------


test: "display newline refactorable to newline"
- (display "\n")
- (displayln "")
- (newline)


test: "string-append before string-join refactorable to string-join with #:before-first"
- (string-append "The " (string-join (list "fox" "hen" "dog")))
- (string-join (list "fox" "hen" "dog") #:before-first "The ")


test: "string-append after string-join refactorable to string-join with #:after-last"
- (string-append (string-join (list "fox" "hen" "dog")) " jumped")
- (string-join (list "fox" "hen" "dog") #:after-last " jumped")


test: "string-append around string-join refactorable to string-join with keyword arguments"
- (string-append "The " (string-join (list "fox" "hen" "dog")) " jumped")
- (string-join (list "fox" "hen" "dog") #:before-first "The " #:after-last " jumped")


test:
"multiline string-append around string-join refactorable to multiline string-join with keyword\
 arguments"
------------------------------
(string-append
 "The "
 (string-join (list "fox" "hen" "dog"))
 " jumped")
------------------------------
------------------------------
(string-join (list "fox" "hen" "dog")
             #:before-first "The "
             #:after-last " jumped")
------------------------------


test: "string-append with only one string can be removed"
- (string-append "hello")
- "hello"


test: "format with only one argument can be removed"
- (format "hello")
- "hello"


test: "format with only one argument can't be removed when formatting directives are present"
- (format "hello ~a")
