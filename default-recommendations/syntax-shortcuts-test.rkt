#lang resyntax/test

require:
resyntax/default-recommendations
syntax-shortcuts

header:
------------------------------------------------------------------------------------------------------
#lang racket/base
(require racket/syntax)
------------------------------------------------------------------------------------------------------

test:
"syntax-e on a lone format-id argument is removable"
-
(format-id #'foo "~a/c" (syntax-e #'bar))
-
(format-id #'foo "~a/c" #'bar)

test:
"syntax-e on multiple format-id arguments is removable"
-
(format-id #'foo "~a.~a.~a" (syntax-e #'bar) (syntax-e #'baz) (syntax-e #'blah))
-
(format-id #'foo "~a.~a.~a" #'bar #'baz #'blah)

test:
"syntax-e on a single format-id argument is removable"
-
(format-id #'foo "~a.~a.~a" #'bar (syntax-e #'baz) #'blah)
-
(format-id #'foo "~a.~a.~a" #'bar #'baz #'blah)

test:
"format-id call without any syntax-e unwrapped arguments not refactorable"
-
(format-id #'foo "~a.~a.~a" #'bar #'baz #'blah)

test:
"making a symbol with format can be simplified to format-symbol"
-
(string->symbol (format "make-~a" "foo"))
-
(format-symbol "make-~a" "foo")

test:
"making a symbol with format from a symbol can be simplified to format-symbol"
-
(string->symbol (format "make-~a" (symbol->string 'foo)))
-
(format-symbol "make-~a" 'foo)

test:
"making a symbol with format from an identifier can be simplified to format-symbol"
-
(string->symbol (format "make-~a" (symbol->string (syntax-e #'foo))))
-
(format-symbol "make-~a" #'foo)

test:
"making a symbol with format from a keyword can be simplified to format-symbol"
-
(string->symbol (format "make-~a" (keyword->string '#:foo)))
-
(format-symbol "make-~a" '#:foo)

test:
"making a symbol with format from a keyword syntax object can be simplified to format-symbol"
-
(string->symbol (format "make-~a" (keyword->string (syntax-e #'#:foo))))
-
(format-symbol "make-~a" #'#:foo)
