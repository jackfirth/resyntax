#lang resyntax/test


require: resyntax/default-recommendations syntax-shortcuts


header:
------------------------------------------------------------------------------------------------------
#lang racket/base
(require racket/syntax)
------------------------------------------------------------------------------------------------------


test: "syntax-e on a lone format-id argument is removable"
- (format-id #'foo "~a/c" (syntax-e #'bar))
- (format-id #'foo "~a/c" #'bar)


test: "syntax-e on multiple format-id arguments is removable"
- (format-id #'foo "~a.~a.~a" (syntax-e #'bar) (syntax-e #'baz) (syntax-e #'blah))
- (format-id #'foo "~a.~a.~a" #'bar #'baz #'blah)


test: "syntax-e on a single format-id argument is removable"
- (format-id #'foo "~a.~a.~a" #'bar (syntax-e #'baz) #'blah)
- (format-id #'foo "~a.~a.~a" #'bar #'baz #'blah)


test: "format-id call without any syntax-e unwrapped arguments not refactorable"
- (format-id #'foo "~a.~a.~a" #'bar #'baz #'blah)
