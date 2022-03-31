#lang resyntax/testing/refactoring-test


require: resyntax/default-recommendations legacy-syntax-migrations


header:
- #lang racket/base


test: "the fifth argument to datum->syntax can be removed"
------------------------------
(define stx #'foo)
(datum->syntax stx 'bar stx stx #'blah)
------------------------------
------------------------------
(define stx #'foo)
(datum->syntax stx 'bar stx stx)
------------------------------


test: "syntax-recertify can be removed"
------------------------------
(define stx #'foo)
(define old-stx #'bar)
(syntax-recertify stx old-stx (current-inspector) 'key)
------------------------------
------------------------------
(define stx #'foo)
(define old-stx #'bar)
stx
------------------------------


test: "syntax-disarm can be removed"
------------------------------
(define stx #'foo)
(syntax-disarm stx (current-inspector))
------------------------------
------------------------------
(define stx #'foo)
stx
------------------------------


test: "syntax-rearm can be removed"
------------------------------
(define stx #'foo)
(syntax-rearm stx #'bar)
------------------------------
------------------------------
(define stx #'foo)
stx
------------------------------


test: "syntax-protect can be removed"
------------------------------
(define stx #'foo)
(syntax-protect stx)
------------------------------
------------------------------
(define stx #'foo)
stx
------------------------------
