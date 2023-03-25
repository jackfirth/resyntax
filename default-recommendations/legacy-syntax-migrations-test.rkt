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


test: "syntax-local-match-introduce replaced with syntax-local-introduce"
------------------------------
(require (for-template racket/match))
(define (f)
  (syntax-local-match-introduce #'foo))
------------------------------
------------------------------
(require (for-template racket/match))
(define (f)
  (syntax-local-introduce #'foo))
------------------------------


test: "syntax-local-provide-introduce replaced with syntax-local-introduce"
------------------------------
(require (for-template racket/provide-syntax))
(define (f)
  (syntax-local-provide-introduce #'foo))
------------------------------
------------------------------
(require (for-template racket/provide-syntax))
(define (f)
  (syntax-local-introduce #'foo))
------------------------------


test: "syntax-local-require-introduce replaced with syntax-local-introduce"
------------------------------
(require (for-template racket/require-syntax))
(define (f)
  (syntax-local-require-introduce #'foo))
------------------------------
------------------------------
(require (for-template racket/require-syntax))
(define (f)
  (syntax-local-introduce #'foo))
------------------------------


test: "syntax-local-syntax-parse-pattern-introduce replaced with syntax-local-introduce"
------------------------------
(require (for-template syntax/parse))
(define (f)
  (syntax-local-syntax-parse-pattern-introduce #'foo))
------------------------------
------------------------------
(require (for-template syntax/parse))
(define (f)
  (syntax-local-introduce #'foo))
------------------------------
