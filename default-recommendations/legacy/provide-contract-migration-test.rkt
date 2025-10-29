#lang resyntax/test


require: resyntax/default-recommendations provide-contract-migration


header:
------------------------------
#lang racket/base
(require racket/contract/base)
------------------------------


test: "provide/contract refactorable to provide with contract-out"
------------------------------
(provide/contract [foo integer?])
(define foo 42)
==============================
(provide (contract-out [foo integer?]))
(define foo 42)
------------------------------


test: "provide/contract with unprotected submodule refactorable to provide with contract-out"
------------------------------
(provide/contract #:unprotected-submodule unsafe [foo integer?])
(define foo 42)
==============================
(provide (contract-out #:unprotected-submodule unsafe [foo integer?]))
(define foo 42)
------------------------------
