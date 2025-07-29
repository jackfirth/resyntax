#lang resyntax/test

require:
resyntax/default-recommendations
contract-shortcuts

header:
------------------------------
#lang racket/base
(require racket/contract/base)
------------------------------

test:
"nested or/c contracts can be flattened"
-
(void (or/c 1 2 (or/c 3 4)))
-
(void (or/c 1 2 3 4))

test:
"flat or/c contracts can't be flattened"
-
(or/c 1 2 3)

test:
"multiple nested or/c contracts can be flattened at once"
-
(void (or/c (or/c 1 2) (or/c 3 4) (or/c 5 6)))
-
(void (or/c 1 2 3 4 5 6))

test:
"deeply nested or/c contracts can be flattened in one pass"
-
(void (or/c 1 (or/c 2 (or/c 3 (or/c 4 5 6)))))
-
(void (or/c 1 2 3 4 5 6))

test:
"multiline nested or/c contracts can't be flattened"
------------------------------
(or/c 1 (or/c 2 3))
------------------------------

test:
"nested and/c contracts can be flattened"
-
(void (and/c 1 2 (and/c 3 4)))
-
(void (and/c 1 2 3 4))

test:
"flat and/c contracts can't be flattened"
-
(and/c 1 2 3)

test:
"multiple nested and/c contracts can be flattened at once"
-
(void (and/c (and/c 1 2) (and/c 3 4) (and/c 5 6)))
-
(void (and/c 1 2 3 4 5 6))

test:
"deeply nested and/c contracts can be flattened in one pass"
-
(void (and/c 1 (and/c 2 (and/c 3 (and/c 4 5 6)))))
-
(void (and/c 1 2 3 4 5 6))

test:
"multiline nested and/c contracts can't be flattened"
------------------------------
(and/c 1 (and/c 2 3))
------------------------------

test:
"nested or/c contracts interspersed with and/c contracts can be flattened"
-
(void (or/c (or/c 1 2) (and/c 3 4) (or/c 5 6)))
-
(void (or/c 1 2 (and/c 3 4) 5 6))

test:
"nested and/c contracts interspersed with or/c contracts can be flattened"
-
(void (and/c (and/c 1 2) (or/c 3 4) (and/c 5 6)))
-
(void (and/c 1 2 (or/c 3 4) 5 6))

test:
"contracts equivalent to path-string? can be refactored to path-string?"
-
(void (or/c path? string?))
-
(void (or/c string? path?))
-
(void path-string?)

test:
"->* contracts using #:rest (listof arg) can be replaced with -> and ellipses"
-
(void (->* (string? number?) #:rest (listof symbol?) list?))
-
(void (->* (string? number?) () #:rest (listof symbol?) list?))
-
(void (-> string? number? symbol? ... list?))

test:
"infix ->* contracts using #:rest (listof arg) can be replaced with -> and ellipses"
-
(void ((string? number?) #:rest (listof symbol?) . ->* . list?))
-
(void (-> string? number? symbol? ... list?))

test:
"->* contracts using #:rest and optional arguments not refactorable to -> and ellipses"
-
(void (->* () (string?) #:rest (listof symbol?) list?))

test:
"provide/contract refactorable to provide with contract-out"
------------------------------
(provide/contract [foo integer?])
(define foo 42)
==============================
(provide (contract-out [foo integer?]))
(define foo 42)
------------------------------

test:
"provide/contract with unprotected submodule refactorable to provide with contract-out"
------------------------------
(provide/contract #:unprotected-submodule unsafe [foo integer?])
(define foo 42)
==============================
(provide (contract-out #:unprotected-submodule unsafe [foo integer?]))
(define foo 42)
------------------------------
