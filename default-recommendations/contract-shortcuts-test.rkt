#lang resyntax/testing/refactoring-test


require: resyntax/default-recommendations contract-shortcuts


test: "nested or/c contracts can be flattened"
------------------------------
#lang racket/base
(require racket/contract/base)
(void (or/c 1 2 (or/c 3 4)))
------------------------------
------------------------------
#lang racket/base
(require racket/contract/base)
(void (or/c 1 2 3 4))
------------------------------


test: "flat or/c contracts can't be flattened"
------------------------------
#lang racket/base
(require racket/contract/base)
(void (or/c 1 2 3))
------------------------------


test: "multiple nested or/c contracts can be flattened at once"
------------------------------
#lang racket/base
(require racket/contract/base)
(void (or/c (or/c 1 2) (or/c 3 4) (or/c 5 6)))
------------------------------
------------------------------
#lang racket/base
(require racket/contract/base)
(void (or/c 1 2 3 4 5 6))
------------------------------


test: "deeply nested or/c contracts can be flattened in one pass"
------------------------------
#lang racket/base
(require racket/contract/base)
(void (or/c 1 (or/c 2 (or/c 3 (or/c 4 5 6)))))
------------------------------
------------------------------
#lang racket/base
(require racket/contract/base)
(void (or/c 1 2 3 4 5 6))
------------------------------


test: "multiline nested or/c contracts can't be flattened"
------------------------------
#lang racket/base
(require racket/contract/base)
(void
 (or/c 1
       (or/c 2 3)))
------------------------------


test: "nested and/c contracts can be flattened"
------------------------------
#lang racket/base
(require racket/contract/base)
(void (and/c 1 2 (and/c 3 4)))
------------------------------
------------------------------
#lang racket/base
(require racket/contract/base)
(void (and/c 1 2 3 4))
------------------------------


test: "flat and/c contracts can't be flattened"
------------------------------
#lang racket/base
(require racket/contract/base)
(void (and/c 1 2 3))
------------------------------


test: "multiple nested and/c contracts can be flattened at once"
------------------------------
#lang racket/base
(require racket/contract/base)
(void (and/c (and/c 1 2) (and/c 3 4) (and/c 5 6)))
------------------------------
------------------------------
#lang racket/base
(require racket/contract/base)
(void (and/c 1 2 3 4 5 6))
------------------------------


test: "deeply nested and/c contracts can be flattened in one pass"
------------------------------
#lang racket/base
(require racket/contract/base)
(void (and/c 1 (and/c 2 (and/c 3 (and/c 4 5 6)))))
------------------------------
------------------------------
#lang racket/base
(require racket/contract/base)
(void (and/c 1 2 3 4 5 6))
------------------------------


test: "multiline nested and/c contracts can't be flattened"
------------------------------
#lang racket/base
(require racket/contract/base)
(void
 (and/c 1
        (and/c 2 3)))
------------------------------


test: "nested or/c contracts interspersed with and/c contracts can be flattened"
------------------------------
#lang racket/base
(require racket/contract/base)
(void (or/c (or/c 1 2) (and/c 3 4) (or/c 5 6)))
------------------------------
------------------------------
#lang racket/base
(require racket/contract/base)
(void (or/c 1 2 (and/c 3 4) 5 6))
------------------------------


test: "nested and/c contracts interspersed with or/c contracts can be flattened"
------------------------------
#lang racket/base
(require racket/contract/base)
(void (and/c (and/c 1 2) (or/c 3 4) (and/c 5 6)))
------------------------------
------------------------------
#lang racket/base
(require racket/contract/base)
(void (and/c 1 2 (or/c 3 4) 5 6))
------------------------------


test: "contracts equivalent to predicate/c can be refactored to predicate/c"
------------------------------
#lang racket/base
(require racket/contract/base)
(void (-> any/c boolean?))
------------------------------
------------------------------
#lang racket/base
(require racket/contract/base)
(void predicate/c)
------------------------------
