#lang resyntax/test


require: resyntax/default-recommendations dict-suggestions


header:
--------------------
#lang racket/base
(require racket/dict)
--------------------


test: "in-dict refactorable to in-dict-keys when only the key is used"
--------------------
(for ([(k v) (in-dict (hash 'a 1 'b 2 'c 3))])
  (displayln k))
====================
(for ([k (in-dict-keys (hash 'a 1 'b 2 'c 3))])
  (displayln k))
--------------------


test: "in-dict refactorable to in-dict-values when only the value is used"
--------------------
(for ([(k v) (in-dict (hash 'a 1 'b 2 'c 3))])
  (displayln v))
====================
(for ([v (in-dict-values (hash 'a 1 'b 2 'c 3))])
  (displayln v))
--------------------


no-change-test: "in-dict not refactorable to in-dict-keys when key and value both used"
--------------------
(for ([(k v) (in-dict (hash 'a 1 'b 2 'c 3))])
  (displayln k)
  (displayln v))
--------------------


test: "in-dict between other clauses refactorable to in-dict-keys when only the key is used"
--------------------
(for ([i (in-naturals)]
      [(k v) (in-dict (hash 'a 1 'b 2 'c 3))]
      [j (in-naturals)])
  (displayln (list i j k)))
====================
(for ([i (in-naturals)]
      [k (in-dict-keys (hash 'a 1 'b 2 'c 3))]
      [j (in-naturals)])
  (displayln (list i j k)))
--------------------


test: "in-dict between other clauses refactorable to in-dict-values when only the value is used"
--------------------
(for ([i (in-naturals)]
      [(k v) (in-dict (hash 'a 1 'b 2 'c 3))]
      [j (in-naturals)])
  (displayln (list i j v)))
====================
(for ([i (in-naturals)]
      [v (in-dict-values (hash 'a 1 'b 2 'c 3))]
      [j (in-naturals)])
  (displayln (list i j v)))
--------------------


test: "in-dict in for* loop refactorable to in-dict-keys"
--------------------
(for* ([(k v) (in-dict (hash 'a 1 'b 2 'c 3))])
  (displayln k))
====================
(for* ([k (in-dict-keys (hash 'a 1 'b 2 'c 3))])
  (displayln k))
--------------------


test: "in-dict in for/list loop refactorable to in-dict-keys"
--------------------
(for/list ([(k v) (in-dict (hash 'a 1 'b 2 'c 3))])
  k)
====================
(for/list ([k (in-dict-keys (hash 'a 1 'b 2 'c 3))])
  k)
--------------------


test: "in-dict in for/list loop refactorable to in-dict-values"
--------------------
(for/list ([(k v) (in-dict (hash 'a 1 'b 2 'c 3))])
  v)
====================
(for/list ([v (in-dict-values (hash 'a 1 'b 2 'c 3))])
  v)
--------------------
