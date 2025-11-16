#lang resyntax/test


require: resyntax/default-recommendations boolean-shortcuts


header:
------------------------------
#lang racket/base
(require resyntax/base)
------------------------------


no-change-test: "suppressing a rule prevents its application"
------------------------------
(resyntax-suppress nested-and-to-flat-and
  (and 1 (and 2 3)))
------------------------------


no-change-test: "suppressed nested or is not refactored"
------------------------------
(resyntax-suppress nested-or-to-flat-or
  (or 1 (or 2 3)))
------------------------------


test: "unsuppressed nested or is refactored normally"
- (or 1 (or 2 3))
- (or 1 2 3)


test: "suppression is specific to the rule name"
------------------------------
(resyntax-suppress nested-or-to-flat-or
  (and 1 (and 2 3)))
==============================
(resyntax-suppress nested-or-to-flat-or
  (and 1 2 3))
------------------------------
