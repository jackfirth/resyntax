---
name: New lint request
about: Suggest a new Resyntax refactoring rule
title: ''
labels: new lint
assignees: ''

---

_Describe the lint you'd like to see added to Resyntax. Include example code in the test case below. If applicable, include links to code you've seen where this lint would help. Also, see the Resyntax documentation on [what makes a good refactoring rule](https://docs.racket-lang.org/resyntax/Refactoring_Rules_and_Suites.html#%28part._.What_.Makes_a_.Good_.Refactoring_.Rule_%29) and [how to write Resyntax test cases](https://docs.racket-lang.org/resyntax/Testing_Refactoring_Rules.html)._

```scheme
#lang resyntax/test
require: resyntax/default-recommendations default-recommendations
header:
- #lang racket

test: "original code should be refactorable to new code"
--------------------
;; Put the original code here
====================
;; Put the code you'd like Resyntax to generate here
--------------------
```
