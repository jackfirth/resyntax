---
name: New lint request
about: Suggest a new Resyntax refactoring rule
title: ''
labels: new lint
assignees: ''

---

_Describe the lint you'd like to see added to Resyntax. Include example code in the test case below. If applicable, include links to files where this lint would help._

```scheme
#lang resyntax/test

test: "original code should be refactorable to new code"
--------------------
#lang racket
;; Put the original code here
--------------------
--------------------
#lang racket
;; Put the code you'd like Resyntax to generate here
--------------------
```
