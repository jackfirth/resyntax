---
name: Unwanted suggestion report
about: Report a bug where Resyntax suggested an unwanted refactoring
title: Unwanted suggestion from `rule-name-here`
labels: bug, existing lint
assignees: ''

---

_Describe what Resyntax suggested and why it's undesirable. Include the rule name in the issue title, then put the original code and the suggested code below. If Resyntax made the suggestion to publicly viewable code, include a link to that code._

```scheme
#lang resyntax/test
require: resyntax/default-recommendations default-recommendations
header: - #lang racket

test: "this code should not be refactored"
--------------------
;; Put the original code here
--------------------
```

```scheme
;; Put Resyntax's unwanted suggestion here
```
