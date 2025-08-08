---
name: Bad suggestion report
about: Report a bug where Resyntax incorrectly suggested a refactoring
title: ''
labels: bug, existing lint
assignees: ''

---

_Describe the incorrect refactoring suggestion that Resyntax made. Include the name of the refactoring rule that produced the suggestion below. Include the original code and what Resyntax suggested in the test case below. Explain why the suggestion is incorrect or problematic._

```scheme
#lang resyntax/test
require: resyntax/default-recommendations default-recommendations
header: - #lang racket

test: "this code should not be refactored"
--------------------
;; Put the original code here
--------------------
```

### Suggestion from rule `<rule name here>`:
```scheme
;; Put the incorrect code that Resyntax suggested here
```

