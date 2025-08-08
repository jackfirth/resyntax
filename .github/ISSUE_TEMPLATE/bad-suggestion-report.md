---
name: Bad suggestion report
about: Report a bug where Resyntax incorrectly suggested a refactoring
title: ''
labels: bug, existing lint
assignees: ''

---

_Describe the incorrect refactoring suggestion that Resyntax made. Include the original code and what Resyntax suggested in the test case below. Explain why the suggestion is incorrect or problematic._

```scheme
#lang resyntax/test
require: resyntax/default-recommendations default-recommendations
header: - #lang racket

test: "this code should not be refactored"
--------------------
;; Put the original code that Resyntax incorrectly suggests refactoring here
--------------------
```

**What Resyntax suggested:**
```scheme
;; Describe or show what Resyntax suggested as the refactored code
```

**Why this suggestion is incorrect:**
<!-- Explain why the suggested refactoring is wrong, problematic, or inappropriate -->