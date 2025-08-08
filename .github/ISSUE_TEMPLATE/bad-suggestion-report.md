---
name: Bad suggestion report
about: Report a bug where Resyntax incorrectly suggested a refactoring
title: ''
labels: bug
assignees: ''

---

_Describe the incorrect refactoring suggestion that Resyntax made. Include the original code and what Resyntax suggested in the test case below. Explain why the suggestion is incorrect or problematic. Also, see the Resyntax documentation on [what makes a good refactoring rule](https://docs.racket-lang.org/resyntax/Refactoring_Rules_and_Suites.html#%28part._.What_.Makes_a_.Good_.Refactoring_.Rule_%29) and [how to write Resyntax test cases](https://docs.racket-lang.org/resyntax/Testing_Refactoring_Rules.html)._

```scheme
#lang resyntax/test
require: resyntax/default-recommendations default-recommendations
header: - #lang racket

test: "this code should not be refactored"
- ;; Put the original code that Resyntax incorrectly suggests refactoring here
```

**What Resyntax suggested:**
<!-- Describe or show what Resyntax suggested as the refactored code -->

**Why this suggestion is incorrect:**
<!-- Explain why the suggested refactoring is wrong, problematic, or inappropriate -->