#lang resyntax/testing/refactoring-test


require: resyntax/default-recommendations string-shortcuts


header:
- #lang racket/base


test: "display newline refactorable to newline"
- (display "\n")
- (displayln "")
- (newline)
