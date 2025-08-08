#lang resyntax/test


header:
- #lang racket/base


test: "automatic default-recommendations: nested and should be flattened"
- (and (and 1 2) 3)
- (and 1 2 3)


test: "automatic default-recommendations: nested or should be flattened"
- (or (or 1 2) 3)
- (or 1 2 3)

