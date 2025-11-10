#lang resyntax/test


header:
------------------------------
#lang racket/base
------------------------------


test: "timeout parameter can be customized in tests"
@analyzer-timeout-millis 5000
- (or 1 (or 2 3))
- (or 1 2 3)


no-change-test: "timeout parameter works with no-change-test"
@analyzer-timeout-millis 5000
- (define x 42)
