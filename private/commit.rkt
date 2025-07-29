#lang racket/base

(provide (struct-out resyntax-commit))

(struct resyntax-commit (message changes) #:transparent)
