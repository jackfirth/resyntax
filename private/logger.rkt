#lang racket/base


(provide log-resyntax-fatal
         log-resyntax-error
         log-resyntax-warning
         log-resyntax-info
         log-resyntax-debug
         resyntax-logger)


(define-logger resyntax)
