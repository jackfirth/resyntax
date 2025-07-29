#lang racket/base

(provide (struct-out statement-transformer))

;@----------------------------------------------------------------------------------------------------

(struct statement-transformer (procedure))
