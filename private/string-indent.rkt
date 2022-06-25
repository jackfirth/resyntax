#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [string-indent (-> string? #:amount exact-nonnegative-integer? (and/c string? immutable?))]))


(require racket/string)


;@----------------------------------------------------------------------------------------------------


(define (string-indent s #:amount amount)
  (define lines
    (for/list ([line (in-lines (open-input-string s))])
      (string-append (make-string amount #\space) line)))
  (string->immutable-string (string-join lines "\n")))
