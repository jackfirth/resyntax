#lang racket/base


(provide condition-expression
         known-false
         known-not-false
         likely-boolean
         if-like-expression)


(require racket/string
         resyntax/default-recommendations/private/literal-constant
         resyntax/default-recommendations/private/metafunction
         syntax/parse)


(module+ test
  (require rackunit
           (submod "..")))


;@----------------------------------------------------------------------------------------------------


(define-syntax-class known-false
  (pattern constant:literal-constant #:when (not (attribute constant.value))))


(define-syntax-class known-not-false
  (pattern constant:literal-constant #:when (attribute constant.value)))


(define-syntax-class known-true
  (pattern constant:literal-constant #:when (equal? (attribute constant.value) #true)))


(define-syntax-class likely-boolean
  #:literals (or and not if)
  (pattern true:known-true)
  (pattern false:known-false)
  (pattern (or _:likely-boolean ...))
  (pattern (and _ ... _:likely-boolean))
  (pattern (not _))
  (pattern (f:likely-boolean-returning arg ...))
  (pattern (if _ _:likely-boolean _:likely-boolean)))


(define-syntax-class likely-boolean-returning
  #:literals (= < > <= >=)
  (pattern id:id #:when (string-suffix? (symbol->string (syntax-e #'id)) "?"))
  (pattern (~or = < > <= >=)))


(define-syntax-class condition-expression
  #:attributes (negated? base-condition)
  #:literals (not)
  (pattern (not base-condition:expr) #:with negated? #true)
  (pattern (~and base-condition:expr (~not (not _))) #:with negated? #false))


(define-syntax-class if-like-expression
  #:attributes (negated? base-condition [true-body 1] [false-body 1])
  #:literals (if cond else)

  (pattern (if :condition-expression first-branch second-branch)
    #:with (true-body ...) #'((~if negated? second-branch first-branch))
    #:with (false-body ...) #'((~if negated? first-branch second-branch)))

  (pattern (cond [:condition-expression first-body ...] [else second-body ...])
    #:with (true-body ...) #'(~if negated? (second-body ...) (first-body ...))
    #:with (false-body ...) #'(~if negated? (first-body ...) (second-body ...)))

  (pattern (cond [:condition-expression first-body ...] [second-branch])
    #:with (true-body ...) #'(~if negated? (second-branch) (first-body ...))
    #:with (false-body ...) #'(~if negated? (first-body ...) (second-branch))))


(module+ test
  (test-case "if-like-expression"

    (syntax-parse #'(if (even? n) a b)
      [:if-like-expression
       (check-false (syntax->datum #'negated?))
       (check-equal? (syntax->datum #'base-condition) '(even? n))
       (check-equal? (syntax->datum #'(true-body ...)) '(a))
       (check-equal? (syntax->datum #'(false-body ...)) '(b))])

    (syntax-parse #'(if (not (even? n)) a b)
      [:if-like-expression
       (check-true (syntax->datum #'negated?))
       (check-equal? (syntax->datum #'base-condition) '(even? n))
       (check-equal? (syntax->datum #'(true-body ...)) '(b))
       (check-equal? (syntax->datum #'(false-body ...)) '(a))])

    (syntax-parse #'(cond [(even? n) a b c] [else x y z])
      [:if-like-expression
       (check-false (syntax->datum #'negated?))
       (check-equal? (syntax->datum #'base-condition) '(even? n))
       (check-equal? (syntax->datum #'(true-body ...)) '(a b c))
       (check-equal? (syntax->datum #'(false-body ...)) '(x y z))])

    (syntax-parse #'(cond [(not (even? n)) a b c] [else x y z])
      [:if-like-expression
       (check-true (syntax->datum #'negated?))
       (check-equal? (syntax->datum #'base-condition) '(even? n))
       (check-equal? (syntax->datum #'(true-body ...)) '(x y z))
       (check-equal? (syntax->datum #'(false-body ...)) '(a b c))])

    (syntax-parse #'(cond [(even? n) a b c] [x])
      [:if-like-expression
       (check-false (syntax->datum #'negated?))
       (check-equal? (syntax->datum #'base-condition) '(even? n))
       (check-equal? (syntax->datum #'(true-body ...)) '(a b c))
       (check-equal? (syntax->datum #'(false-body ...)) '(x))])

    (syntax-parse #'(cond [(not (even? n)) a b c] [x])
      [:if-like-expression
       (check-true (syntax->datum #'negated?))
       (check-equal? (syntax->datum #'base-condition) '(even? n))
       (check-equal? (syntax->datum #'(true-body ...)) '(x))
       (check-equal? (syntax->datum #'(false-body ...)) '(a b c))])))
