#lang resyntax/testing/refactoring-test


require: resyntax/default-recommendations legacy-struct-migrations


header:
- #lang racket/base


test: "define-struct without options"
- (define-struct point (x y))
----------------------------------------
(struct point (x y)
  #:extra-constructor-name make-point)
----------------------------------------


test: "define-struct with simple options"
----------------------------------------
(define-struct point (x y)
  #:transparent
  #:mutable)
----------------------------------------
----------------------------------------
(struct point (x y)
  #:transparent
  #:mutable
  #:extra-constructor-name make-point)
----------------------------------------


test: "one-line define-struct with simple options"
- (define-struct point (x y) #:transparent #:mutable)
----------------------------------------
(struct point (x y) #:transparent #:mutable
  #:extra-constructor-name make-point)
----------------------------------------


test: "define-struct with supertype"
----------------------------------------
(struct point ())
(define-struct (2d-point point) (x y))
----------------------------------------
----------------------------------------
(struct point ())
(struct 2d-point point (x y)
  #:extra-constructor-name make-2d-point)
----------------------------------------


test: "define-struct with multi-form single-line options"
----------------------------------------
(define-struct point (x y)
  #:guard (λ (x y _) (values x y))
  #:property prop:custom-print-quotable 'never
  #:inspector #false)
----------------------------------------
----------------------------------------
(struct point (x y)
  #:guard (λ (x y _) (values x y))
  #:property prop:custom-print-quotable 'never
  #:inspector #false
  #:extra-constructor-name make-point)
----------------------------------------


test: "define-struct with multi-line options"
----------------------------------------
(define-struct point (x y)
  #:property prop:custom-write
  (λ (this out mode)
    (write-string "#<point>" out)))
----------------------------------------
----------------------------------------
(struct point (x y)
  #:property prop:custom-write
  (λ (this out mode)
    (write-string "#<point>" out))
  #:extra-constructor-name make-point)
----------------------------------------


test: "define-struct with options with separating whitespace"
----------------------------------------
(define-struct point (x y)

  #:property prop:custom-write
  (λ (this out mode)
    (write-string "#<point>" out))

  #:guard (λ (x y _) (values x y)))
----------------------------------------
----------------------------------------
(struct point (x y)

  #:property prop:custom-write
  (λ (this out mode)
    (write-string "#<point>" out))

  #:guard (λ (x y _) (values x y))
  #:extra-constructor-name make-point)
----------------------------------------


test: "define-struct with field comments"
----------------------------------------
(define-struct point (x ;; The X coordinate of the point
                      y ;; The Y coordinate of the point
                      ))
----------------------------------------
----------------------------------------
(struct point (x ;; The X coordinate of the point
               y ;; The Y coordinate of the point
               )
  #:extra-constructor-name make-point)
----------------------------------------


test: "define-struct with comments between options"
----------------------------------------
(define-struct point (x y)

  ;; Custom write implementation
  #:property prop:custom-write
  (λ (this out mode)
    (write-string "#<point>" out))

  ;; Field guard
  #:guard (λ (x y _) (values x y)))
----------------------------------------
----------------------------------------
(struct point (x y)

  ;; Custom write implementation
  #:property prop:custom-write
  (λ (this out mode)
    (write-string "#<point>" out))

  ;; Field guard
  #:guard (λ (x y _) (values x y))
  #:extra-constructor-name make-point)
----------------------------------------
