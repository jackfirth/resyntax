#lang racket/base


(provide pure-expression)


(require racket/list
         resyntax/default-recommendations/private/literal-constant
         syntax/parse)


;@----------------------------------------------------------------------------------------------------


; These are all the pure functions defined in docs.racket-lang.org/reference/generic-numbers.html
; The racket/math ones aren't included because I haven't bothered to do that yet.
(define-literal-set pure-numeric-functions
  (+
   -
   *
   /
   quotient
   remainder
   quotient/remainder
   modulo
   add1
   sub1
   abs
   max
   min
   gcd
   lcm
   round
   floor
   ceiling
   truncate
   numerator
   denominator
   rationalize
   =
   <
   <=
   >
   >=
   sqrt
   integer-sqrt
   integer-sqrt/remainder
   expt
   exp
   log
   sin
   cos
   tan
   asin
   acos
   atan
   make-rectangular
   make-polar
   real-part
   imag-part
   magnitude
   angle
   bitwise-ior
   bitwise-and
   bitwise-xor
   bitwise-not
   bitwise-bit-set?
   bitwise-first-bit-set
   bitwise-bit-field
   arithmetic-shift
   integer-length
   pseudo-random-generator?
   pseudo-random-generator-vector?
   number->string
   string->number
   real->decimal-string
   integer-bytes->integer
   floating-point-bytes->real))


; These are most of the pure functions defined in docs.racket-lang.org/reference/pairs.html
; Functions that take other functions as arguments aren't included because those are only pure if the
; given argument functions are pure.
(define-literal-set pure-list-functions
  (pair?
   null?
   cons
   car
   cdr
   list?
   list
   list*
   length
   list-ref
   list-tail
   append
   reverse
   remq
   remv
   remw
   remq*
   remv*
   remw*
   memw
   memv
   memq
   assw
   assv
   assq
   caar
   cadr
   cdar
   cddr
   caaar
   caadr
   cadar
   caddr
   cdaar
   cdadr
   cddar
   cdddr
   caaaar
   caaadr
   caadar
   caaddr
   cadaar
   cadadr
   caddar
   cadddr
   cdaaar
   cdaadr
   cdadar
   cdaddr
   cddaar
   cddadr
   cdddar
   cddddr
   cons?
   empty?
   first
   rest
   second
   third
   fourth
   fifth
   sixth
   seventh
   eighth
   ninth
   tenth
   eleventh
   twelfth
   thirteenth
   fourteenth
   fifteenth
   last
   last-pair
   make-list
   list-set
   take
   drop
   split-at
   take-right
   drop-right
   split-at-right
   add-between
   append*
   flatten
   range
   inclusive-range
   combinations
   in-combinations
   permutations
   in-permutations
   cartesian-product))


(define-literal-set pure-functions
  #:literal-sets (pure-numeric-functions pure-list-functions)
  (hash-ref))


(define-syntax-class pure-expression
  #:literals (and or if cond when unless)
  (pattern :literal-constant)
  (pattern :id)
  (pattern (f:id arg:pure-expression ...)
    #:when ((literal-set->predicate pure-functions) (attribute f)))
  (pattern ((~or and or if when unless) subexpr:pure-expression ...))
  (pattern (cond clause:cond-clause ...)))


(define-syntax-class cond-clause
  #:literals (else)
  (pattern [test:pure-expression expr:pure-expression ...])
  (pattern [else expr:pure-expression ...]))
