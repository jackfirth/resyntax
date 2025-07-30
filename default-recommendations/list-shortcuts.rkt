#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [list-shortcuts refactoring-suite?]))


(require (for-syntax racket/base)
         guard
         racket/function
         racket/list
         racket/sequence
         racket/set
         resyntax/base
         resyntax/default-recommendations/private/lambda-by-any-name
         resyntax/default-recommendations/private/literal-constant
         resyntax/default-recommendations/private/syntax-identifier-sets
         syntax/parse)


;@----------------------------------------------------------------------------------------------------


(define-refactoring-rule first-reverse-to-last
  #:description "The `last` function can be used to get the last item from a list."
  #:literals (first reverse)
  ;; This can't match (car (reverse list)) because of https://github.com/jackfirth/resyntax/issues/11
  (first (reverse list))
  (last list))


(define-syntax-class null-test
  #:attributes (subject)
  #:literals (eq? eqv? equal? null list quote)
  (pattern ((~or eq? eqv? equal?) subject (~or null (list) '())))
  (pattern ((~or eq? eqv? equal?) (~or null (list) '()) subject)))


;; This can't suggest using empty? because of https://github.com/jackfirth/resyntax/issues/11
(define-refactoring-rule equal-null-list-to-null-predicate
  #:description "The `null?` predicate can be used to test for the empty list."
  test:null-test
  (null? test.subject))


(define-refactoring-rule append*-and-map-to-append-map
  #:description
  "The `append-map` function can be used to map each element into multiple elements in a single pass."
  #:literals (append* map)
  (append* (map f lst))
  (append-map f lst))


(define-refactoring-rule append-single-list-to-single-list
  #:description "The `append` function does nothing when applied to only one list."
  #:literals (append)
  (append lst)
  lst)


(define-refactoring-rule filter-to-remove*
  #:description
  "The `remove*` function is a simpler way to remove all elements of one list from another."
  #:literals (filter andmap not equal?)
  (filter (_:lambda-by-any-name (x1) (andmap (_:lambda-by-any-name (y1) (not (equal? x2 y2))) ys)) xs)
  #:when (free-identifier=? #'x1 #'x2)
  #:when (free-identifier=? #'y1 #'y2)
  #:when (not (set-member? (syntax-free-identifiers #'ys) #'x1))
  (remove* ys xs))


(define-refactoring-rule filter-to-remv*
  #:description
  "The `remv*` function is a simpler way to remove all elements of one list from another."
  #:literals (filter andmap not eqv?)
  (filter (_:lambda-by-any-name (x1) (andmap (_:lambda-by-any-name (y1) (not (eqv? x2 y2))) ys)) xs)
  #:when (free-identifier=? #'x1 #'x2)
  #:when (free-identifier=? #'y1 #'y2)
  #:when (not (set-member? (syntax-free-identifiers #'ys) #'x1))
  (remv* ys xs))


(define-refactoring-rule filter-to-remq*
  #:description
  "The `remq*` function is a simpler way to remove all elements of one list from another."
  #:literals (filter andmap not eq?)
  (filter (_:lambda-by-any-name (x1) (andmap (_:lambda-by-any-name (y1) (not (eq? x2 y2))) ys)) xs)
  #:when (free-identifier=? #'x1 #'x2)
  #:when (free-identifier=? #'y1 #'y2)
  #:when (not (set-member? (syntax-free-identifiers #'ys) #'x1))
  (remq* ys xs))


(define-refactoring-rule sort-with-keyed-comparator-to-sort-by-key
  #:description "This `sort` expression can be replaced with a simpler, equivalent expression."
  #:literals (sort <)
  (sort lst (_:lambda-by-any-name (x1:id y1:id) (less-than:id (f1:id x2:id) (f2:id y2:id))))
  #:when (free-identifier=? #'x1 #'x2)
  #:when (free-identifier=? #'y1 #'y2)
  #:when (free-identifier=? #'f1 #'f2)
  (sort lst less-than #:key f1))


(define-syntax-class unquoted
  #:attributes (expr literal?)
  #:literals (unquote)
  (pattern expr:literal-constant #:attr literal? #true)
  (pattern (unquote expr) #:attr literal? #false))


(define-refactoring-rule quasiquote-to-list
  #:description "This quasiquotation is equialent to a simple `list` call."
  #:literals (quasiquote)
  (quasiquote (arg:unquoted ...))
  #:when (for/or ([literal? (in-list (attribute arg.literal?))])
           (not literal?))
  (list arg.expr ...))


(define-refactoring-rule quasiquote-to-append
  #:description "This quasiquotation is equialent to calling `append`."
  #:literals (quasiquote unquote-splicing)
  (quasiquote ((unquote-splicing arg) ...))
  (append arg ...))


(define-refactoring-rule ignored-map-to-for-each
  #:description
  "The result of this `map` expression is unused. Make that explicit with `for-each` instead."
  #:literals (map)
  (map proc list ...)
  #:when (equal? (syntax-property this-syntax 'expression-result) 'ignored)
  (for-each proc list ...))


(define-refactoring-rule build-list-const-to-make-list
  #:description "Using `build-list` with `const` is equivalent to using `make-list`."
  #:literals (build-list const)
  (build-list count (const elem))
  (make-list count elem))


(define/guard (all-free-identifier=? ids)
  (guard-match (cons first-id remaining-ids) (sequence->list ids) #:else #false)
  (for/and ([id (in-list remaining-ids)])
    (free-identifier=? first-id id)))


(define/guard (contiguous-increasing-integer-series? ints)
  (define int-list (sequence->list ints))
  (guard (not (empty? int-list)) #:else #false)
  (for/and ([previous (in-list int-list)]
            [next (in-list (rest int-list))])
    (equal? (add1 previous) next)))


(define-syntax-class list-selection-expression
  #:attributes (target-list-id index)
  #:literals (list-ref
              first
              second
              third
              fourth
              fifth
              sixth
              seventh
              eighth
              ninth
              tenth
              car
              cadr
              caddr
              cadddr)

  (pattern (list-ref target-list-id:id index-stx:nat) #:attr index (syntax-e #'index-stx))
  (pattern (first target-list-id:id) #:attr index 0)
  (pattern (second target-list-id:id) #:attr index 1)
  (pattern (third target-list-id:id) #:attr index 2)
  (pattern (fourth target-list-id:id) #:attr index 3)
  (pattern (fifth target-list-id:id) #:attr index 4)
  (pattern (sixth target-list-id:id) #:attr index 5)
  (pattern (seventh target-list-id:id) #:attr index 6)
  (pattern (eighth target-list-id:id) #:attr index 7)
  (pattern (ninth target-list-id:id) #:attr index 8)
  (pattern (tenth target-list-id:id) #:attr index 9)
  (pattern (car target-list-id:id) #:attr index 0)
  (pattern (cadr target-list-id:id) #:attr index 1)
  (pattern (caddr target-list-id:id) #:attr index 2)
  (pattern (cadddr target-list-id:id) #:attr index 3))


(define-refactoring-rule list-selectors-to-take-and-drop
  #:description
  "This list expression is constructing a sublist of a larger list, which can be expressed more\
 clearly with `take` and `drop`."
  #:literals (list)

  (list selection:list-selection-expression ...)

  #:when (>= (length (attribute selection)) 3)

  #:when (all-free-identifier=? (attribute selection.target-list-id))
  #:with target-list-id (first (attribute selection.target-list-id))

  #:when (contiguous-increasing-integer-series? (attribute selection.index))
  #:do [(define first-index (first (attribute selection.index)))
        (define last-index (last (attribute selection.index)))]
  
  #:with target-list-with-prefix-dropped
  (if (zero? first-index) #'target-list-id #`(drop target-list-id #,first-index))

  #:with amount-to-take (- (add1 last-index) first-index)

  (take target-list-with-prefix-dropped amount-to-take))


(define-syntax-class static-list-expression
  #:literals (cons list list*)
  #:attributes ([element 1] maker improper-tail simplifiable?)

  (pattern ((~and list maker) ~! element ...)
    #:attr improper-tail #false
    #:attr simplifiable? #false)

  (pattern ((~and list* maker) element ... (~and improper-tail (~not :static-list-expression)))
    #:attr simplifiable? #false)

  (pattern (cons ~! head tail:static-list-expression)
    #:attr [element 1] (cons #'head (attribute tail.element))
    #:attr maker (attribute tail.maker)
    #:attr improper-tail (attribute tail.improper-tail)
    #:attr simplifiable? #true)

  (pattern (list* head ... tail:static-list-expression)
    #:attr [element 1] (append (attribute head) (attribute tail.element))
    #:attr maker (attribute tail.maker)
    #:attr improper-tail (attribute tail.improper-tail)
    #:attr simplifiable? #true))


(define-refactoring-rule consing-onto-static-list
  #:description "This list-constructing expression can be simplified"
  #:literals (cons list)
  expr:static-list-expression
  #:when (attribute expr.simplifiable?)
  (expr.maker expr.element ... (~? expr.improper-tail)))


(define-refactoring-rule length-comparison-to-empty-check
  #:description
  "Checking if a list's length is zero is less efficient than using the `empty?` predicate"
  #:literals (length equal? eqv? eq? = zero?)
  (~or ((~or equal? eqv? eq? =) (length list-expr:expr) 0)
       ((~or equal? eqv? eq? =) 0 (length list-expr:expr))
       (zero? (length list-expr:expr)))
  (empty? list-expr))


(define-refactoring-suite list-shortcuts
  #:rules (append-single-list-to-single-list
           append*-and-map-to-append-map
           build-list-const-to-make-list
           consing-onto-static-list
           equal-null-list-to-null-predicate
           filter-to-remove*
           filter-to-remq*
           filter-to-remv*
           first-reverse-to-last
           ignored-map-to-for-each
           length-comparison-to-empty-check
           list-selectors-to-take-and-drop
           quasiquote-to-append
           quasiquote-to-list
           sort-with-keyed-comparator-to-sort-by-key))
