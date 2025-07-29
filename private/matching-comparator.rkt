#lang racket/base

(provide matching-comparator)

(require (for-syntax racket/base
                     racket/list
                     racket/match
                     racket/syntax)
         racket/match
         rebellion/base/comparator
         syntax/parse/define)

;@----------------------------------------------------------------------------------------------------

(begin-for-syntax
  (define-splicing-syntax-class
   compare-directive
   #:attributes (comparable-expr comparator-expr comparator-expr-lexical-context)
   (pattern (~seq #:compare comparable-expr:expr comparator-expr:expr)
     #:with comparator-expr-lexical-context (syntax-local-introduce (attribute comparator-expr)))
   (pattern (~seq #:compare comparable-expr:expr)
     #:with comparator-expr #'real<=>
     #:with comparator-expr-lexical-context (attribute comparator-expr)))

  (define-syntax-class matching-comparator-clause
    #:attributes
    (match-pattern [comparable-expr 1] [comparator-expr 1] [comparator-expr-lexical-context 1])
    (pattern [match-pattern :compare-directive ...])))

(define (return-comparison-unless-equivalent comparison)
  (and (not (equal? comparison equivalent)) comparison))

(begin-for-syntax
  (define (build-static-comparison-chain comparisons)
    (define/with-syntax (comparison ... last-comparison) comparisons)
    #'(or (return-comparison-unless-equivalent comparison) ... last-comparison)))

(define-syntax-parse-rule (matching-comparator clause:matching-comparator-clause ...+)

  #:with ((comparator-id ...)
          ...) (let ([counts (make-hash '())])
                 (for/list ([pattern-comparator-exprs (in-list (attribute clause.comparator-expr))]
                            [pattern-comparator-contexts
                             (in-list (attribute clause.comparator-expr-lexical-context))]
                            [pattern-index (in-naturals)])
                   (for/list ([expr (in-list pattern-comparator-exprs)]
                              [context (in-list pattern-comparator-contexts)]
                              [i (in-naturals)])
                     (cond
                       [(identifier? expr)
                        (define occurrence-index (hash-ref! counts (syntax-e expr) 0))
                        (hash-update! counts (syntax-e expr) add1)
                        (format-id context "~a-id~a" expr occurrence-index #:subs? #false)]
                       [else (format-id context "pattern~a-comparator~a" pattern-index i)]))))

  #:with (([left-comparable-thunk-id right-comparable-thunk-id] ...)
          ...) (for/list ([pattern-comparable-exprs (in-list (attribute clause.comparable-expr))]
                          [pattern-index (in-naturals)])
                 (define counts (make-hash))
                 (for/list ([expr (in-list pattern-comparable-exprs)]
                            [i (in-naturals)])
                   (define (make-id side)
                     (define context (syntax-local-introduce expr))
                     (cond
                       [(identifier? expr)
                        (define occurrence-index (hash-ref! counts (syntax-e expr) 0))
                        (hash-update! counts (syntax-e expr) add1)
                        (format-id context
                                   "~a-~a~a"
                                   side
                                   expr
                                   (if (zero? occurrence-index) "" occurrence-index)
                                   #:subs? #false)]
                       [else (format-id context "~a-pattern~a-comparable~a" side pattern-index i)]))
                   (list (make-id 'left) (make-id 'right))))

  #:with (pattern-index ...) (for/list ([i (in-range 0 (length (attribute clause.match-pattern)))])
                               i)

  #:with (comparable-values-expr
          ...) (for/list ([pattern-comaprable-exprs (in-list (attribute clause.comparable-expr))]
                          [pattern-index (in-naturals)])
                 (build-comparable-values-expression pattern-index pattern-comaprable-exprs))

  #:with left-comparables-id #'left-comparables
  #:with right-comparables-id #'right-comparables

  #:with
  (comparable-values-comparison ...)
  (for/list ([pattern-comparator-ids (in-list (attribute comparator-id))]
             [pattern-comparable-exprs (in-list (attribute clause.comparable-expr))]
             [pattern-left-comparable-ids (in-list (attribute left-comparable-thunk-id))]
             [pattern-right-comparable-ids (in-list (attribute right-comparable-thunk-id))])
    (build-comparable-values-comparison (attribute left-comparables-id)
                                        (attribute right-comparables-id)
                                        pattern-comparator-ids
                                        pattern-comparable-exprs
                                        pattern-left-comparable-ids
                                        pattern-right-comparable-ids))

  (let ([comparator-id clause.comparator-expr] ...
        ...)
    (define (pattern-selector v)
      (match v
        [clause.match-pattern comparable-values-expr] ...))
    (make-comparator (λ (left right)
                       (define-values (left-index left-comparables-id) (pattern-selector left))
                       (define-values (right-index right-comparables-id) (pattern-selector right))
                       (cond
                         [(< left-index right-index) lesser]
                         [(> left-index right-index) greater]
                         [(equal? left-index 'pattern-index) comparable-values-comparison] ...)))))

(begin-for-syntax
  (define (build-comparable-values-expression index comparable-exprs)
    (define wrapped-comparable-exprs
      (for/list ([e (in-list comparable-exprs)])
        (if (identifier? e)
            e
            #`(λ () #,e))))
    (define wrapped-comparables
      (match (length comparable-exprs)
        [0 #'#false]
        [1 (first wrapped-comparable-exprs)]
        [_ #`(λ () (values #,@wrapped-comparable-exprs))]))
    #`(values '#,index #,wrapped-comparables)))

(begin-for-syntax
  (define (build-comparable-values-comparison left-values-id
                                              right-values-id
                                              comparator-ids
                                              comparable-exprs
                                              left-comparable-ids
                                              right-comparable-ids)
    (define comparisons
      (for/list ([e (in-list comparable-exprs)]
                 [comparator-id (in-list comparator-ids)]
                 [left (in-list left-comparable-ids)]
                 [right (in-list right-comparable-ids)])
        (if (identifier? e)
            #`(compare #,comparator-id #,left #,right)
            #`(compare #,comparator-id (#,left) (#,right)))))
    (match (length comparator-ids)
      [0 #'equivalent]
      [1 #`(compare #,(first comparator-ids) #,left-values-id #,right-values-id)]
      [_
       #`(let-values ([(#,@left-comparable-ids) (#,left-values-id)]
                      [(#,@right-comparable-ids) (#,right-values-id)])
           #,(build-static-comparison-chain comparisons))])))
