#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [named-let-loopification refactoring-suite?]))


(require racket/list
         resyntax/base
         resyntax/default-recommendations/let-replacement/private/let-binding
         resyntax/default-recommendations/private/list-function
         resyntax/default-recommendations/private/literal-constant
         resyntax/private/identifier-naming
         resyntax/private/logger
         resyntax/private/syntax-traversal
         syntax/parse)


;@----------------------------------------------------------------------------------------------------


(define-refactoring-rule named-let-loop-to-for-in-range
  #:description "This named `let` expression is equivalent to a `for` loop that uses `in-range`."
  #:literals (let when < +)
  (let loop:id ([x:id start:expr])
    (when (< x2:id (~or stop:id stop:literal-constant))
      body ...+
      (loop2:id (+ x3:id (~or step:id step:literal-constant)))))
  #:when (free-identifier=? (attribute x) (attribute x2))
  #:when (free-identifier=? (attribute x) (attribute x3))
  #:when (free-identifier=? (attribute loop) (attribute loop2))
  #:when (or (not (identifier? (attribute step)))
             (identifier-binding-unchanged-in-context? (attribute step) this-syntax))
  #:when (not (syntax-find-first #'(body ...) id:id
                #:when (free-identifier=? (attribute id) (attribute loop))))
  (for ([x (in-range start stop step)])
    body ...))


(define-refactoring-rule named-let-loop-to-for/list
  #:description "This named `let` expression is equivalent to a `for/list` loop."
  #:literals (let cond else null? empty? null quote car first cdr rest cons)
  (let loop:id ([vs:id init-list])
    (cond
      [(:empty-predicate-by-any-name vs2:id) :empty-list-by-any-name]
      [else
       loop-body:expr ...
       (cons loop-element:expr (loop2:id (:rest-by-any-name vs3:id)))]))
  #:when (log-resyntax-rule-condition (free-identifier=? #'loop #'loop2))
  #:when (log-resyntax-rule-condition (free-identifier=? #'vs #'vs2))
  #:when (log-resyntax-rule-condition (free-identifier=? #'vs #'vs3))
  #:when (log-resyntax-rule-condition
          (not
           (for/or ([body-stx (in-list (cons #'loop-element (attribute loop-body)))])
             (syntax-find-first body-stx
               (~and (~var usage (expression-directly-enclosing (attribute vs)))
                     (~not (:first-by-any-name _)))))))
  #:cut

  #:with element-id (depluralize-id #'vs)

  #:with (modified-result-element modified-body ...)
  (for/list ([body-stx (cons #'loop-element (attribute loop-body))])
    (syntax-traverse body-stx
      [(:first-by-any-name vs-usage:id)
       #:when (free-identifier=? (attribute vs-usage) (attribute vs))
       (attribute element-id)]))

  (for/list ([element-id (in-list init-list)])
    modified-body ...
    modified-result-element))


(define-refactoring-rule named-let-loop-to-for/and
  #:description "This named `let` expression is equivalent to a `for/and` loop."
  #:literals (let cond else)
  (let loop:id ([vs:id init-list])
    (cond
      [(:empty-predicate-by-any-name vs2:id) #true]
      [element-condition:expr (loop2:id (:rest-by-any-name vs3:id))]
      [else #false]))

  #:when (free-identifier=? (attribute loop) (attribute loop2))
  #:when (free-identifier=? (attribute vs) (attribute vs2))
  #:when (free-identifier=? (attribute vs) (attribute vs3))
  #:when (not (syntax-find-first (attribute element-condition)
                (~and (~var usage (expression-directly-enclosing (attribute vs)))
                      (~not (:first-by-any-name _)))))
  #:cut

  #:with element-id (depluralize-id (attribute vs))
  #:with modified-element-condition
  (syntax-traverse (attribute element-condition)
    [(:first-by-any-name vs-usage:id)
     #:when (free-identifier=? (attribute vs-usage) (attribute vs))
     (attribute element-id)])

  (for/and ([element-id (in-list init-list)])
    modified-element-condition))


(define-refactoring-rule named-let-loop-to-for/or
  #:description "This named `let` expression is equivalent to a `for/or` loop."
  #:literals (let cond else)
  (let loop:id ([vs:id init-list])
    (cond
      [(:empty-predicate-by-any-name vs2:id) #false]
      [element-condition:expr #true]
      [else (loop2:id (:rest-by-any-name vs3:id))]))

  #:when (free-identifier=? (attribute loop) (attribute loop2))
  #:when (free-identifier=? (attribute vs) (attribute vs2))
  #:when (free-identifier=? (attribute vs) (attribute vs3))
  #:when (not (syntax-find-first (attribute element-condition)
                (~and (~var usage (expression-directly-enclosing (attribute vs)))
                      (~not (:first-by-any-name _)))))
  #:cut

  #:with element-id (depluralize-id (attribute vs))
  #:with modified-element-condition
  (syntax-traverse (attribute element-condition)
    [(:first-by-any-name vs-usage:id)
     #:when (log-resyntax-rule-condition (free-identifier=? (attribute vs) (attribute vs-usage)))
     (attribute element-id)])

  (for/or ([element-id (in-list init-list)])
    modified-element-condition))


(define-refactoring-rule named-let-loop-to-for/first-in-naturals
  #:description
  "This named `let` expression can be replaced by a simpler, equivalent `for/first` loop."
  #:literals (let cond else + add1)

  (let loop:id ([i:id start:expr])
    (cond
      [condition:expr
       (loop2:id (~or (+ i2:id 1) (add1 i2:id)))]
      [else
       body:expr ...+]))
  #:when (free-identifier=? (attribute loop) (attribute loop2))
  #:when (free-identifier=? (attribute i) (attribute i2))
  #:when (not (syntax-find-first #'(condition body ...)
                id:id #:when (free-identifier=? (attribute id) (attribute loop))))

  (for/first ([i (in-naturals start)]
              #:unless condition)
    body ...))


(define-refactoring-rule named-let-loop-to-for/first-in-vector
  #:description
  "This named `let` expression can be replaced by a simpler, equivalent `for/first` loop."
  #:literals (let add1 + vector-length vector-ref if and <)
  (let loop1:id ([i1:id 0])
    (and (< i2:id (vector-length vec1:id))
         (let ([x:id (vector-ref vec2:id i3:id)])
           (if condition:expr
               true-branch:expr
               (loop2:id (~or (add1 i4:id) (+ i4:id 1) (+ 1 i4:id)))))))
  #:when (free-identifier=? #'loop1 #'loop2)
  #:when (free-identifier=? #'i1 #'i2)
  #:when (free-identifier=? #'i1 #'i3)
  #:when (free-identifier=? #'i1 #'i4)
  #:when (free-identifier=? #'vec1 #'vec2)
  (for/first ([x (in-vector vec1)] #:when condition)
    true-branch))


(define-refactoring-rule named-let-read-loop-to-for-in-port
  #:description
  "A named `let` that repeatedly calls `read` can be rewritten to a `for` loop using `in-port`."
  #:literals (let unless read eof-object?)
  (let loop:id ([v:id (read)])
    (unless (eof-object? v2:id)
      body ...
      (loop2:id (read))))
  #:when (free-identifier=? (attribute v) (attribute v2))
  #:when (free-identifier=? (attribute loop) (attribute loop2))
  (for ([v (in-port)])
    body ...))


(define-refactoring-suite named-let-loopification
  #:rules (named-let-loop-to-for-in-range
           named-let-loop-to-for/and
           named-let-loop-to-for/first-in-naturals
           named-let-loop-to-for/first-in-vector
           named-let-loop-to-for/list
           named-let-loop-to-for/or
           named-let-read-loop-to-for-in-port))
