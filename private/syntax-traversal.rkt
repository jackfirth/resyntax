#lang racket/base


(require racket/contract/base)


(provide atom
         expression-directly-enclosing
         syntax-find-first
         syntax-search
         syntax-search-everything
         syntax-traverse)


(require (for-syntax racket/base
                     resyntax/private/more-syntax-parse-classes)
         racket/sequence
         racket/stream
         syntax/parse
         syntax/parse/define)


(module+ test
  (require rackunit
           (submod "..")))


;@----------------------------------------------------------------------------------------------------


(define-syntax-class atom
  (pattern (~or :id :number :str :boolean :regexp :keyword)))


(define-syntax-class (expression-directly-enclosing id)
  (pattern (part ...)
    #:when (for/or ([part-stx (in-list (attribute part))]
                    #:when (identifier? part-stx))
             (free-identifier=? id part-stx)))
  (pattern (part ...+ . tail-part)
    #:when (for/or ([part-stx (in-list (cons #'tail-part (attribute part)))]
                    #:when (identifier? part-stx))
             (free-identifier=? id part-stx))))


(begin-for-syntax
  (define-syntax-class syntax-search-clause
    #:attributes (syntax-pattern [directive 1] output-stream)
    (pattern [syntax-pattern directive:syntax-parse-pattern-directive ... body:expr ... last-body]
      #:declare last-body (expr/c #'stream?)
      #:with output-stream #'(stream-lazy (let () body ... last-body.c)))
    (pattern [syntax-pattern directive:syntax-parse-pattern-directive ...]
      #:with output-stream #'(stream this-syntax))))


(define-syntax-parse-rule
  (syntax-search stx-expr
    (~optional (~seq #:skip-root? skip-root?) #:defaults ([skip-root? #'#false]))
    option:syntax-parse-option ...
    clause:syntax-search-clause ...)
  #:declare stx-expr (expr/c #'syntax?)
  (let ([skip-root-id skip-root?])
    (define-syntax-class search-case
      #:attributes (output-stream)
      (~@ . option) ...
      (pattern clause.syntax-pattern (~@ . clause.directive) ...
        #:attr output-stream clause.output-stream)
      ...)
    (let loop ([stx stx-expr.c] [root? #true])
      (stream-lazy
       (syntax-parse stx
         [child
          #:when (not (and skip-root-id root?))
          #:with (~var matched search-case) (attribute child)
          (attribute matched.output-stream)]
         [(part (... ...))
          #:cut
          (apply stream-append
                 (for/list ([part-stx (in-list (attribute part))])
                   (loop part-stx #false)))]
         [(part (... ...+) . tail-part)
          #:cut
          (stream-append (apply stream-append
                                (for/list ([part-stx (in-list (attribute part))])
                                  (loop part-stx #false)))
                         (loop #'tail-part #false))]
         [_ (stream)])))))


(define (syntax-search-everything stx)
  (stream-cons stx (syntax-search stx #:skip-root? #true [_ (syntax-search-everything this-syntax)])))


(module+ test
  (test-case "syntax-search"
    (define stx
      #'(define (foo)
          (cons x y)
          (define (bar)
            (cons a b))
          (cons c d)))
    (define actual
      (sequence->list
       (syntax-search stx
         #:literals (cons)
         [(cons _ _) (stream (syntax->datum this-syntax))])))
    (define expected '((cons x y) (cons a b) (cons c d)))
    (check-equal? actual expected)))


(define-syntax-parse-rule (syntax-find-first stx-expr
                            option:syntax-parse-option ...
                            syntax-pattern
                            directive:syntax-parse-pattern-directive ...)
  (let ()
    (define results (syntax-search stx-expr (~@ . option) ... [syntax-pattern (~@ . directive) ...]))
    (and (not (stream-empty? results)) (stream-first results))))


(module+ test
  (test-case "syntax-find-first"
    (define stx
      #'(define (foo)
          (cons x y)
          (define (bar)
            (cons a b))
          (cons c d)))
    (define actual (syntax-find-first stx #:literals (cons) #:datum-literals (a) (cons a _)))
    (check-equal? (syntax->datum actual) '(cons a b))
    (check-false (syntax-find-first stx #:literals (cons) (cons _ _ _)))))


(define-syntax-parse-rule
  (syntax-traverse (~var stx-expr (expr/c #'syntax?))
    (~optional (~seq #:skip-root? skip-root?) #:defaults ([skip-root? #'#false]))
    option:syntax-parse-option ...
    [clause-pattern directive:syntax-parse-pattern-directive ... clause-body:expr ...+] ...)
  (let ([skip-root-id skip-root?])
    (define-syntax-class traversal-case
      #:attributes (traversed)
      (~@ . option) ...
      (pattern clause-pattern (~@ . directive) ...
        #:attr traversed (let () clause-body ...)) ...)
    (let loop ([stx stx-expr.c] [root? #true])

      (define (rewrap-datum datum)
        (datum->syntax stx datum stx stx))

      (syntax-parse stx

        [child
         #:when (not (and skip-root-id root?))
         #:with (~var matched traversal-case) (attribute child)
         (define case-scope (make-syntax-introducer))
         (case-scope (attribute matched.traversed) 'add)]
        
        [(part (... ...))
         #:cut
         (rewrap-datum
          (for/list ([child (in-list (attribute part))])
            (loop child #false)))]
        [(part (... ...+) . tail-part)
         #:cut
         (define traversed-children
           (for/list ([child (in-list (attribute part))])
             (loop child #false)))
         (define traversed-tail (loop #'tail-part #false))
         (rewrap-datum (append traversed-children traversed-tail))]
        [_ stx]))))


(module+ test
  (test-case "syntax-traverse"
    (define stx
      #'(define (foo)
          (cons x y)
          (define (bar)
            (cons a b))
          (cons c d)))
    (define actual
      (syntax-traverse stx
        #:literals (cons)
        [(cons _ _) #'CONS-EXPRESSION]))
    (define expected
      '(define (foo)
         CONS-EXPRESSION
         (define (bar)
           CONS-EXPRESSION)
         CONS-EXPRESSION))
    (check-equal? (syntax->datum actual) expected))

  (test-case "syntax-traverse #:skip-root? true"
    (define stx #'(a b (c d) e))
    (define actual
      (syntax-traverse stx
        #:skip-root? #true
        [(_ ...) #'LIST]))
    (check-equal? (syntax->datum actual) '(a b LIST e)))

  (test-case "syntax-traverse #:skip-root? true doesn't execute directives on root"
    (define stx #'(a b (c d) e))
    (define execution-count 0)
    (syntax-traverse stx
      #:skip-root? #true
      [(_ ...)
       #:do [(set! execution-count (add1 execution-count))]
       #'LIST])
    (check-equal? execution-count 1))

  (test-case "syntax-traverse #:skip-root? false"
    (define stx #'(a b (c d) e))
    (define actual
      (syntax-traverse stx
        #:skip-root? #false
        [(_ ...) #'LIST]))
    (check-equal? (syntax->datum actual) 'LIST))

  (test-case "syntax-traverse originality"
    (define stx (read-syntax #false (open-input-string "(1 2 (a b) 3 4)")))
    (check-true (syntax-original? stx))
    (define traversed-stx
      (syntax-traverse stx
        [(_ id:id) (attribute id)]))
    (check-equal? (syntax->datum traversed-stx) '(1 2 b 3 4))
    (check-true (syntax-original? traversed-stx))
    (define/syntax-parse (1* 2* b* 3* 4*) traversed-stx)
    (check-true (syntax-original? #'1*))
    (check-true (syntax-original? #'2*))
    (check-false (syntax-original? #'b*))
    (check-true (syntax-original? #'3*))
    (check-true (syntax-original? #'4*))))
