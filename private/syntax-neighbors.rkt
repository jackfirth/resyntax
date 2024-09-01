#lang racket/base


;; This module provides a means to track which subforms of a syntax object have remained
;; unchanged and remained neighbors after a syntax object transformation. The function
;; syntax-mark-original-neighbors traverses all forms within a syntax object and stores
;; their original neighboring forms within syntax properties. Storing this metadata in
;; properties allows it to be preserved when macros, refactoring rules, and other syntax
;; transformations shuffle subforms around. This is used by Resyntax to preserve formatting
;; and comments when sequences of adjacent forms are left unchanged by a refactoring rule.


(require racket/contract/base)


(provide
 ~replacement
 (contract-out
  [syntax-original-leading-neighbor (-> syntax? (or/c syntax? #false))]
  [syntax-original-trailing-neighbor (-> syntax? (or/c syntax? #false))]
  [syntax-originally-neighbors? (-> syntax? syntax? boolean?)]
  [syntax-mark-original-neighbors (-> syntax? syntax?)]
  [syntax-extract-original (-> syntax? syntax?)]))


(require guard
         racket/syntax-srcloc
         syntax/parse
         syntax/parse/experimental/template)


(module+ test
  (require (submod "..")
           racket/syntax
           rackunit))


;@----------------------------------------------------------------------------------------------------


(define (syntax-mark-original-neighbors stx)
  (syntax-parse stx
    [(~and (subform ...+) (_ trailing-neighbor ...) (leading-neighbor ... _))
     (define leading-neighbors (cons #false (attribute leading-neighbor)))
     (define trailing-neighbors (append (attribute trailing-neighbor) (list #false)))
     (define results
       (for/list ([leading (in-list leading-neighbors)]
                  [trailing (in-list trailing-neighbors)]
                  [subform-stx (in-list (attribute subform))])
         (define leading-pos (and leading (syntax-position leading)))
         (define trailing-pos (and trailing (syntax-position trailing)))
         (define subform-pos (syntax-position subform-stx))
         (mark-neighbors (syntax-mark-original-neighbors subform-stx)
                         #:leading-neighbor (and leading (< leading-pos subform-pos) leading)
                         #:trailing-neighbor (and trailing (< subform-pos trailing-pos) trailing))))
     (datum->syntax stx results stx stx)]
    [_ stx]))


(define (mark-neighbors stx #:leading-neighbor leading-stx #:trailing-neighbor trailing-stx)
  (define stx-with-leading
    (if leading-stx
        (syntax-property stx 'original-leading-neighbor leading-stx)
        stx))
  (if trailing-stx
      (syntax-property stx-with-leading
                       'original-trailing-neighbor
                       trailing-stx)
      stx-with-leading))


(define (syntax-original-leading-neighbor stx)
  (syntax-property stx 'original-leading-neighbor))


(define (syntax-original-trailing-neighbor stx)
  (syntax-property stx 'original-trailing-neighbor))


(define-template-metafunction (~replacement stx)
  (syntax-parse stx
    [(_ new-stx #:original orig-syntax)
     (syntax-property #'new-stx 'replacement-for #'orig-syntax)]))


(define (syntax-extract-original stx)
  (or (syntax-property stx 'replacement-for) stx))


(define (syntax-originally-neighbors? left-stx right-stx)
  (let* ([left-stx (syntax-extract-original left-stx)]
         [right-stx (syntax-extract-original right-stx)])
    (guarded-block
      (define left-trailer (syntax-original-trailing-neighbor left-stx))
      (define right-leader (syntax-original-leading-neighbor right-stx))
      (guard (and left-trailer right-leader) #:else #false)
      (define left-srcloc (syntax-srcloc left-stx))
      (define left-trailer-srcloc (syntax-srcloc left-trailer))
      (define right-srcloc (syntax-srcloc right-stx))
      (define right-leader-srcloc (syntax-srcloc right-leader))
      (guard (and left-srcloc left-trailer-srcloc right-srcloc right-leader-srcloc) #:else #false)
      (and (equal? left-trailer-srcloc right-srcloc)
           (equal? right-leader-srcloc left-srcloc)))))


(module+ test
  (test-case "syntax-mark-original-neighbors"
    (define stx #'(foo (a b c) bar (baz)))
    (define marked (syntax-mark-original-neighbors stx))
    (check-equal? (syntax->datum marked) (syntax->datum stx))
    (define/with-syntax (foo* (a* b* c*) bar* (baz*)) marked)
    (check-false (syntax-original-leading-neighbor #'foo*))
    (check-equal? (syntax->datum (syntax-original-trailing-neighbor #'foo*)) '(a b c))
    (check-false (syntax-original-leading-neighbor #'a*))
    (check-equal? (syntax->datum (syntax-original-trailing-neighbor #'a*)) 'b)
    (check-equal? (syntax->datum (syntax-original-leading-neighbor #'b*)) 'a)
    (check-equal? (syntax->datum (syntax-original-trailing-neighbor #'b*)) 'c)
    (check-equal? (syntax->datum (syntax-original-leading-neighbor #'c*)) 'b)
    (check-false (syntax-original-trailing-neighbor #'c*))
    (check-equal? (syntax->datum (syntax-original-leading-neighbor #'bar*)) '(a b c))
    (check-equal? (syntax->datum (syntax-original-trailing-neighbor #'bar*)) '(baz))
    (check-false (syntax-original-leading-neighbor #'baz*))
    (check-false (syntax-original-trailing-neighbor #'baz*))
    (check-false (syntax-originally-neighbors? #'foo* #'b*))
    (check-true (syntax-originally-neighbors? #'a* #'b*))
    (check-true (syntax-originally-neighbors? #'b* #'c*))
    (check-false (syntax-originally-neighbors? #'c* #'bar*))
    (check-false (syntax-originally-neighbors? #'bar* #'baz*))))
