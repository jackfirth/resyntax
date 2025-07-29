#lang racket/base

;; This module provides a means to track which subforms of a syntax object have remained
;; unchanged and remained neighbors after a syntax object transformation. The function
;; syntax-mark-original-neighbors traverses all forms within a syntax object and stores
;; their original neighboring forms within syntax properties. Storing this metadata in
;; properties allows it to be preserved when macros, refactoring rules, and other syntax
;; transformations shuffle subforms around. This is used by Resyntax to preserve formatting
;; and comments when sequences of adjacent forms are left unchanged by a refactoring rule.

(require racket/contract/base)

(provide (contract-out [syntax-original-path (-> syntax? (or/c syntax-path? #false))]
                       [syntax-has-original-path? (-> syntax? boolean?)]
                       [syntax-label-original-paths (-> syntax? syntax?)]
                       [syntax-originally-neighbors? (-> syntax? syntax? boolean?)]
                       [syntax-extract-originals-from-pair
                        (-> syntax? syntax? (values syntax? syntax?))]))

(require guard
         racket/match
         racket/struct
         racket/syntax-srcloc
         resyntax/private/logger
         resyntax/private/syntax-path
         syntax/parse
         syntax/parse/experimental/template)

(module+ test
  (require racket/syntax
           rackunit
           (submod "..")))

;@----------------------------------------------------------------------------------------------------

(define original-syntax-path-key 'original-syntax-path)

(define (syntax-label-original-paths stx)
  (syntax-label-paths stx original-syntax-path-key))

(define (syntax-has-original-path? stx)
  (and (syntax-property stx original-syntax-path-key) #true))

(define (syntax-original-path stx)
  ; The property value will be a cons tree if a macro produced a syntax object with the path property
  ; set. The main way this occurs is via `(begin x ...)`, as each of the `x` subforms counts as an
  ; "expansion" of the surrounding `(begin ...)` and therefore has its properties merged. In such a
  ; case, each `x` counts as the "result" and the `(begin ...)` counts as the "original", so if an
  ; `x` and the `(begin ...)` both have their paths set, the resulting property path will be
  ; `(cons <path-of-x> <path-of-(begin...)>)`. We therefore want to pick the *head* of any cons cells
  ; we encounter when looking up the original syntax path property value. There might be other cases
  ; where we want to look at the tail for some reason, but if those cases exist I haven't found them
  ; yet and they don't cause any of Resyntax's tests to fail.
  (let loop ([possible-cons-tree (syntax-property stx original-syntax-path-key)])
    (if (pair? possible-cons-tree)
        (loop (car possible-cons-tree))
        possible-cons-tree)))

(define (syntax-extract-originals-from-pair left-stx right-stx)
  (values (or (syntax-property left-stx 'tail-replacement-for)
              (syntax-property left-stx 'replacement-for)
              left-stx)
          (or (syntax-property right-stx 'head-replacement-for)
              (syntax-property right-stx 'replacement-for)
              right-stx)))

(define (syntax-originally-neighbors? left-stx* right-stx*)
  (define-values (left-stx right-stx) (syntax-extract-originals-from-pair left-stx* right-stx*))
  (guarded-block
   (define left-path (syntax-original-path left-stx))
   (define right-path (syntax-original-path right-stx))
   ;; If either of the above is missing, then they're not neighbors. We log a debug message in that
   ;; case to aide in debugging test failures caused by dropped comments.
   (guard left-path
          #:else (log-resyntax-debug (string-append "not neighbors because left-path is missing\n"
                                                    "  original left syntax: ~a\n"
                                                    "  original right syntax: ~a\n"
                                                    "  replacement left syntax: ~a\n"
                                                    "  replacement right syntax: ~a")
                                     (syntax->datum left-stx)
                                     (syntax->datum right-stx)
                                     (syntax->datum left-stx*)
                                     (syntax->datum right-stx*))
          #false)
   (guard right-path
          #:else (log-resyntax-debug (string-append "not neighbors because right-path is missing\n"
                                                    "  original left syntax: ~a\n"
                                                    "  original right syntax: ~a\n"
                                                    "  replacement left syntax: ~a\n"
                                                    "  replacement right syntax: ~a")
                                     (syntax->datum left-stx)
                                     (syntax->datum right-stx)
                                     (syntax->datum left-stx*)
                                     (syntax->datum right-stx*))
          #false)
   (define neighbors? (syntax-path-neighbors? left-path right-path))
   (unless neighbors?
     (log-resyntax-debug (string-append "not neighbors because syntax-path-neighbors? says so\n"
                                        "  original left path: ~a\n"
                                        "  original right path: ~a\n"
                                        "  original left syntax: ~a\n"
                                        "  original right syntax: ~a\n"
                                        "  replacement left syntax: ~a\n"
                                        "  replacement right syntax: ~a")
                         left-path
                         right-path
                         (syntax->datum left-stx)
                         (syntax->datum right-stx)
                         (syntax->datum left-stx*)
                         (syntax->datum right-stx*)))
   neighbors?))

(module+ test
  (test-case "syntax-originally-neighbors?"
    (define stx #'(foo (a b c) bar (baz)))
    (define labeled (syntax-label-original-paths stx))
    (check-equal? (syntax->datum labeled) (syntax->datum stx))
    (define/with-syntax (foo* (a* b* c*) bar* (baz*)) labeled)
    (check-false (syntax-originally-neighbors? #'foo* #'b*))
    (check-true (syntax-originally-neighbors? #'a* #'b*))
    (check-true (syntax-originally-neighbors? #'b* #'c*))
    (check-false (syntax-originally-neighbors? #'c* #'bar*))
    (check-false (syntax-originally-neighbors? #'bar* #'baz*))))

(define (improper-list-drop-tail improper-list)
  (cons (car improper-list)
        (let loop ([improper-list (cdr improper-list)])
          (if (pair? improper-list)
              (cons (car improper-list) (loop (cdr improper-list)))
              '()))))

(module+ test
  (test-case "improper-list-drop-tail"
    (check-equal? (improper-list-drop-tail '(1 2 3 . 4)) '(1 2 3))))
