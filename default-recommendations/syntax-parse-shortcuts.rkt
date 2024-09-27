#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [syntax-parse-shortcuts refactoring-suite?]))


(require resyntax/base
         resyntax/default-recommendations/private/syntax-lines
         syntax/parse/define)


;@----------------------------------------------------------------------------------------------------


(define define-simple-macro-migration-name-length-increase
  (- (string-length "define-syntax-parse-rule") (string-length "define-simple-macro")))

(define-refactoring-rule define-simple-macro-to-define-syntax-parse-rule
  #:description "The `define-simple-macro` form has been renamed to `define-syntax-parse-rule`."
  #:literals (define-simple-macro)
  (original:define-simple-macro header form ...)

  ;; The define-simple-macro is a renamed alias of define-syntax-parse-rule, so it's
  ;; free-identifier=?. As a result, we need to check the actual symbol of the identifier instead of
  ;; just its binding. See https://github.com/jackfirth/resyntax/issues/106.
  #:when (equal? (syntax-e #'original) 'define-simple-macro)

  #:do
  [(define should-reformat?
     (and (equal? (syntax-line #'original) (syntax-line #'header))
          (or (multiline-syntax? #'header)
              (> (+ (syntax-column #'header)
                    (syntax-span #'header)
                    define-simple-macro-migration-name-length-increase)
                 102))))]

  #:with new-id (if should-reformat?
                    #'define-syntax-parse-rule
                    #'(~focus-replacement-on define-syntax-parse-rule))
   
  (new-id header form ...))


(define-refactoring-suite syntax-parse-shortcuts
  #:rules (define-simple-macro-to-define-syntax-parse-rule))
