#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [exception-suggestions refactoring-suite?]))


(require racket/string
         resyntax/base
         resyntax/default-recommendations/private/literal-constant
         syntax/parse)


;@----------------------------------------------------------------------------------------------------


(define-refactoring-rule literal-exception-handler-to-lambda
  #:description
  "A `with-handlers` handler should be a procedure. Wrap this literal in a lambda."
  #:literals (with-handlers)
  
  (with-handlers (clause-before ...
                  [pred:expr handler:literal-constant]
                  clause-after ...)
    body:expr ...)
  
  (with-handlers (clause-before ...
                  [pred (λ (_) handler)]
                  clause-after ...)
    body ...))


(define-refactoring-rule error-to-raise-arguments-error
  #:description
  "Use `raise-arguments-error` instead of `error` for better error messages that follow Racket \
conventions."
  #:literals (error)
  
  (error sym:expr message:str arg:id ...+)

  #:do [(define message-str (syntax-e (attribute message)))
        (define args-list (attribute arg))]
  #:when (= (length (regexp-match* #rx"~a" message-str)) (length args-list))
  ;; Check that all ~a occurrences are surrounded by spaces or at string boundaries
  #:when (let ([matches (regexp-match-positions* #rx"~a" message-str)])
           (for/and ([match (in-list matches)])
             (define start (car match))
             (define end (cdr match))
             (define before-ok? (or (= start 0)
                                   (char-whitespace? (string-ref message-str (- start 1)))))
             (define after-ok? (or (= end (string-length message-str))
                                  (char-whitespace? (string-ref message-str end))))
             (and before-ok? after-ok?)))
  #:do [(define cleaned-message (string-replace message-str "~a" ""))
        ;; Clean up extra spaces and trailing punctuation from placeholder removal
        (define cleaned-message-normalized
          (regexp-replace* #rx"  +" cleaned-message " "))]
  #:with new-message (regexp-replace #rx"[,;: ]+$" cleaned-message-normalized "")
  #:with (arg-str ...)
  (for/list ([arg-id (in-list args-list)])
    (symbol->string (syntax-e arg-id)))
  
  (raise-arguments-error sym new-message (~@ arg-str arg) ...))


(define-refactoring-suite exception-suggestions
  #:rules (literal-exception-handler-to-lambda
           error-to-raise-arguments-error))
