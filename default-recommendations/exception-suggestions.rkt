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
  "Use `raise-arguments-error` instead of `error` for better error messages that follow Racket conventions."
  #:literals (error)
  
  (error sym:expr message:str arg:id ...)
  #:when (string-contains? (syntax-e #'message) "~a")
  #:do [(define message-str (syntax-e #'message))
        (define cleaned-message (string-replace message-str "~a" ""))
        ;; Remove trailing punctuation and whitespace that was left after removing ~a
        (define cleaned-message-trimmed (string-trim cleaned-message))
        (define cleaned-final
          (regexp-replace #rx"[,;:\\s]+$" cleaned-message-trimmed ""))
        (define args-list (syntax->list #'(arg ...)))
        (define field-pairs
          (apply append
                 (map (λ (arg-stx)
                        (list (datum->syntax #'message (symbol->string (syntax-e arg-stx)))
                              arg-stx))
                      args-list)))]
  #:with new-message cleaned-final
  #:with (field-pair ...) field-pairs
  
  (raise-arguments-error sym new-message field-pair ...))


(define-refactoring-suite exception-suggestions
  #:rules (literal-exception-handler-to-lambda
           error-to-raise-arguments-error))
