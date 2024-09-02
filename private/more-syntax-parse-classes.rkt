#lang racket/base


(provide syntax-parse-option
         syntax-parse-pattern-directive)


(require syntax/parse)


;@----------------------------------------------------------------------------------------------------


(define-splicing-syntax-class syntax-parse-option
  (pattern (~seq #:context ~! context-expr:expr))
  (pattern (~seq #:literals ~! (literal:syntax-parse-literal ...)))
  (pattern (~seq #:datum-literals ~! (datum-literal:syntax-parse-datum-literal ...)))
  (pattern (~seq #:literal-sets ~! (literal-set:syntax-parse-literal-set ...)))
  (pattern #:track-literals)
  (pattern (~seq #:conventions ~! (convention-id:id ...)))
  (pattern (~seq #:local-conventions ~! (convention-id:id ...)))
  (pattern #:disable-colon-notation))


(define-syntax-class syntax-parse-literal
  (pattern literal-id:id)
  (pattern (pattern-id:id literal-id:id))
  (pattern (pattern-id:id literal-id:id #:phase phase-expr:expr)))


(define-syntax-class syntax-parse-datum-literal
  (pattern literal-id:id)
  (pattern (pattern-id:id literal-id:id)))


(define-syntax-class syntax-parse-literal-set
  (pattern literal-set-id:id)
  (pattern (literal-set-id:id literal-set-option:syntax-parse-literal-set-option ...)))


(define-splicing-syntax-class syntax-parse-literal-set-option
  (pattern (~seq #:at context-id:id))
  (pattern (~seq #:phase phase-expr:expr)))


(define-splicing-syntax-class syntax-parse-pattern-directive
  (pattern (~seq #:declare ~! pvar-id:id
                 (~or syntax-class-id:id (syntax-class-id:id arg ...))
                 (~optional (~seq #:role role-expr:expr))))
  (pattern (~seq #:post ~! action-pattern))
  (pattern (~seq #:and ~! action-pattern))
  (pattern (~seq #:with ~! syntax-pattern stx-expr:expr))
  (pattern (~seq #:attr ~! (~or attr-name-id:id (attr-name-id:id depth)) expr:expr))
  (pattern (~seq #:fail-when ~! condition-expr:expr message-expr:expr))
  (pattern (~seq #:fail-unless ~! condition-expr:expr message-expr:expr))
  (pattern (~seq #:when ~! condition-expr:expr))
  (pattern (~seq #:do ~! [defn-or-expr ...]))
  (pattern (~seq #:undo ~! [defn-or-expr ...]))
  (pattern #:cut))
