#lang racket/base

(require racket/contract)

(provide
 (contract-out
  [refactoring-rule? predicate/c]
  [standard-refactoring-rules (listof refactoring-rule?)]))

(module+ private
  (provide
   (contract-out
    [refactoring-rule-refactor (-> refactoring-rule? syntax? (option/c syntax?))])))

(require racket/syntax
         rebellion/base/option
         rebellion/type/record
         syntax/parse
         syntax/parse/define)

;@----------------------------------------------------------------------------------------------------

(define-record-type refactoring-rule (transformer)
  #:omit-root-binding)

(define (refactoring-rule-refactor rule syntax)
  ((refactoring-rule-transformer rule) syntax))

(define-simple-macro
  (define-refactoring-rule id:id parse-option ... [pattern pattern-directive ... replacement])
  (define id
    (constructor:refactoring-rule
     #:transformer
     (syntax-parser
       parse-option ...
       [pattern pattern-directive ... (present #'replacement)]
       [_ absent]))))

(define-syntax-class define-struct-id-maybe-super
  #:attributes (id super-id)
  (pattern id:id #:attr super-id #false)
  (pattern (id:id super-id:id)))

(define-refactoring-rule struct-from-define-struct-with-default-constructor-name
  #:literals (define-struct)
  [(define-struct id-maybe-super:define-struct-id-maybe-super fields
     (~and option (~not #:constructor-name) (~not #:extra-constructor-name)) ...)
   #:with make-id (format-id #'id-maybe-super.id "make-~a" #'id-maybe-super.id)
   (struct id-maybe-super.id (~? id-maybe-super.super-id) fields
     #:extra-constructor-name make-id
     option ...)])

(define-refactoring-rule false/c-migration
  #:literals (false/c)
  [false/c
   #false])

(define-refactoring-rule symbols-migration
  #:literals (symbols)
  [(symbols sym ...)
   (or/c sym ...)])

(define-refactoring-rule vector-immutableof-migration
  #:literals (vector-immutableof)
  [(vector-immutableof c)
   (vectorof c #:immutable #true)])

(define-refactoring-rule vector-immutable/c-migration
  #:literals (vector-immutable/c)
  [(vector-immutable/c c ...)
   (vector/c c ... #:immutable #true)])

(define-refactoring-rule box-immutable/c-migration
  #:literals (box-immutable/c)
  [(box-immutable/c c)
   (box/c c #:immutable #true)])

(define-refactoring-rule flat-contract-migration
  #:literals (flat-contract)
  [(flat-contract predicate)
   predicate])

(define-refactoring-rule flat-contract-predicate-migration
  #:literals (flat-contract-predicate)
  [(flat-contract-predicate c)
   c])

(define-refactoring-rule contract-struct-migration
  #:literals (contract-struct)
  [(contract-struct id fields)
   (struct id fields)])

(define-refactoring-rule define-contract-struct-migration
  #:literals (define-contract-struct)
  [(define-contract-struct id fields)
   #:with make-id (format-id #'id "make-~a" #'id)
   (define-struct id fields #:extra-constructor-name make-id)])

(define-syntax-class cond-condition-clause
  #:literals (cond else)
  (pattern [(~and condition (~not else)) body ...+]))

(define-refactoring-rule cond-mandatory-else
  #:literals (cond else)
  [(cond clause:cond-condition-clause ...)
   (cond clause ... [else (void)])])

(define standard-refactoring-rules
  (list struct-from-define-struct-with-default-constructor-name
        false/c-migration
        symbols-migration
        vector-immutableof-migration
        vector-immutable/c-migration
        box-immutable/c-migration
        flat-contract-migration
        flat-contract-predicate-migration
        contract-struct-migration
        define-contract-struct-migration
        cond-mandatory-else))
