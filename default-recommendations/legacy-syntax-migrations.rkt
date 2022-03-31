#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [legacy-syntax-migrations refactoring-suite?]))


(require (for-syntax racket/base)
         rebellion/private/static-name
         resyntax/refactoring-rule
         resyntax/refactoring-suite
         resyntax/syntax-replacement)


;@----------------------------------------------------------------------------------------------------


(define-refactoring-rule datum->syntax-migration
  #:description "The fifth argument to datum->syntax is ignored."
  ;; I have absolutely no idea why, but using (~and id datum->syntax) here it causes syntax-parse to
  ;; report a compile error claiming that it expected an identifier not starting with ~.
  [(id ctxt v srcloc prop ignored)
   #:when (free-identifier=? #'id #'datum->syntax)
   ((ORIGINAL-SPLICE id ctxt v srcloc prop))])


(define-refactoring-rule syntax-recertify-migration
  #:description "The syntax-recertify function is a legacy function that does nothing."
  #:literals (syntax-recertify)
  [(syntax-recertify stx _ _ _)
   stx])


(define-refactoring-rule syntax-disarm-migration
  #:description "The syntax-disarm function is a legacy function that does nothing."
  #:literals (syntax-disarm)
  [(syntax-disarm stx _)
   stx])


(define-refactoring-rule syntax-rearm-migration
  #:description "The syntax-rearm function is a legacy function that does nothing."
  #:literals (syntax-rearm)
  [(syntax-rearm stx _ ...)
   stx])


(define-refactoring-rule syntax-protect-migration
  #:description "The syntax-protect function is a legacy function that does nothing."
  #:literals (syntax-protect)
  [(syntax-protect stx)
   stx])


(define legacy-syntax-migrations
  (refactoring-suite
   #:name (name legacy-syntax-migrations)
   #:rules
   (list datum->syntax-migration
         syntax-disarm-migration
         syntax-protect-migration
         syntax-rearm-migration
         syntax-recertify-migration)))
