#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [file-io-suggestions refactoring-suite?]))


(require racket/file
         racket/list
         rebellion/private/static-name
         resyntax/refactoring-rule
         resyntax/refactoring-suite
         syntax/parse)


;@----------------------------------------------------------------------------------------------------


(define-splicing-syntax-class function-call-argument
  #:attributes (keyword expr)
  (pattern (~seq expr:expr) #:attr keyword #false)
  (pattern (~seq keyword:keyword expr:expr)))


(define-splicing-syntax-class function-call-arguments
  #:attributes ([positional 1] keyword)
  (pattern (~seq arg:function-call-argument ...)
    #:cut
    #:with (positional ...)
    (for/list ([expr (in-list (attribute arg.expr))]
               [keyword (in-list (attribute arg.keyword))]
               #:unless keyword)
      expr)

    #:attr keyword
    (for/hasheq ([expr (in-list (attribute arg.expr))]
                 [keyword (in-list (attribute arg.keyword))]
                 #:when keyword)
      (values (syntax-e keyword) expr))))


(define-refactoring-rule make-temporary-directory-migration
  #:description "Use `make-temporary-directory` to make directories instead of `make-temporary-file`."
  #:literals (make-temporary-file)
  #:datum-literals (quote directory)

  (make-temporary-file args:function-call-arguments)
  #:with 'directory
  (or (and (>= (length (attribute args.positional)) 2)
           (second (attribute args.positional)))
      (hash-ref (attribute args.keyword) '#:copy-from #false))
  #:cut
  #:attr template-arg
  (and (not (empty? (attribute args.positional)))
       (first (attribute args.positional)))
  #:attr base-dir-arg
  (or (and (>= (length (attribute args.positional)) 3)
           (third (attribute args.positional)))
      (hash-ref (attribute args.keyword) '#:base-dir #false))

  (make-temporary-directory (~? template-arg) (~? (~@ #:base-dir base-dir-arg))))


(define-refactoring-suite file-io-suggestions
  #:rules (make-temporary-directory-migration))
