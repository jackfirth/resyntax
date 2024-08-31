#lang racket/base


(provide branching-form-allowing-internal-definitions-within-clauses
         header-form-allowing-internal-definitions)


(require (for-syntax racket/base)
         racket/block
         (only-in racket/class
                  define/augment
                  define/augment-final
                  define/augride
                  define/overment
                  define/override
                  define/override-final
                  define/public
                  define/public-final
                  define/pubment
                  define/private)
         racket/match
         rebellion/private/static-name
         resyntax/default-recommendations/private/lambda-by-any-name
         resyntax/private/syntax-replacement
         syntax/parse
         syntax/parse/lib/function-header)


;@----------------------------------------------------------------------------------------------------


(define-splicing-syntax-class header-form-allowing-internal-definitions
  #:attributes ([original 1])
  #:literals (block
              #%module-begin
              module
              module*
              module+
              let
              let*
              let-values
              let*-values
              when
              unless
              with-handlers
              parameterize
              for
              for/list
              for/vector
              for/hash
              for/hasheq
              for/hasheqv
              for/and
              for/or
              for/sum
              for/product
              for/first
              for/last
              for*
              for*/list
              for*/vector
              for*/hash
              for*/hasheq
              for*/hasheqv
              for*/and
              for*/or
              for*/sum
              for*/product
              for*/first
              for*/last)

  (pattern (~seq id:header-id-with-no-header-forms)
    #:with (original ...) #'(id))

  (pattern (~seq id:header-id-with-one-header-form ~! header-form)
    #:with (original ...) #'(id header-form))

  (pattern (~seq id:header-id-with-two-header-forms ~! first-header-form second-header-form)
    #:with (original ...) #'(id first-header-form second-header-form))

  ;; define forms have to be handled specially to ensure they're function definitions and not variable
  ;; definitions, since only function definitions allow multi-form bodies.
  (pattern (~seq id:define-by-any-name ~! header-form:function-header)
    #:with (original ...) #'(id header-form))

  ;; let forms have to be handled specially because of named lets
  (pattern (~seq id:let ~! (~optional name:id) header)
    #:with (original ...) #'(id (~? name) header))

  ;; for/vector and for*/vector have keyword options
  (pattern
    (~seq
     (~or for-id:for/vector for-id:for*/vector)
     ~!
     (~and (~seq keyword-option ...)
           (~seq (~alt (~optional (~seq #:length length-expr))
                       (~optional (~seq #:fill fill-expr))) ...))
     clauses)
    #:with (original ...) #'(for-id keyword-option ... clauses)))


(define-syntax-class header-id-with-no-header-forms
  #:literals (block #%module-begin)
  (pattern (~or block #%module-begin)))


(define-syntax-class header-id-with-one-header-form
  #:literals (module+
                 let*
               let-values
               let*-values
               when
               unless
               with-handlers
               parameterize
               for
               for/list
               for/vector
               for/hash
               for/hasheq
               for/hasheqv
               for/and
               for/or
               for/sum
               for/product
               for/first
               for/last
               for*
               for*/list
               for*/vector
               for*/hash
               for*/hasheq
               for*/hasheqv
               for*/and
               for*/or
               for*/sum
               for*/product
               for*/first
               for*/last)
  (pattern (~or :lambda-by-any-name
                module+
                let*
                let-values
                let*-values
                when
                unless
                with-handlers
                parameterize
                for
                for/list
                for/vector
                for/hash
                for/hasheq
                for/hasheqv
                for/and
                for/or
                for/sum
                for/product
                for/first
                for/last
                for*
                for*/list
                for*/vector
                for*/hash
                for*/hasheq
                for*/hasheqv
                for*/and
                for*/or
                for*/sum
                for*/product
                for*/first
                for*/last)))


(define-syntax-class header-id-with-two-header-forms
  #:literals (module module* for/fold for*/fold)
  (pattern (~or module module* for/fold for*/fold)))


(define-splicing-syntax-class branching-form-allowing-internal-definitions-within-clauses
  #:literals (cond case-lambda match)
  #:attributes ([original 1])

  (pattern (~seq cond-id:cond ~!)
    #:with (original ...) #'(cond-id))

  (pattern (~seq case-lambda-id:case-lambda)
    #:with (original ...) #'(case-lambda-id))

  (pattern (~seq match-id:match ~! subject:expr)
    #:with (original ...) #'(match-id subject)))


;; There's a lot of variants of define that support the same grammar but have different meanings. We
;; can recognize and refactor all of them with this syntax class.
(define-syntax-class define-by-any-name
  #:literals (define
               define/augment
               define/augment-final
               define/augride
               define/overment
               define/override
               define/override-final
               define/public
               define/public-final
               define/pubment
               define/private)
  (pattern
    (~or define
         define/augment
         define/augment-final
         define/augride
         define/overment
         define/override
         define/override-final
         define/public
         define/public-final
         define/pubment
         define/private)))