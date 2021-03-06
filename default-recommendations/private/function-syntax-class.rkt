#lang racket/base


(require rebellion/type/record
         resyntax/default-recommendations/private/lambda-by-any-name
         syntax/parse)


;@----------------------------------------------------------------------------------------------------


(define-record-type function-arity
  (minimum-positional-arguments
   maximum-positional-arguments
   required-keywords
   optional-keywords))


(define-record-type parsed-function-parameter (name keyword default))


(define (required-positional-parameter? v)
  (and (parsed-function-parameter? v)
       (not (parsed-function-parameter-keyword v))
       (not (parsed-function-parameter-default v))))


(define (optional-positional-parameter? v)
  (and (parsed-function-parameter? v)
       (not (parsed-function-parameter-keyword v))
       (parsed-function-parameter-default v)))


(define (required-keyword-parameter? v)
  (and (parsed-function-parameter? v)
       (parsed-function-parameter-keyword v)
       (not (parsed-function-parameter-default v))))


(define (optional-keyword-parameter? v)
  (and (parsed-function-parameter? v)
       (parsed-function-parameter-keyword v)
       (parsed-function-parameter-default v)))


(define-splicing-syntax-class function-parameter
  #:attributes (name keyword default parsed)

  (pattern name:id
    #:attr keyword #false
    #:attr default #false
    #:attr parsed (parsed-function-parameter #:name #'name #:keyword #false #:default #false))

  (pattern [name:id default:expr]
    #:attr keyword #false
    #:attr parsed (parsed-function-parameter #:name #'name #:keyword #false #:default #'default))

  (pattern (~seq keyword:keyword name:id)
    #:attr default #false
    #:attr parsed
    (parsed-function-parameter #:name #'name #:keyword (syntax-e #'keyword) #:default #false))

  (pattern (~seq keyword:keyword [name:id default:expr])
    #:attr parsed
    (parsed-function-parameter #:name #'name #:keyword (syntax-e #'keyword) #:default #'default)))


(define-record-type parsed-lambda-header
  (required-positional-parameters
   optional-positional-parameters
   required-keyword-parameters
   optional-keyword-parameters
   rest-parameter))


(define (build-parsed-lambda-header parameters rest-parameter)
  (parsed-lambda-header
   #:required-positional-parameters (filter required-positional-parameter? parameters)
   #:optional-positional-parameters (filter optional-positional-parameter? parameters)
   #:required-keyword-parameters (filter required-keyword-parameter? parameters)
   #:optional-keyword-parameters (filter optional-keyword-parameter? parameters)
   #:rest-parameter rest-parameter))


(define (parsed-lambda-header-arity header)
  (define minimum-positional-arguments
    (length (parsed-lambda-header-required-positional-parameters header)))
  (define maximum-positional-arguments
    (if (parsed-lambda-header-rest-parameter header)
        +inf.0
        (+ minimum-positional-arguments
           (length (parsed-lambda-header-optional-positional-parameters header)))))
  (function-arity
   #:minimum-positional-arguments minimum-positional-arguments
   #:maximum-positional-arguments maximum-positional-arguments
   #:required-keywords
   (map parsed-function-parameter-keyword (parsed-lambda-header-required-keyword-parameters header))
   #:optional-keywords
   (map parsed-function-parameter-keyword (parsed-lambda-header-optional-keyword-parameters header))))


(define-syntax-class lambda-header
  #:attributes (arity [parameter-name 1] [non-rest-parameter 1] rest-parameter parsed)

  (pattern rest-parameter:id
    #:attr parsed (build-parsed-lambda-header '() #'rest-parameter)
    #:with arity (parsed-lambda-header-arity (attribute parsed))
    #:with (parameter-name ...) (list #'rest-parameter)
    #:with (non-rest-parameter ...) '())

  (pattern (non-rest-parameter:function-parameter ...)
    #:attr parsed (build-parsed-lambda-header (attribute non-rest-parameter) #false)
    #:with arity (parsed-lambda-header-arity (attribute parsed))
    #:attr rest-parameter #false
    #:with (parameter-name ...) #'(non-rest-parameter.name ...))

  (pattern (non-rest-parameter:function-parameter ... . rest-parameter:id)
    #:attr parsed (build-parsed-lambda-header (attribute non-rest-parameter) #'rest-parameter)
    #:with arity (parsed-lambda-header-arity (attribute parsed))
    #:with (parameter-name ...) #'(non-rest-parameter.name ... rest-parameter)))


(define-record-type parsed-lambda-expression (header body-forms))


(define-syntax-class lambda-expression
  #:attributes (header [body 1] parsed)

  (pattern (_:lambda-by-any-name header:lambda-header body:expr ...+)
    #:attr parsed (parsed-lambda-expression #:header #'header #:body-forms (attribute body))))


(define-record-type parsed-nested-lambda-expression (headers body-form-lists))


(define-syntax-class nested-lambda-expression
  #:attributes ([header 1] [innermost-body 1] parsed)

  (pattern (_:lambda-by-any-name first-header:lambda-header nested:nested-lambda-expression)
    #:cut
    #:with (header ...) (cons #'first-header (attribute nested.header))
    #:with (innermost-body ...) (attribute nested.innermost-body)
    #:attr parsed
    (parsed-nested-lambda-expression
     #:headers (attribute header)
     #:body-form-lists
     (cons
      (list #'nested) (parsed-nested-lambda-expression-body-form-lists (attribute nested.parsed)))))

  (pattern
      (_:lambda-by-any-name
       first-header:lambda-header
       (~and first-body (~not :nested-lambda-expression))
       remaining-body ...)
    #:cut
    #:with (header ...) (list #'first-header)
    #:with (innermost-body ...) #'(first-body remaining-body ...)
    #:attr parsed
    (parsed-nested-lambda-expression
     #:headers (attribute header) #:body-form-lists (list (attribute innermost-body)))))


(define (simplify-function-definition header expr)
  (