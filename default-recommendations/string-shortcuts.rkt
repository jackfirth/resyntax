#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [string-shortcuts refactoring-suite?]))


(require racket/list
         racket/port
         racket/set
         racket/string
         rebellion/private/static-name
         resyntax/base
         resyntax/private/syntax-neighbors
         syntax/parse)


;@----------------------------------------------------------------------------------------------------


(define-refactoring-rule display-newline-to-newline
  #:description "The `newline` function can be used to print a single newline character."
  #:literals (display displayln)
  (~or (display "\n") (displayln ""))
  (newline))


(define-syntax-class newline-by-any-name
  #:literals (display displayln newline)
  (pattern (~or (display "\n") (displayln "") (newline))))


(define-splicing-syntax-class body-forms-not-starting-with-newline
  (pattern (~seq))
  (pattern (~seq (~not :newline-by-any-name) body ...)))


(define-definition-context-refactoring-rule display-and-newline-to-displayln
  #:description "The `displayln` function can be used to display a value with a newline after it."
  #:literals (display)
  (~seq before ...
        (~and display-form (display v))
        newline-after:newline-by-any-name
        after:body-forms-not-starting-with-newline)
  (before ...
   (~replacement (displayln v) #:original-splice (display-form newline-after))
   (~@ . after)))


(define-splicing-syntax-class keywordless-string-join-call
  #:literals (string-join)
  (pattern (~seq string-join strs (~optional sep))))


(define-syntax-class string-append-and-string-join-expression
  #:attributes (refactored)
  #:literals (string-append)

  (pattern (string-append before (join-call:keywordless-string-join-call))
    #:with refactored #'((~@ . join-call) #:before-first before))

  (pattern (string-append (join-call:keywordless-string-join-call) after)
    #:with refactored #'((~@ . join-call) #:after-last after))

  (pattern (string-append before (join-call:keywordless-string-join-call) after)
    #:with refactored #'((~@ . join-call) #:before-first before #:after-last after)))


(define-refactoring-rule string-append-and-string-join-to-string-join
  #:description
  "This use of `string-append` can be removed by using `string-join`'s keyword arguments."
  #:literals (string-join)
  expr:string-append-and-string-join-expression
  expr.refactored)


(define-refactoring-rule string-append-identity
  #:description "This use of `string-append` does nothing."
  #:literals (string-append)
  (string-append s)
  s)


(define-refactoring-rule manual-string-join
  #:description "This use of `string-append` and `add-between` is equivalent to `string-join`."
  #:literals (apply string-append add-between)
  (apply string-append (add-between strings separator))
  (string-join strings separator))


(define formatting-directives
  (set "~n"
       "~%"
       "~a"
       "~A"
       "~s"
       "~S"
       "~.a"
       "~.A"
       "~.s"
       "~.S"
       "~.v"
       "~.V"
       "~e"
       "~E"
       "~c"
       "~C"
       "~b"
       "~B"
       "~o"
       "~O"
       "~x"
       "~X"
       "~~"
       "~ "
       "~\n"
       "~\t"))


(define (string-contains-formatting-directives? s)
  (for/or ([directive (in-set formatting-directives)])
    (string-contains? s directive)))


(define-refactoring-rule format-identity
  #:description "This use of `format` does nothing."
  #:literals (format)
  (format s:str)
  #:when (not (string-contains-formatting-directives? (syntax-e #'s)))
  s)


(define-definition-context-refactoring-rule manual-with-output-to-string
  #:description "This code is equivalent to a use of `with-output-to-string`."
  #:literals (define open-output-string parameterize current-output-port get-output-string)
  (~seq body-before ...
        (define out:id (open-output-string))
        (~or (~seq (parameterize ([current-output-port out2:id])
                     body-inside ...
                     (get-output-string out3:id)))
             (~seq (parameterize ([current-output-port out2:id])
                     body-inside ...)
                   (get-output-string out3:id))))
  #:when (free-identifier=? (attribute out) (attribute out2))
  #:when (free-identifier=? (attribute out) (attribute out3))
  (body-before ...
   (with-output-to-string
     (λ () body-inside ...))))


;; Helper function to create a template string and argument list
(define (merge-string-append-with-format before-list format-template format-args after-list)
  (define (expr->template-part expr)
    (cond
      [(and (syntax? expr) (string? (syntax-e expr))) (syntax-e expr)]
      [else "~a"]))
  
  (define (expr-is-string-literal? expr)
    (and (syntax? expr) (string? (syntax-e expr))))
  
  (define new-template
    (string-append
     (apply string-append (map expr->template-part before-list))
     format-template
     (apply string-append (map expr->template-part after-list))))
  
  (define new-args
    (append
     (filter (lambda (expr) (not (expr-is-string-literal? expr))) before-list)
     format-args
     (filter (lambda (expr) (not (expr-is-string-literal? expr))) after-list)))
  
  (values new-template new-args))


(define-syntax-class string-append-with-format-expression
  #:attributes (refactored)
  #:literals (string-append format)
  
  ;; Simple case: only format call
  (pattern (string-append (format template:str format-arg ...))
    #:with refactored #'(format template format-arg ...))
  
  ;; Case: string literals around format call  
  (pattern (string-append before ... (format template:str format-arg ...) after ...)
    #:do [(define-values (new-template new-args)
            (merge-string-append-with-format 
             (syntax->list #'(before ...))
             (syntax-e #'template)
             (syntax->list #'(format-arg ...))
             (syntax->list #'(after ...))))]
    #:with new-template-literal new-template
    #:with (new-arg ...) new-args
    #:with refactored #'(format new-template-literal new-arg ...)))


(define-refactoring-rule string-append-with-format-to-format
  #:description "This `string-append` with `format` expression can be simplified to a single `format` call."
  #:literals (string-append format)
  expr:string-append-with-format-expression
  expr.refactored)


(define-refactoring-suite string-shortcuts
  #:rules (display-and-newline-to-displayln
           display-newline-to-newline
           format-identity
           manual-string-join
           manual-with-output-to-string
           string-append-and-string-join-to-string-join
           string-append-identity
           string-append-with-format-to-format))
