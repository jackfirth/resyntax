#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [syntax-shortcuts refactoring-suite?]))


(require racket/string
         racket/syntax
         racket/list
         rebellion/private/static-name
         resyntax/base
         syntax/parse)


;@----------------------------------------------------------------------------------------------------


(define-syntax-class format-id-argument
  #:attributes (uses-syntax-e? simplified)
  #:literals (syntax-e)

  (pattern (syntax-e simplified:expr)
    #:with uses-syntax-e? #'#true)

  (pattern simplified:expr
    #:with uses-syntax-e? #'#false))


(define-refactoring-rule syntax-e-in-format-id-unnecessary
  #:description
  "Using `syntax-e` on the arguments of `format-id` is unnecessary, `format-id` already unwrap
 syntax object arguments."
  #:literals (format-id syntax-e)

  (format-id lctx:expr fmt:expr arg:format-id-argument ...+)
  #:when
  (for/or ([uses-syntax-e? (attribute arg.uses-syntax-e?)])
    (syntax-e uses-syntax-e?))

  (format-id lctx fmt arg.simplified ...))


(define-syntax-class format-symbol-argument
  #:attributes (simplified)
  #:literals (syntax-e keyword->string symbol->string)

  (pattern (syntax-e inner:format-symbol-argument) #:attr simplified (attribute inner.simplified))

  (pattern (keyword->string inner:format-symbol-argument)
    #:attr simplified (attribute inner.simplified))

  (pattern (symbol->string inner:format-symbol-argument)
    #:attr simplified (attribute inner.simplified))

  (pattern simplified:expr))


;; The format-symbol function only allows ~a placeholders. Rather a fancy generic utilty that finds
;; all placeholders, we just explicitly list out all the other ones and check one-by-one whether any
;; of them are contained in the template string. That's easier to implement and the performance
;; doesn't matter at all since template strings are almost always short.
(define disallowed-format-symbol-placeholders
  (list "~n"
        "~%"
        "~s"
        "~S"
        "~v"
        "~V"
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
        "~ "
        "~\n"
        "~\t"))


(define-refactoring-rule format-string-to-format-symbol
  #:description
  "This `format` expression can be simplified to an equivalent `format-symbol` expression."
  #:literals (format string->symbol)

  (string->symbol (format template:str arg:format-symbol-argument ...))
  #:when (for/and ([disallowed (in-list disallowed-format-symbol-placeholders)])
           (not (string-contains? (syntax-e #'template) disallowed)))

  (format-symbol template (~replacement arg.simplified #:original arg) ...))


(define-refactoring-rule flatten-apply-append-syntax-template
  #:description
  "Flattening nested syntax templates with `apply append` and `map syntax->list` can be simplified\
 by using a single `syntax->list` call on a flattened template."
  #:literals (apply append map syntax->list syntax ...)

  (apply append (map syntax->list (syntax->list (syntax ((inner ...) ...)))))

  #:with flattened-template
  (let* ([inner-attrs (attribute inner)]
         ; Wrap in list if it's not already a list
         [inner-list (if (list? inner-attrs) inner-attrs (list inner-attrs))]
         [ellipsis-sym (datum->syntax #'here '...)])
    (datum->syntax #'here (append inner-list (list ellipsis-sym ellipsis-sym))))
  
  (syntax->list #'flattened-template))


(define-refactoring-suite syntax-shortcuts
  #:rules (flatten-apply-append-syntax-template
           format-string-to-format-symbol
           syntax-e-in-format-id-unnecessary))
