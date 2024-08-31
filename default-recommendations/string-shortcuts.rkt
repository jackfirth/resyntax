#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [string-shortcuts refactoring-suite?]))


(require racket/list
         racket/set
         racket/string
         rebellion/private/static-name
         resyntax/refactoring-rule
         resyntax/refactoring-suite
         resyntax/private/syntax-replacement
         syntax/parse)


;@----------------------------------------------------------------------------------------------------


(define-refactoring-rule display-newline-to-newline
  #:description "The `newline` function can be used to print a single newline character."
  #:literals (display displayln)
  [(~or (display "\n") (displayln "")) (newline)])


(define-syntax-class keywordless-string-join-call
  #:attributes ([original 1])
  #:literals (string-join)

  (pattern ((~and id string-join) strs)
    #:with (original ...) #'(id strs))

  (pattern ((~and id string-join) strs sep)
    #:with (original ...) #'(id sep)))


(define-syntax-class string-append-and-string-join-expression
  #:attributes (refactored)
  #:literals (string-append)

  (pattern (string-append before join-call:keywordless-string-join-call)
    #:with refactored
    #'(join-call.original ... (ORIGINAL-GAP before join-call) #:before-first before))

  (pattern (string-append join-call:keywordless-string-join-call after)
    #:with refactored
    #'(join-call.original ... (ORIGINAL-GAP join-call after) #:after-last after))

  (pattern (string-append before join-call:keywordless-string-join-call after)
    #:with refactored
    #'(join-call.original ...
       (ORIGINAL-GAP before join-call) #:before-first before
       (ORIGINAL-GAP join-call after) #:after-last after)))


(define-refactoring-rule string-append-and-string-join-to-string-join
  #:description
  "This use of `string-append` can be removed by using `string-join`'s keyword arguments."
  #:literals (string-join)
  [expr:string-append-and-string-join-expression expr.refactored])


(define-refactoring-rule string-append-identity
  #:description "This use of `string-append` does nothing."
  #:literals (string-append)
  [(string-append s) s])


(define-refactoring-rule manual-string-join
  #:description "This use of `string-append` and `add-between` is equivalent to `string-join`."
  #:literals (apply string-append add-between)
  [(apply string-append (add-between strings separator))
   (string-join strings separator)])


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
  [(format s:str)
   #:when (not (string-contains-formatting-directives? (syntax-e #'s)))
   s])


(define string-shortcuts
  (refactoring-suite
   #:name (name string-shortcuts)
   #:rules
   (list display-newline-to-newline
         format-identity
         manual-string-join
         string-append-and-string-join-to-string-join
         string-append-identity)))
