#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [string-shortcuts refactoring-suite?]))


(require racket/string
         rebellion/private/static-name
         resyntax/refactoring-rule
         resyntax/refactoring-suite
         resyntax/private/syntax-replacement
         syntax/parse
         syntax/parse/define)


;@----------------------------------------------------------------------------------------------------


(define-refactoring-rule display-newline-to-newline
  #:description "The `newline` function can be used to print a single newline character."
  #:literals (display displayln)
  [(~or (display "\n") (displayln "")) (newline)])


(define-syntax-class keywordless-string-join-call
  #:attributes (original-splice)
  #:literals (string-join)

  (pattern ((~and id string-join) strs)
    #:with original-splice #'(ORIGINAL-SPLICE id strs))

  (pattern ((~and id string-join) strs sep)
    #:with original-splice #'(ORIGINAL-SPLICE id sep)))


(define-syntax-class string-append-and-string-join-expression
  #:attributes (refactored)

  (pattern (string-append before join-call:keywordless-string-join-call)
    #:with refactored
    #'(join-call.original-splice (ORIGINAL-GAP before join-call) #:before-first before))

  (pattern (string-append join-call:keywordless-string-join-call after)
    #:with refactored
    #'(join-call.original-splice (ORIGINAL-GAP join-call after) #:after-last after))

  (pattern (string-append before join-call:keywordless-string-join-call after)
    #:with refactored
    #'(join-call.original-splice
       (ORIGINAL-GAP before join-call) #:before-first before
       (ORIGINAL-GAP join-call after) #:after-last after)))


(define-refactoring-rule sring-append-and-string-join-to-string-join
  #:description
  "This use of `string-append` can be removed by using `string-join`'s keyword arguments."
  #:literals (string-append string-join)
  [expr:string-append-and-string-join-expression expr.refactored])


(define string-shortcuts
  (refactoring-suite
   #:name (name string-shortcuts)
   #:rules
   (list display-newline-to-newline
         sring-append-and-string-join-to-string-join)))
