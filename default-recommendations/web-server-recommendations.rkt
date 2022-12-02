#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [web-server-recommendations refactoring-suite?]))


(require rebellion/private/static-name
         resyntax/refactoring-rule
         resyntax/refactoring-suite
         resyntax/private/syntax-replacement
         syntax/parse
         syntax/parse/define
         web-server/http/request-structs)


;@----------------------------------------------------------------------------------------------------


(define-refactoring-rule headers-assq-to-headers-assq*
  #:description "The `headers-assq` function is case-sensitive, which violates HTTP specifications.\
 Use `headers-assq*` instead."
  #:literals (headers-assq)
  [headers-assq headers-assq*])


(define web-server-recommendations
  (refactoring-suite
   #:name (name web-server-recommendations)
   #:rules
   (list headers-assq-to-headers-assq*)))
