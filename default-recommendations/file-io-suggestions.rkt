#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [file-io-suggestions refactoring-suite?]))


(require racket/file
         racket/set
         rebellion/private/static-name
         resyntax/base
         resyntax/default-recommendations/private/lambda-by-any-name
         resyntax/default-recommendations/private/syntax-identifier-sets
         syntax/parse)


;@----------------------------------------------------------------------------------------------------


(define-refactoring-rule call-with-output-file-to-display-to-file
  #:description
  "This use of `call-with-output-file` can be replaced with a simpler call to `display-to-file`."
  #:literals (call-with-output-file display)
  (call-with-output-file path:expr
    (_:lambda-by-any-name (out:id) (display v:expr out-use:id)))
  #:when (free-identifier=? (attribute out-use) (attribute out))
  #:when (not (set-member? (syntax-free-identifiers (attribute v)) (attribute out)))
  (display-to-file v path))


(define-refactoring-rule call-with-output-file-to-display-lines-to-file
  #:description
  "This use of `call-with-output-file` can be replaced with a simpler call to\
 `display-lines-to-file`."
  #:literals (call-with-output-file displayln)
  (call-with-output-file path:expr
    (_:lambda-by-any-name (out:id) (displayln v:expr out-use:id) ...+))
  #:when (for/and ([out-use-id (in-list (attribute out-use))])
           (free-identifier=? out-use-id (attribute out)))
  #:when (not (set-member? (syntax-free-identifiers #'(v ...)) (attribute out)))
  (display-lines-to-file (list v ...) path))


(define-refactoring-suite file-io-suggestions
  #:rules (call-with-output-file-to-display-lines-to-file
           call-with-output-file-to-display-to-file))
