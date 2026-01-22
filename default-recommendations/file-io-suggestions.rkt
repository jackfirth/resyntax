#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [file-io-suggestions refactoring-suite?]))


(require racket/file
         rebellion/private/static-name
         resyntax/base
         resyntax/default-recommendations/private/lambda-by-any-name
         syntax/parse)


;@----------------------------------------------------------------------------------------------------


(define-refactoring-rule manual-display-to-file
  #:description
  "This `call-with-output-file` expression can be replaced with `display-to-file`."
  #:literals (call-with-output-file display display-to-file)
  (call-with-output-file path:expr
    (_:lambda-by-any-name (out:id)
      (display content:expr out-ref:id)))
  #:when (free-identifier=? #'out #'out-ref)
  (display-to-file content path))


(define-refactoring-rule manual-displayln-to-file
  #:description
  "This `call-with-output-file` expression can be replaced with `display-lines-to-file`."
  #:literals (call-with-output-file displayln display-lines-to-file list)
  (call-with-output-file path:expr
    (_:lambda-by-any-name (out:id)
      (displayln content:expr out-ref:id)))
  #:when (free-identifier=? #'out #'out-ref)
  (display-lines-to-file (list content) path))


(define-refactoring-suite file-io-suggestions
  #:rules (manual-display-to-file
           manual-displayln-to-file))
