#lang resyntax/test


require: resyntax/default-recommendations syntax-parse-shortcuts


header:
------------------------------
#lang racket/base
(require syntax/parse/define)
------------------------------


no-change-test:
"define-syntax-parse-rule not refactorable (https://github.com/jackfirth/resyntax/issues/106)"
------------------------------
(define-syntax-parse-rule (my-or a:expr b:expr)
  ;; The let form is needed to avoid evaluating a twice.
  (let ([tmp a]) (if a a b)))
------------------------------
