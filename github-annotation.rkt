#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [github-annotation? predicate/c]
  [github-annotation
   (->* (#:path path-string?
         #:message string?
         #:annotation-level github-annotation-level?
         #:start-line natural?
         #:end-line natural?)
        (#:start-column (or/c natural? #f)
         #:end-column (or/c natural? #f)
         #:title string?
         #:raw-details string?)
        github-annotation?)]
  [github-annotation-path (-> github-annotation? path?)]
  [github-annotation-message (-> github-annotation? immutable-string?)]
  [github-annotation-annotation-level (-> github-annotation? github-annotation-level?)]
  [github-annotation-start-line (-> github-annotation? natural?)]
  [github-annotation-end-line (-> github-annotation? natural?)]
  [github-annotation-start-column (-> github-annotation? (or/c natural? #false))]
  [github-annotation-end-column (-> github-annotation? (or/c natural? #false))]
  [github-annotation-title (-> github-annotation? immutable-string?)]
  [github-annotation-raw-details (-> github-annotation? immutable-string?)]
  [github-annotation->json (-> github-annotation? immutable-string?)]
  [github-annotation->jsexpr (-> github-annotation? jsexpr?)]
  [json->github-annotation (-> string? github-annotation?)]
  [jsexpr->github-annotation (-> jsexpr? github-annotation?)]
  [github-annotation-level? predicate/c]
  [notice github-annotation-level?]
  [warning github-annotation-level?]
  [failure github-annotation-level?]))


(require json
         racket/list
         racket/match
         racket/math
         racket/sequence
         racket/string
         rebellion/base/immutable-string
         rebellion/base/option
         rebellion/type/enum
         rebellion/type/record)


(module+ test
  (require (submod "..")
           rackunit
           rebellion/private/static-name))


;@----------------------------------------------------------------------------------------------------


(define-enum-type github-annotation-level (notice warning failure))


(define-record-type github-annotation
  (path
   start-line
   end-line
   start-column
   end-column
   annotation-level
   message
   title
   raw-details)
  #:omit-root-binding)


(define (github-annotation
         #:path path
         #:message message
         #:annotation-level level
         #:start-line start-line
         #:end-line end-line
         #:start-column [start-column #false]
         #:end-column [end-column #false]
         #:title [title ""]
         #:raw-details [raw-details ""])
  (constructor:github-annotation
   #:path (cleanse-path (if (string? path) (string->path path) path))
   #:start-line start-line
   #:end-line end-line
   #:start-column start-column
   #:end-column end-column
   #:annotation-level level
   #:message (string->immutable-string message)
   #:title (string->immutable-string title)
   #:raw-details (string->immutable-string raw-details)))
   

(define (optional-hash . key-opt-value-pairs)
  (for*/hash ([k+opt (in-slice 2 (in-list key-opt-value-pairs))]
              [v (in-option (second k+opt))])
    (values (first k+opt) v)))


(module+ test
  (test-case (name-string optional-hash)
    (check-equal? (optional-hash) (hash))
    (check-equal? (optional-hash 'a (present 1)) (hash 'a 1))
    (check-equal? (optional-hash 'a absent) (hash))))


(define (github-annotation->jsexpr annotation)
  (define level
    (match (github-annotation-annotation-level annotation)
      [(== notice) "notice"]
      [(== warning) "warning"]
      [(== failure) "failure"]))
  (optional-hash
     'path (present (path->string (github-annotation-path annotation)))
     'start_line (present (github-annotation-start-line annotation))
     'end_line (present (github-annotation-end-line annotation))
     'start_column (falsey->option (github-annotation-start-column annotation))
     'end_column (falsey->option (github-annotation-end-column annotation))
     'annotation_level (present level)
     'message (present (github-annotation-message annotation))
     'title (nonempty-string-option (github-annotation-title annotation))
     'raw_details (nonempty-string-option (github-annotation-raw-details annotation))))


(define (github-annotation->json annotation)
  (string->immutable-string (jsexpr->string (github-annotation->jsexpr annotation))))


(define (jsexpr->github-annotation annotation-jsexpr)
  (define level
    (match (hash-ref annotation-jsexpr 'annotation_level)
      ["notice" notice]
      ["warning" warning]
      ["failure" failure]))
  (github-annotation
   #:path (hash-ref annotation-jsexpr 'path)
   #:start-line (hash-ref annotation-jsexpr 'start_line)
   #:end-line (hash-ref annotation-jsexpr 'end_line)
   #:start-column (hash-ref annotation-jsexpr 'start_column #false)
   #:end-column (hash-ref annotation-jsexpr 'end_column #false)
   #:annotation-level level
   #:message (hash-ref annotation-jsexpr 'message)
   #:title (hash-ref annotation-jsexpr 'title "")
   #:raw-details (hash-ref annotation-jsexpr 'raw_details "")))


(define (json->github-annotation annotation-json)
  (jsexpr->github-annotation (string->jsexpr annotation-json)))


(define (nonempty-string-option str)
  (if (non-empty-string? str) (present str) absent))
