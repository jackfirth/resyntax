#lang racket/base

(require racket/contract/base)

(provide (contract-out [github-review-request? (-> any/c boolean?)]
                       [github-review-request-jsexpr (-> github-review-request? jsexpr?)]
                       [refactoring-results->github-review
                        (-> (sequence/c refactoring-result?)
                            #:file-count exact-nonnegative-integer?
                            github-review-request?)]))

(require json
         racket/list
         racket/match
         racket/pretty
         racket/sequence
         racket/string
         rebellion/collection/list
         rebellion/streaming/transducer
         rebellion/type/record
         resyntax/private/line-replacement
         resyntax/private/refactoring-result
         resyntax/private/run-command
         resyntax/private/source
         resyntax/private/string-indent
         resyntax/private/syntax-replacement)

;@----------------------------------------------------------------------------------------------------

(define-record-type github-review-request (owner-repo pull-number body event comments))

(define (github-review-request-jsexpr req)
  (match-define (github-review-request #:owner-repo owner-repo
                                       #:pull-number pull-number
                                       #:body body
                                       #:event event
                                       #:comments comments)
    req)
  (match-define (list owner repo) (string-split owner-repo "/"))
  (hash 'owner
        owner
        'repo
        repo
        'body
        body
        'event
        event
        'comments
        (map github-review-comment-jsexpr comments)
        'pull_number
        pull-number))

(define-record-type github-review-comment (path body start-line end-line start-side end-side))

(define (github-review-comment-jsexpr comment)
  (match-define (github-review-comment #:path path
                                       #:body body
                                       #:start-line start-line
                                       #:end-line end-line
                                       #:start-side start-side
                                       #:end-side end-side)
    comment)
  (if (= start-line end-line)
      (hash 'path path 'body body 'line end-line 'side end-side)
      (hash 'path
            path
            'body
            body
            'start_line
            start-line
            'line
            end-line
            'start_side
            start-side
            'side
            end-side)))

(define (git-path path)
  (string-split (run-command "git" "ls-tree" "-r" "-z" "--name-only" "HEAD" path) "\0"))

(define git-pr-ref-regexp #rx"^refs/pull/([0-9]+)/merge$")

(define (git-ref->pr-number ref)
  (match ref
    [(regexp git-pr-ref-regexp (list _ num)) (string->number num)]
    [_ (error (format "ref ~a doesn't represent a pull request" ref))]))

(define (refactoring-result->github-review-comment result)
  (define path
    (file-source-path (syntax-replacement-source (refactoring-result-syntax-replacement result))))
  (define replacement (refactoring-result-line-replacement result))
  (define body
    (format #<<EOS
**`~a`:** ~a

```suggestion
~a
```

<details>
<summary>Debugging details</summary>

<details>
  <summary>Textual replacement</summary>

  ```scheme
~a
  ```
</details>

<details>
  <summary>Syntactic replacement</summary>

  ```scheme
~a
  ```
</details>
</details>
EOS
            (refactoring-result-rule-name result)
            (refactoring-result-message result)
            (line-replacement-new-text replacement)
            (string-indent (pretty-format replacement) #:amount 2)
            (string-indent (pretty-format (refactoring-result-syntax-replacement result))
                           #:amount 2)))
  (github-review-comment #:path (first (git-path path))
                         #:body body
                         #:start-line (line-replacement-start-line replacement)
                         #:end-line (line-replacement-original-end-line replacement)
                         #:start-side "RIGHT"
                         #:end-side "RIGHT"))

(define branch-ref (getenv "GITHUB_REF"))
(define github-repository (getenv "GITHUB_REPOSITORY"))

(define (github-review-body comments? file-count)
  (format "[Resyntax](https://docs.racket-lang.org/resyntax/) analyzed ~a in this pull request and ~a"
          (if (= file-count 1)
              "1 file"
              (format "~a files" file-count))
          (if comments? "has added suggestions." "found no issues.")))

(define (refactoring-results->github-review results #:file-count file-count)
  (define comments
    (transduce results (mapping refactoring-result->github-review-comment) #:into into-list))
  (github-review-request #:owner-repo github-repository
                         #:pull-number (git-ref->pr-number branch-ref)
                         #:body (github-review-body (not (null? comments)) file-count)
                         #:event (if (empty? comments) "APPROVE" "COMMENT")
                         #:comments comments))
