#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [github-review-request? predicate/c]
  [github-review-request-send (-> github-review-request? jsexpr?)]
  [refactoring-results->github-review
   (-> (sequence/c refactoring-result?) #:file-count exact-nonnegative-integer?
       github-review-request?)]))


(require json
         net/url
         net/url-connect
         racket/list
         racket/match
         racket/pretty
         racket/sequence
         racket/string
         rebellion/collection/list
         rebellion/streaming/reducer
         rebellion/streaming/transducer
         rebellion/type/record
         resyntax/refactoring-result
         resyntax/line-replacement
         resyntax/private/run-command
         resyntax/private/string-indent
         resyntax/source
         uri-old)


;@----------------------------------------------------------------------------------------------------


; https://docs.github.com/en/actions/reference/authentication-in-a-workflow#about-the-github_token-secret
(define github-token
  (make-parameter (getenv "GITHUB_TOKEN") #false 'github-token))


; Returns the API URL. For example: `https://api.github.com`.
(define github-api-url
  (make-parameter (getenv "GITHUB_API_URL") #false 'github-api-url))


(define-record-type github-review-request
  (owner-repo pull-number body event comments))


(define (github-review-request-jsexpr req)
  (match-define (github-review-request #:body body #:event event #:comments comments) req)
  (hash 'body body 'event event 'comments (map github-review-comment-jsexpr comments)))


(define-record-type github-review-comment
  (path body start-line end-line start-side end-side))


(define (github-review-comment-jsexpr comment)
  (match-define
    (github-review-comment #:path path
                           #:body body
                           #:start-line start-line
                           #:end-line end-line
                           #:start-side start-side
                           #:end-side end-side)
    comment)
  (if (= start-line end-line)
      (hash 'path path
            'body body
            'line end-line
            'side end-side)
      (hash 'path path
            'body body
            'start_line start-line
            'line end-line
            'start_side start-side
            'side end-side)))


; https://docs.github.com/en/rest/reference/pulls#create-a-review-for-a-pull-request
(define (github-review-request-url req)
  (string->url
   (format "~a/repos/~a/pulls/~a/reviews"
           (github-api-url)
           (github-review-request-owner-repo req)
           (github-review-request-pull-number req))))


(define (github-review-request-send req)
  (parameterize ([current-https-protocol 'secure])
    (define response-port
      (post-pure-port
       (github-review-request-url req)
       (jsexpr->bytes (github-review-request-jsexpr req))
       ; https://docs.github.com/en/rest/reference/pulls#list-review-comments-in-a-repository-preview-notices
       (list "Accept: application/vnd.github.comfort-fade-preview+json"
             (format "Authorization: Bearer ~a" (github-token)))))
    (define response-or-eof (read-json response-port))
    (if (eof-object? response-or-eof)
        (error (format "No response data for request to ~a"
                       (github-review-request-url req)))
        response-or-eof)))


(define (github-new-issue-url #:owner owner
                              #:repository repo
                              #:title title
                              #:body body
                              #:labels [labels #()])
  (define label-string
    (transduce labels
               (mapping symbol->string)
               (mapping uri-escape-i)
               #:into (join-into-string ",")))
  (define label-part (if (equal? label-string "") "" (format "&labels=~a" label-string)))
  (format "https://github.com/~a/~a/issues/new?title=~a&body=~a~a"
          (uri-escape-i owner)
          (uri-escape-i repo)
          (uri-escape-i title)
          (uri-escape-i body)
          label-part))


(define (git-path path)
  (string-split (run-command "git" "ls-tree" "-r" "-z" "--name-only" "HEAD" path) "\0"))


(define git-pr-ref-regexp #rx"^refs/pull/([0-9]+)/merge$")


(define (git-pr-ref? ref)
  (regexp-match git-pr-ref-regexp ref))


(define (git-ref->pr-number ref)
  (match ref
    [(regexp git-pr-ref-regexp (list _ num))
     (string->number num)]
    [_
     (error (format "ref ~a doesn't represent a pull request" ref))]))


(define (refactoring-result->github-review-comment result)
  (define path (file-source-path (refactoring-result-source result)))
  (define replacement (refactoring-result-line-replacement result))
  (define body
    (format #<<EOS
**`~a`** ~a

```suggestion
~a
```

Debugging details below:

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
EOS
            (refactoring-result-rule-name result)
            (refactoring-result-message result)
            (line-replacement-new-text replacement)
            (string-indent (pretty-format replacement) #:amount 2)
            (string-indent (pretty-format (refactoring-result-replacement result)) #:amount 2)))
  (github-review-comment
   #:path (first (git-path path))
   #:body body
   #:start-line (line-replacement-start-line replacement)
   #:end-line (line-replacement-original-end-line replacement)
   #:start-side "RIGHT"
   #:end-side "RIGHT"))


(define branch-ref (getenv "GITHUB_REF"))
(define github-repository (getenv "GITHUB_REPOSITORY"))


(define (github-review-body comments? file-count)
  (format "[Resyntax](https://docs.racket-lang.org/resyntax/) analyzed ~a in this pull request and ~a"
          (if (= file-count 1) "1 file" (format "~a files" file-count))
          (if comments? "has added suggestions." "found no issues.")))


(define (refactoring-results->github-review results #:file-count file-count)
  (define comments
    (transduce results (mapping refactoring-result->github-review-comment) #:into into-list))
  (define review
    (github-review-request
     #:owner-repo (format "origin/~a" github-repository)
     #:pull-number (git-ref->pr-number branch-ref)
     #:body (github-review-body (not (null? comments)) file-count)
     #:event (if (empty? comments) "APPROVE" "REQUEST_CHANGES")
     #:comments comments))
  (printf "DEBUG: github review request\n\n~v\n\n" (pretty-format review))
  review)
