#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [github-review-request? (-> any/c boolean?)]
  [github-review-request-jsexpr (-> github-review-request? jsexpr?)]
  [refactoring-results->github-review
   (-> (sequence/c refactoring-result?) #:file-count exact-nonnegative-integer?
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


(define-record-type github-review-request
  (owner-repo pull-number body event comments))


(define (github-review-request-jsexpr req)
  (match-define
    (github-review-request
     #:owner-repo owner-repo #:pull-number pull-number #:body body #:event event #:comments comments)
    req)
  (match-define (list owner repo) (string-split owner-repo "/"))
  (hash 'owner owner
        'repo repo
        'body body
        'event event
        'comments (map github-review-comment-jsexpr comments)
        'pull_number pull-number))


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


(define (git-path path)
  (string-split (run-command "git" "ls-tree" "-r" "-z" "--name-only" "HEAD" path) "\0"))


(define git-pr-ref-regexp #rx"^refs/pull/([0-9]+)/merge$")


(define (git-ref->pr-number ref)
  (match ref
    [(regexp git-pr-ref-regexp (list _ num))
     (string->number num)]
    [_
     (error (format "ref ~a doesn't represent a pull request" ref))]))


(define (refactoring-result->github-review-comment result)
  (cond
    [(refactoring-result-has-fix? result)
     ;; For results with fixes, generate a suggestion comment
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
     (github-review-comment
      #:path (first (git-path path))
      #:body body
      #:start-line (line-replacement-start-line replacement)
      #:end-line (line-replacement-original-end-line replacement)
      #:start-side "RIGHT"
      #:end-side "RIGHT")]
    [else
     ;; For warning-only results, generate a comment without a suggestion
     (define source (refactoring-result-source result))
     (define path (file-source-path source))
     (define line (refactoring-result-original-line result))
     (define body
       (format "**`~a`:** ~a"
               (refactoring-result-rule-name result)
               (refactoring-result-message result)))
     (github-review-comment
      #:path (first (git-path path))
      #:body body
      #:start-line line
      #:end-line line
      #:start-side "RIGHT"
      #:end-side "RIGHT")]))


(define branch-ref (getenv "GITHUB_REF"))
(define github-repository (getenv "GITHUB_REPOSITORY"))


(define (github-review-body comments? file-count)
  (format "[Resyntax](https://docs.racket-lang.org/resyntax/) analyzed ~a in this pull request and ~a"
          (if (= file-count 1) "1 file" (format "~a files" file-count))
          (if comments? "has added suggestions." "found no issues.")))


(define (refactoring-results->github-review results #:file-count file-count)
  (define comments
    (transduce results (mapping refactoring-result->github-review-comment) #:into into-list))
  (github-review-request
   #:owner-repo github-repository
   #:pull-number (git-ref->pr-number branch-ref)
   #:body (github-review-body (not (null? comments)) file-count)
   #:event (if (empty? comments) "APPROVE" "COMMENT")
   #:comments comments))


(module+ test
  (require rackunit)

  (test-case "github-review-comment-jsexpr"
    (test-case "single-line comment"
      (define comment
        (github-review-comment
         #:path "file.rkt"
         #:body "Fix this issue"
         #:start-line 10
         #:end-line 10
         #:start-side "RIGHT"
         #:end-side "RIGHT"))
      (define result (github-review-comment-jsexpr comment))
      (check-equal? (hash-ref result 'path) "file.rkt")
      (check-equal? (hash-ref result 'body) "Fix this issue")
      (check-equal? (hash-ref result 'line) 10)
      (check-equal? (hash-ref result 'side) "RIGHT")
      (check-false (hash-has-key? result 'start_line))
      (check-false (hash-has-key? result 'start_side)))
    (test-case "multi-line comment"
      (define comment
        (github-review-comment
         #:path "another.rkt"
         #:body "Multi-line issue"
         #:start-line 5
         #:end-line 8
         #:start-side "LEFT"
         #:end-side "RIGHT"))
      (define result (github-review-comment-jsexpr comment))
      (check-equal? (hash-ref result 'path) "another.rkt")
      (check-equal? (hash-ref result 'body) "Multi-line issue")
      (check-equal? (hash-ref result 'start_line) 5)
      (check-equal? (hash-ref result 'line) 8)
      (check-equal? (hash-ref result 'start_side) "LEFT")
      (check-equal? (hash-ref result 'side) "RIGHT")))
  (test-case "github-review-request-jsexpr"
    (define comment1
      (github-review-comment
       #:path "file1.rkt"
       #:body "Comment 1"
       #:start-line 1
       #:end-line 1
       #:start-side "RIGHT"
       #:end-side "RIGHT"))
    (define comment2
      (github-review-comment
       #:path "file2.rkt"
       #:body "Comment 2"
       #:start-line 5
       #:end-line 10
       #:start-side "LEFT"
       #:end-side "RIGHT"))
    (define request
      (github-review-request
       #:owner-repo "owner/repo"
       #:pull-number 123
       #:body "Review body"
       #:event "COMMENT"
       #:comments (list comment1 comment2)))
    (define result (github-review-request-jsexpr request))
    (check-equal? (hash-ref result 'owner) "owner")
    (check-equal? (hash-ref result 'repo) "repo")
    (check-equal? (hash-ref result 'pull_number) 123)
    (check-equal? (hash-ref result 'body) "Review body")
    (check-equal? (hash-ref result 'event) "COMMENT")
    (check-equal? (length (hash-ref result 'comments)) 2))
  (test-case "git-ref->pr-number"
    (test-case "valid PR ref"
      (check-equal? (git-ref->pr-number "refs/pull/42/merge") 42)
      (check-equal? (git-ref->pr-number "refs/pull/123/merge") 123)
      (check-equal? (git-ref->pr-number "refs/pull/1/merge") 1))
    (test-case "invalid refs raise errors"
      (check-exn exn:fail? (lambda () (git-ref->pr-number "refs/heads/main")))
      (check-exn exn:fail? (lambda () (git-ref->pr-number "refs/pull/42/head")))
      (check-exn exn:fail? (lambda () (git-ref->pr-number "invalid")))))
  (test-case "github-review-body"
    (test-case "with comments"
      (check-equal? (github-review-body #t 1)
                    "[Resyntax](https://docs.racket-lang.org/resyntax/) analyzed 1 file in this pull request and has added suggestions.")
      (check-equal? (github-review-body #t 5)
                    "[Resyntax](https://docs.racket-lang.org/resyntax/) analyzed 5 files in this pull request and has added suggestions."))
    (test-case "without comments"
      (check-equal? (github-review-body #f 1)
                    "[Resyntax](https://docs.racket-lang.org/resyntax/) analyzed 1 file in this pull request and found no issues.")
      (check-equal? (github-review-body #f 3)
                    "[Resyntax](https://docs.racket-lang.org/resyntax/) analyzed 3 files in this pull request and found no issues."))))
