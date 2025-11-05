#lang racket/base


(require rackunit
         resyntax/private/github)


(module+ test
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
