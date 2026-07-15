#lang racket/base


(require racket/contract)


(provide
 (contract-out
  [git-diff-modified-lines (-> string? (hash/c path? immutable-range-set?))]
  [git-commit! (-> string? void?)]))


(require fancy-app
         racket/match
         racket/port
         racket/string
         racket/system
         rebellion/base/comparator
         rebellion/base/option
         rebellion/base/range
         rebellion/collection/range-set)


(module+ test
  (require racket/file
           rackunit
           resyntax/private/run-command))


;@----------------------------------------------------------------------------------------------------



(define (git-diff-modified-lines ref)
  (define cmd
    (format
     "git diff -U0 --inter-hunk-context=0 --diff-filter=AM ~a | grep -e '^+++ b/' -e '^@@'" ref))
  (define empty-lines (range-set #:comparator natural<=>))
  (for*/fold ([files (hash)]
              [current-file #false]
              #:result files)
             ([line (in-list (string-split (with-output-to-string (λ () (system cmd))) "\n"))]
              [lexeme (in-option (lex-line line))])
    (if (path? lexeme)
        (values files lexeme)
        (values (hash-update files current-file (range-set-add _ lexeme) empty-lines)
                current-file))))


(define (lex-line line)
  (define file-match (regexp-match #px"^\\+\\+\\+ b/(.*)$" line))
  (define single-line-match (regexp-match #px"^@@ .* \\+(\\d+) @@" line))
  (define range-match (regexp-match #px"^@@ .* \\+(\\d+),(\\d+) @@" line))
  (cond
    [file-match
     (match-define (list _ f) file-match)
     (present (simplify-path f))]
    [single-line-match
     (match-define (list _ l) single-line-match)
     (let ([l (string->number l)])
       (present (closed-open-range l (add1 l) #:comparator natural<=>)))]
    [range-match
     (match-define (list _ l s) range-match)
     (let ([l (string->number l)]
           [s (string->number s)])
       (if (equal? s 0)
           absent ;; TODO: fix range-set-add so that I can return an empty range here
           (present (closed-open-range l (+ l s) #:comparator natural<=>))))]
    [else
     (raise-argument-error
      'lex-line
      "a git file name line (starting with '+++ b/') or a hunk range line (starting with '@@')"
      line)]))


(define (git-commit! message)
  (define escaped-message (string-replace message "'" "'\"'\"'"))
  (unless (system (format "git commit --all --quiet --message='~a'" escaped-message))
    (raise-arguments-error 'git-commit!
                           "committing files to Git failed"
                           "commit message" message)))


(module+ test

  (define (git-quietly! . args)
    (parameterize ([current-output-port (open-output-nowhere)]
                   [current-error-port (open-output-nowhere)])
      (unless (apply system* (find-executable-path "git") args)
        (error 'git-quietly! "git command failed: git ~a" (string-join args " ")))))

  (define (call-with-temporary-git-repository body)
    (define repo-dir (make-temporary-directory "resyntax-git-test-~a"))
    (dynamic-wind
     void
     (λ ()
       (parameterize ([current-directory repo-dir])
         (git-quietly! "init")
         (git-quietly! "config" "user.name" "Resyntax Tests")
         (git-quietly! "config" "user.email" "resyntax-tests@example.com")
         (git-quietly! "config" "commit.gpgsign" "false")
         (display-to-file "line one\nline two\nline three\nline four\nline five\n" "example.txt")
         (git-quietly! "add" "--all")
         (git-quietly! "commit" "--message" "initial commit")
         (body)))
     (λ () (delete-directory/files repo-dir))))

  (define (line-range start end)
    (closed-open-range start end #:comparator natural<=>))

  (test-case "lex-line"

    (test-case "file name line"
      (check-equal? (lex-line "+++ b/some/file.rkt") (present (build-path "some/file.rkt"))))

    (test-case "single-line hunk"
      (check-equal? (lex-line "@@ -2 +7 @@") (present (line-range 7 8))))

    (test-case "multi-line hunk"
      (check-equal? (lex-line "@@ -2,3 +7,4 @@") (present (line-range 7 11))))

    (test-case "deletion-only hunk"
      (check-equal? (lex-line "@@ -2,3 +7,0 @@") absent))

    (test-case "unrecognized line"
      (check-exn exn:fail:contract? (λ () (lex-line "not a diff line")))))

  (test-case "git-diff-modified-lines"

    (test-case "no changes"
      (call-with-temporary-git-repository
       (λ ()
         (check-equal? (git-diff-modified-lines "HEAD") (hash)))))

    (test-case "modified lines"
      (call-with-temporary-git-repository
       (λ ()
         (display-to-file "line one\nCHANGED\nline three\nCHANGED\nCHANGED\n" "example.txt"
                          #:exists 'replace)
         (check-equal? (git-diff-modified-lines "HEAD")
                       (hash (build-path "example.txt")
                             (range-set (line-range 2 3) (line-range 4 6)))))))

    (test-case "added file"
      (call-with-temporary-git-repository
       (λ ()
         (display-to-file "line one\nline two\nline three\n" "added.txt")
         (git-quietly! "add" "--all")
         (check-equal? (git-diff-modified-lines "HEAD")
                       (hash (build-path "added.txt") (range-set (line-range 1 4)))))))

    (test-case "deleted lines are not modifications"
      (call-with-temporary-git-repository
       (λ ()
         (display-to-file "line one\nline two\nline four\nline five\n" "example.txt"
                          #:exists 'replace)
         (check-equal? (git-diff-modified-lines "HEAD") (hash))))))

  (test-case "git-commit!"

    (test-case "commits all modified files"
      (call-with-temporary-git-repository
       (λ ()
         (display-to-file "line one\nCHANGED\nline three\nline four\nline five\n" "example.txt"
                          #:exists 'replace)
         (git-commit! "add a line, don't break the 'quoting'")
         (check-equal? (string-trim (run-command "git" "log" "-1" "--format=%s"))
                       "add a line, don't break the 'quoting'")
         (check-equal? (string-trim (run-command "git" "status" "--porcelain")) ""))))

    (test-case "raises when committing fails"
      (call-with-temporary-git-repository
       (λ ()
         (check-exn #rx"committing files to Git failed"
                    (λ ()
                      (parameterize ([current-output-port (open-output-nowhere)])
                        (git-commit! "empty commit")))))))))
