#lang racket/base


(require racket/contract)


(provide
 (contract-out
  [git-diff-modified-lines (-> string? (hash/c path? immutable-range-set?))]))


(require fancy-app
         racket/match
         racket/port
         racket/string
         racket/system
         rebellion/base/comparator
         rebellion/base/option
         rebellion/base/range
         rebellion/collection/range-set)


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
  (define single-line-match (regexp-match #px"^@@ .* \\+(\\d+) @@$" line))
  (define range-match (regexp-match #px"^@@ .* \\+(\\d+),(\\d+) @@$" line))
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
