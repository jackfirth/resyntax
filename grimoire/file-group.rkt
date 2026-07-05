#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [file-groups-resolve (-> (sequence/c file-group?) (hash/c file-source? immutable-range-set?))]
  [file-group? (-> any/c boolean?)]
  [single-file-group? (-> any/c boolean?)]
  [single-file-group (-> path-string? immutable-range-set? single-file-group?)]
  [directory-file-group? (-> any/c boolean?)]
  [directory-file-group (-> path-string? directory-file-group?)]
  [package-file-group? (-> any/c boolean?)]
  [package-file-group (-> string? package-file-group?)]
  [git-repository-file-group? (-> any/c boolean?)]
  [git-repository-file-group (-> path-string? string? git-repository-file-group?)]))


(require fancy-app
         pkg/lib
         racket/file
         racket/match
         racket/path
         racket/sequence
         racket/string
         rebellion/base/comparator
         rebellion/base/range
         rebellion/collection/entry
         rebellion/collection/hash
         rebellion/collection/list
         rebellion/collection/range-set
         rebellion/streaming/reducer
         rebellion/streaming/transducer
         resyntax/private/git
         resyntax/private/logger
         resyntax/grimoire/source)


(module+ test
  (require (submod "..")
           racket/file
           racket/list
           racket/system
           rackunit))


;@----------------------------------------------------------------------------------------------------


(struct file-group () #:transparent)


(struct single-file-group file-group (path ranges)
  #:transparent
  #:guard (λ (path ranges _) (values (simple-form-path path) ranges)))


(struct directory-file-group file-group (path)
  #:transparent
  #:guard (λ (path _) (simple-form-path path)))


(struct package-file-group file-group (package-name)
  #:transparent
  #:guard (λ (package-name _) (string->immutable-string package-name)))


(struct git-repository-file-group file-group (repository-path ref)
  #:transparent
  #:guard
  (λ (repository-path ref _)
    (values (simple-form-path repository-path) (string->immutable-string ref))))


(define all-lines (range-set (unbounded-range #:comparator natural<=>)))


(define (file-groups-resolve groups)
  (transduce groups
             (append-mapping file-group-resolve)
             (grouping (make-fold-reducer range-set-add-all (range-set #:comparator natural<=>)))
             #:into into-hash))


;; Resolves a single group into a list of entries mapping file sources to line range sets.
(define (file-group-resolve group)
  (define path-entries
    (match group
      [(single-file-group path lines)
       (list (entry path lines))]
      [(directory-file-group path)
       (for/list ([file (in-directory path)])
         (entry file all-lines))]
      [(package-file-group package-name)
       (define pkgdir (pkg-directory package-name))
       (unless pkgdir
         (raise-user-error 'resyntax
                           "cannot analyze package ~a, it hasn't been installed"
                           package-name))
       (for/list ([file (in-directory (simple-form-path pkgdir))])
         (entry file all-lines))]
      [(git-repository-file-group repository-path ref)
       (parameterize ([current-directory repository-path])
         (define diff-lines (git-diff-modified-lines ref))
         (for/list ([(file lines) (in-hash diff-lines)])
           (log-resyntax-debug "~a: modified lines: ~a" file lines)
           ;; Paths from the diff are relative to the repository, so they're resolved eagerly here
           ;; while the current directory is still parameterized to the repository path.
           (entry (simple-form-path file) (expand-modified-line-set lines))))]))
  (transduce path-entries
             (filtering (λ (e) (rkt-path? (entry-key e))))
             (mapping (λ (e) (entry (file-source (entry-key e)) (entry-value e))))
             #:into into-list))


(define (rkt-path? path)
  (path-has-extension? path #".rkt"))


;; GitHub allows pull request reviews to include comments only on modified lines, plus the 3 lines
;; before and after any modified lines.
(define (expand-modified-line-set lines)
  (define context-lines
    (for/list ([line-range (in-range-set lines)])
      (range (range-bound-map (range-lower-bound line-range) (λ (x) (max 0 (- x 3))))
             (range-bound-map (range-upper-bound line-range) (λ (x) (+ x 3)))
             #:comparator (range-comparator line-range))))
  (range-set-add-all lines context-lines))


(define (range-bound-map bound f)
  (if (unbounded? bound)
      unbounded
      (range-bound (f (range-bound-endpoint bound)) (range-bound-type bound))))


(module+ test
  
  (test-case "single-file-group"
    (test-case "constructor and predicates"
      (define group (single-file-group "/tmp/test.rkt" (range-set (closed-open-range 1 10 #:comparator natural<=>))))
      (check-true (single-file-group? group))
      (check-true (file-group? group))
      (check-equal? (single-file-group-path group) (simple-form-path "/tmp/test.rkt"))
      (check-equal? (single-file-group-ranges group) (range-set (closed-open-range 1 10 #:comparator natural<=>))))
    
    (test-case "resolution returns single file"
      (define test-dir (make-temporary-directory "resyntax-test-~a"))
      (define test-file (build-path test-dir "test.rkt"))
      (call-with-output-file test-file
        (λ (out) (displayln "#lang racket/base" out)))
      (define group (single-file-group test-file (range-set (closed-open-range 1 5 #:comparator natural<=>))))
      (define resolved (file-groups-resolve (list group)))
      (check-equal? (hash-count resolved) 1)
      (check-equal? (hash-ref resolved (file-source test-file))
                    (range-set (closed-open-range 1 5 #:comparator natural<=>)))
      (delete-directory/files test-dir)))
  
  (test-case "directory-file-group"
    (test-case "constructor and predicates"
      (define group (directory-file-group "/tmp"))
      (check-true (directory-file-group? group))
      (check-true (file-group? group))
      (check-equal? (directory-file-group-path group) (simple-form-path "/tmp")))
    
    (test-case "resolution returns only .rkt files"
      (define test-dir (make-temporary-directory "resyntax-test-~a"))
      (define rkt-file1 (build-path test-dir "test1.rkt"))
      (define rkt-file2 (build-path test-dir "test2.rkt"))
      (define txt-file (build-path test-dir "test.txt"))
      (call-with-output-file rkt-file1
        (λ (out) (displayln "#lang racket/base" out)))
      (call-with-output-file rkt-file2
        (λ (out) (displayln "#lang racket" out)))
      (call-with-output-file txt-file
        (λ (out) (displayln "not racket" out)))
      (define group (directory-file-group test-dir))
      (define resolved (file-groups-resolve (list group)))
      (check-equal? (hash-count resolved) 2)
      (check-true (hash-has-key? resolved (file-source rkt-file1)))
      (check-true (hash-has-key? resolved (file-source rkt-file2)))
      (delete-directory/files test-dir)))
  
  (test-case "package-file-group"
    (test-case "constructor and predicates"
      (define group (package-file-group "rackunit"))
      (check-true (package-file-group? group))
      (check-true (file-group? group))
      (check-equal? (package-file-group-package-name group) "rackunit"))
    
    (test-case "resolution returns files from installed package"
      (define group (package-file-group "rackunit"))
      (define resolved (file-groups-resolve (list group)))
      (check-true (hash? resolved))
      (check-true (> (hash-count resolved) 0))
      (for ([src (in-hash-keys resolved)])
        (check-pred file-source? src)
        (check-true (path-has-extension? (source-path src) #".rkt"))))

    (test-case "resolution raises error for non-existent package"
      (define group (package-file-group "this-package-does-not-exist-xyz"))
      (check-exn exn:fail:user?
                 (λ () (file-groups-resolve (list group))))))
  
  (test-case "git-repository-file-group"
    (test-case "constructor and predicates"
      (define group (git-repository-file-group "/tmp" "HEAD"))
      (check-true (git-repository-file-group? group))
      (check-true (file-group? group))
      (check-equal? (git-repository-file-group-repository-path group) (simple-form-path "/tmp"))
      (check-equal? (git-repository-file-group-ref group) "HEAD"))
    
    (test-case "file-group-resolve with git repository"
      (define test-dir (make-temporary-directory "resyntax-test-git-~a"))
      (parameterize ([current-directory test-dir])
        (unless (system "git init -q")
          (fail "git init failed"))
        (unless (system "git config user.email 'test@example.com'")
          (fail "git config email failed"))
        (unless (system "git config user.name 'Test User'")
          (fail "git config name failed"))
        (define test-file (build-path test-dir "test.rkt"))
        (call-with-output-file test-file
          (λ (out) (displayln "#lang racket/base\n(void)" out)))
        (unless (system "git add test.rkt")
          (fail "git add failed"))
        (unless (system "git commit -q -m 'Initial commit'")
          (fail "git commit failed"))
        (call-with-output-file test-file #:exists 'append
          (λ (out) (displayln "(define x 1)" out)))
        (define group (git-repository-file-group test-dir "HEAD"))
        (define resolved (file-groups-resolve (list group)))
        (check-true (hash? resolved))
        (check-true (> (hash-count resolved) 0))
        (for ([src (in-hash-keys resolved)])
          (check-pred file-source? src)))
      (delete-directory/files test-dir)))
  
  (test-case "file-groups-resolve"
    (test-case "resolves multiple groups into hash"
      (define test-dir (make-temporary-directory "resyntax-test-~a"))
      (define test-file1 (build-path test-dir "test1.rkt"))
      (define test-file2 (build-path test-dir "test2.rkt"))
      (call-with-output-file test-file1
        (λ (out) (displayln "#lang racket/base" out)))
      (call-with-output-file test-file2
        (λ (out) (displayln "#lang racket" out)))
      (define group1 (single-file-group test-file1 (range-set (closed-open-range 1 5 #:comparator natural<=>))))
      (define group2 (single-file-group test-file2 (range-set (closed-open-range 3 8 #:comparator natural<=>))))
      (define result (file-groups-resolve (list group1 group2)))
      (check-true (hash? result))
      (check-equal? (hash-count result) 2)
      (check-true (hash-has-key? result (file-source test-file1)))
      (check-true (hash-has-key? result (file-source test-file2)))
      (delete-directory/files test-dir))
    
    (test-case "combines ranges for same file"
      (define test-dir (make-temporary-directory "resyntax-test-~a"))
      (define test-file (build-path test-dir "test.rkt"))
      (call-with-output-file test-file
        (λ (out) (displayln "#lang racket/base" out)))
      (define group1 (single-file-group test-file (range-set (closed-open-range 1 3 #:comparator natural<=>))))
      (define group2 (single-file-group test-file (range-set (closed-open-range 5 7 #:comparator natural<=>))))
      (define result (file-groups-resolve (list group1 group2)))
      (check-equal? (hash-count result) 1)
      (define combined-ranges (hash-ref result (file-source test-file)))
      (check-true (range-set-contains? combined-ranges 1))
      (check-true (range-set-contains? combined-ranges 2))
      (check-true (range-set-contains? combined-ranges 5))
      (check-true (range-set-contains? combined-ranges 6))
      (delete-directory/files test-dir)))
  
  (test-case "rkt-path?"
    (test-case "returns true for .rkt files"
      (check-true (rkt-path? "/tmp/test.rkt")))

    (test-case "returns false for non-.rkt files"
      (check-false (rkt-path? "/tmp/test.txt"))
      (check-false (rkt-path? "/tmp/test.scm"))))
  
  (test-case "range-bound-map"
    (test-case "maps bounded endpoints"
      (define bound (range-bound 5 inclusive))
      (define result (range-bound-map bound (λ (x) (* x 2))))
      (check-equal? (range-bound-endpoint result) 10)
      (check-equal? (range-bound-type result) inclusive))
    
    (test-case "preserves unbounded"
      (define result (range-bound-map unbounded (λ (x) (* x 2))))
      (check-equal? result unbounded)))
  
  (test-case "expand-modified-line-set"
    (define ranges (range-set (closed-open-range 4 6) (greater-than-range 15)))
    (define expected (range-set (closed-open-range 1 9) (greater-than-range 12))) 
    (check-equal? (expand-modified-line-set ranges) expected)))
