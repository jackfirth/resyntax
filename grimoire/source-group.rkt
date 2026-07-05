#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [source-group? (-> any/c boolean?)]
  [empty-source-group source-group?]
  [source-group-union (-> source-group? ... source-group?)]
  [source-group-resolve (-> source-group? (hash/c file-source? immutable-range-set?))]
  [single-source-group? (-> any/c boolean?)]
  [single-source-group (-> path-string? immutable-range-set? single-source-group?)]
  [directory-source-group? (-> any/c boolean?)]
  [directory-source-group (-> path-string? directory-source-group?)]
  [package-source-group? (-> any/c boolean?)]
  [package-source-group (-> string? package-source-group?)]
  [git-repository-source-group? (-> any/c boolean?)]
  [git-repository-source-group (-> path-string? string? git-repository-source-group?)]))


(require fancy-app
         pkg/lib
         racket/file
         racket/match
         racket/path
         racket/set
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


(struct source-group () #:transparent)


(struct single-source-group source-group (path ranges)
  #:transparent
  #:guard (λ (path ranges _) (values (simple-form-path path) ranges)))


(struct directory-source-group source-group (path)
  #:transparent
  #:guard (λ (path _) (simple-form-path path)))


(struct package-source-group source-group (package-name)
  #:transparent
  #:guard (λ (package-name _) (string->immutable-string package-name)))


(struct git-repository-source-group source-group (repository-path ref)
  #:transparent
  #:guard
  (λ (repository-path ref _)
    (values (simple-form-path repository-path) (string->immutable-string ref))))


;; A union of any number of the other kinds of source groups. Union groups are always normalized:
;; the subgroups set never contains union groups, so that equal? on source groups treats
;; source-group-union as commutative, associative, and idempotent, with empty-source-group as the
;; identity element.
(struct union-source-group source-group (subgroups) #:transparent)


(define empty-source-group (union-source-group (set)))


(define (source-group-union . groups)
  (define combined
    (for*/set ([group (in-list groups)]
               [basic (in-set (source-group-basic-subgroups group))])
      basic))
  (cond
    [(set-empty? combined) empty-source-group]
    [(equal? (set-count combined) 1) (set-first combined)]
    [else (union-source-group combined)]))


(define (source-group-basic-subgroups group)
  (match group
    [(union-source-group subgroups) subgroups]
    [_ (set group)]))


(define all-lines (range-set (unbounded-range #:comparator natural<=>)))


(define (source-group-resolve group)
  (transduce (source-group-basic-subgroups group)
             (append-mapping basic-source-group-entries)
             (grouping (make-fold-reducer range-set-add-all (range-set #:comparator natural<=>)))
             #:into into-hash))


;; Resolves a single non-union group into a list of entries mapping file sources to line range sets.
(define (basic-source-group-entries group)
  (define path-entries
    (match group
      [(single-source-group path lines)
       (list (entry path lines))]
      [(directory-source-group path)
       (for/list ([file (in-directory path)])
         (entry file all-lines))]
      [(package-source-group package-name)
       (define pkgdir (pkg-directory package-name))
       (unless pkgdir
         (raise-user-error 'resyntax
                           "cannot analyze package ~a, it hasn't been installed"
                           package-name))
       (for/list ([file (in-directory (simple-form-path pkgdir))])
         (entry file all-lines))]
      [(git-repository-source-group repository-path ref)
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
  
  (test-case "single-source-group"
    (test-case "constructor and predicates"
      (define group (single-source-group "/tmp/test.rkt" (range-set (closed-open-range 1 10 #:comparator natural<=>))))
      (check-true (single-source-group? group))
      (check-true (source-group? group))
      (check-equal? (single-source-group-path group) (simple-form-path "/tmp/test.rkt"))
      (check-equal? (single-source-group-ranges group) (range-set (closed-open-range 1 10 #:comparator natural<=>))))
    
    (test-case "resolution returns single file"
      (define test-dir (make-temporary-directory "resyntax-test-~a"))
      (define test-file (build-path test-dir "test.rkt"))
      (call-with-output-file test-file
        (λ (out) (displayln "#lang racket/base" out)))
      (define group (single-source-group test-file (range-set (closed-open-range 1 5 #:comparator natural<=>))))
      (define resolved (source-group-resolve group))
      (check-equal? (hash-count resolved) 1)
      (check-equal? (hash-ref resolved (file-source test-file))
                    (range-set (closed-open-range 1 5 #:comparator natural<=>)))
      (delete-directory/files test-dir)))
  
  (test-case "directory-source-group"
    (test-case "constructor and predicates"
      (define group (directory-source-group "/tmp"))
      (check-true (directory-source-group? group))
      (check-true (source-group? group))
      (check-equal? (directory-source-group-path group) (simple-form-path "/tmp")))
    
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
      (define group (directory-source-group test-dir))
      (define resolved (source-group-resolve group))
      (check-equal? (hash-count resolved) 2)
      (check-true (hash-has-key? resolved (file-source rkt-file1)))
      (check-true (hash-has-key? resolved (file-source rkt-file2)))
      (delete-directory/files test-dir)))
  
  (test-case "package-source-group"
    (test-case "constructor and predicates"
      (define group (package-source-group "rackunit"))
      (check-true (package-source-group? group))
      (check-true (source-group? group))
      (check-equal? (package-source-group-package-name group) "rackunit"))
    
    (test-case "resolution returns files from installed package"
      (define group (package-source-group "rackunit"))
      (define resolved (source-group-resolve group))
      (check-true (hash? resolved))
      (check-true (> (hash-count resolved) 0))
      (for ([src (in-hash-keys resolved)])
        (check-pred file-source? src)
        (check-true (path-has-extension? (source-path src) #".rkt"))))

    (test-case "resolution raises error for non-existent package"
      (define group (package-source-group "this-package-does-not-exist-xyz"))
      (check-exn exn:fail:user?
                 (λ () (source-group-resolve group)))))
  
  (test-case "git-repository-source-group"
    (test-case "constructor and predicates"
      (define group (git-repository-source-group "/tmp" "HEAD"))
      (check-true (git-repository-source-group? group))
      (check-true (source-group? group))
      (check-equal? (git-repository-source-group-repository-path group) (simple-form-path "/tmp"))
      (check-equal? (git-repository-source-group-ref group) "HEAD"))
    
    (test-case "source-group-resolve with git repository"
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
        (define group (git-repository-source-group test-dir "HEAD"))
        (define resolved (source-group-resolve group))
        (check-true (hash? resolved))
        (check-true (> (hash-count resolved) 0))
        (for ([src (in-hash-keys resolved)])
          (check-pred file-source? src)))
      (delete-directory/files test-dir)))
  
  (test-case "source-group-union"

    (define g1 (directory-source-group "/tmp/dir1"))
    (define g2 (package-source-group "some-package"))
    (define g3 (single-source-group "/tmp/foo.rkt" (range-set #:comparator natural<=>)))

    (test-case "commutative"
      (check-equal? (source-group-union g1 g2) (source-group-union g2 g1)))

    (test-case "associative"
      (check-equal? (source-group-union (source-group-union g1 g2) g3)
                    (source-group-union g1 (source-group-union g2 g3))))

    (test-case "empty group is the identity"
      (check-equal? (source-group-union g1 empty-source-group) g1)
      (check-equal? (source-group-union empty-source-group g1) g1))

    (test-case "idempotent"
      (check-equal? (source-group-union g1 g1) g1))

    (test-case "no groups produce the empty group"
      (check-equal? (source-group-union) empty-source-group)
      (check-equal? (source-group-union empty-source-group empty-source-group) empty-source-group))

    (test-case "unioning a single group produces that group"
      (check-equal? (source-group-union g1) g1)))

  (test-case "source-group-resolve"
    (test-case "resolving the empty group produces an empty hash"
      (check-equal? (source-group-resolve empty-source-group) (hash)))

    (test-case "resolves unioned groups into hash"
      (define test-dir (make-temporary-directory "resyntax-test-~a"))
      (define test-file1 (build-path test-dir "test1.rkt"))
      (define test-file2 (build-path test-dir "test2.rkt"))
      (call-with-output-file test-file1
        (λ (out) (displayln "#lang racket/base" out)))
      (call-with-output-file test-file2
        (λ (out) (displayln "#lang racket" out)))
      (define group1 (single-source-group test-file1 (range-set (closed-open-range 1 5 #:comparator natural<=>))))
      (define group2 (single-source-group test-file2 (range-set (closed-open-range 3 8 #:comparator natural<=>))))
      (define result (source-group-resolve (source-group-union group1 group2)))
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
      (define group1 (single-source-group test-file (range-set (closed-open-range 1 3 #:comparator natural<=>))))
      (define group2 (single-source-group test-file (range-set (closed-open-range 5 7 #:comparator natural<=>))))
      (define result (source-group-resolve (source-group-union group1 group2)))
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
