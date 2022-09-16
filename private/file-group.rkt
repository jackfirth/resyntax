#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [file-portion? predicate/c]
  [file-portion (-> path-string? range-set? file-portion?)]
  [file-portion-path (-> file-portion? complete-path?)]
  [file-portion-lines (-> file-portion? range-set?)]
  [file-groups-resolve (-> (sequence/c file-group?) (listof file-portion?))]
  [file-group? predicate/c]
  [single-file-group? predicate/c]
  [single-file-group (-> path-string? single-file-group?)]
  [directory-file-group? predicate/c]
  [directory-file-group (-> path-string? directory-file-group?)]
  [package-file-group? predicate/c]
  [package-file-group (-> string? package-file-group?)]
  [git-repository-file-group? predicate/c]
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
         rebellion/collection/list
         rebellion/collection/range-set
         rebellion/private/guarded-block
         rebellion/streaming/transducer
         resyntax/private/run-command)


;@----------------------------------------------------------------------------------------------------


(struct file-portion (path lines)
  #:transparent
  #:guard (λ (path lines _) (values (simple-form-path path) lines)))


(struct file-group () #:transparent)


(struct single-file-group file-group (path ranges)
  #:transparent
  #:guard (λ (path _) (simple-form-path path)))


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


(define (file-groups-resolve groups)
  (transduce groups
             (append-mapping file-group-resolve)

             ;; TODO: this is incorrect - there could be overlapping portions of the same file. The
             ;; fix is to group the portions by filename and merge their range sets together. I don't
             ;; think I've implemented that operation for range sets yet so I'll get back to that. The
             ;; bug only occurs if the same file is included in multiple groups with different ranges.
             (deduplicating)

             #:into into-list))


(define (file-group-resolve group)
  (define files
    (match group
      [(single-file-group path ranges)
       (list (file-portion path ranges))]
      [(directory-file-group path)
       (for/list ([file (in-directory path)])
         (file-portion file (range-set (unbounded-range #:comparator natural<=>))))]
      [(package-file-group package-name)
       (for/list ([file (in-directory (simple-form-path (pkg-directory package-name)))])
         (file-portion file (range-set (unbounded-range #:comparator natural<=>))))]
      [(git-repository-file-group repository-path ref)
       (parameterize ([current-directory repository-path])
         (define null-separated-filenames
           (run-command "git" "diff" "--name-only" "-z" "--diff-filter=AM" ref "--"))
         (for/list ([file (string-split null-separated-filenames "\0")])
           (file-portion file (range-set (unbounded-range #:comparator natural<=>)))))]))
  (transduce files (filtering rkt-file?) #:into into-list))


(define/guard (rkt-file? portion)
  (define path (file-portion-path portion))
  (guard (path-has-extension? path #".rkt") else
    #false)
  (define content (file->string path))
  (string-prefix? content "#lang racket/base"))
