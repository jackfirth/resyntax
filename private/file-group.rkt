#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [file-portion? (-> any/c boolean?)]
  [file-portion (-> path-string? range-set? file-portion?)]
  [file-portion-path (-> file-portion? complete-path?)]
  [file-portion-lines (-> file-portion? range-set?)]
  [file-groups-resolve (-> (sequence/c file-group?) (listof file-portion?))]
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
         guard
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
         rebellion/streaming/transducer
         resyntax/private/git
         resyntax/private/run-command)


;@----------------------------------------------------------------------------------------------------


(struct file-portion (path lines)
  #:transparent
  #:guard (λ (path lines _) (values (simple-form-path path) lines)))


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
       (define pkgdir (pkg-directory package-name))
       (unless pkgdir
         (raise-user-error 'resyntax
                           "cannot analyze package ~a, it hasn't been installed"
                           package-name))
       (for/list ([file (in-directory (simple-form-path pkgdir))])
         (file-portion file (range-set (unbounded-range #:comparator natural<=>))))]
      [(git-repository-file-group repository-path ref)
       (parameterize ([current-directory repository-path])
         (define diff-lines (git-diff-modified-lines ref))
         (for/list ([(file lines) (in-hash diff-lines)])
           (file-portion file lines)))]))
  (transduce files (filtering rkt-file?) #:into into-list))


(define/guard (rkt-file? portion)
  (define path (file-portion-path portion))
  (guard (path-has-extension? path #".rkt") #:else #false)
  (define content (file->string path))
  (string-prefix? content "#lang racket"))
