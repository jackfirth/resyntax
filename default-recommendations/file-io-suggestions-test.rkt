#lang resyntax/test


require: resyntax/default-recommendations file-io-suggestions


header:
----------------------------------------
#lang racket/base
(require racket/file)
----------------------------------------


test: "should migrate make-temporary-file with 'directory to make-temporary-directory"
- (void (make-temporary-file #:copy-from 'directory))
- (void (make-temporary-directory))


test: "should migrate make-temporary-file with template and 'directory to make-temporary-directory"
- (void (make-temporary-file "footmp~a" #:copy-from 'directory))
- (void (make-temporary-file "footmp~a" 'directory))
- (void (make-temporary-directory "footmp~a"))


test: "should migrate make-temporary-file with base-dir and 'directory to make-temporary-directory"
- (void (make-temporary-file #:base-dir #false #:copy-from 'directory))
- (void (make-temporary-directory #:base-dir #false))


test: "should not migrate make-temporary-file without 'directory to make-temporary-directory"
- (make-temporary-file #:copy-from #false)
