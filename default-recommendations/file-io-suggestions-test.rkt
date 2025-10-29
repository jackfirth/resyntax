#lang resyntax/test


require: resyntax/default-recommendations file-io-suggestions


header:
----------------------------------------
#lang racket/base
(require racket/file)
----------------------------------------


no-change-test:
"should not migrate make-temporary-file without 'directory to make-temporary-directory"
- (make-temporary-file #:copy-from #false)
