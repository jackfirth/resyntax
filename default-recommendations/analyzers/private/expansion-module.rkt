#lang racket/base


(require racket/contract/base)


(provide
 (struct-out expansion-module)
 (contract-out
  [expanded-syntax-modules (-> syntax? (hash/c (treelist/c symbol?) expansion-module?))]))


(require racket/stream
         racket/treelist
         resyntax/private/syntax-path
         resyntax/private/syntax-traversal
         syntax/parse)


;@----------------------------------------------------------------------------------------------------


(struct expansion-module (syntax path name declaration-phase parents kind prelude)
  #:transparent
  #:guard (struct-guard/c syntax?
                          syntax-path?
                          symbol?
                          exact-nonnegative-integer?
                          (treelist/c symbol?)
                          (or/c 'module 'module*)
                          (or/c module-path? #false)))


(define (expanded-syntax-modules orig-expanded-stx)
  (define results
    (let loop ([stx (syntax-label-paths orig-expanded-stx 'expanded-path)]
               [phase 0]
               [parents (treelist)])
      (syntax-search stx
        #:literal-sets ([kernel-literals #:phase phase])
        [(module name prelude body)
         (define path (syntax-property this-syntax 'expanded-path))
         (define name-sym (syntax->datum (attribute name)))
         (define prelude-datum (syntax->datum (attribute prelude)))
         (define exp-mod
           (expansion-module this-syntax path name-sym phase parents 'module prelude-datum))
         (stream-cons exp-mod (loop (attribute body) 0 (treelist-add parents name-sym)))])))
  (for/hash ([expmod (in-stream results)])
    (define ancestry (treelist-add (expansion-module-parents expmod) (expansion-module-name expmod)))
    (values ancestry expmod)))
