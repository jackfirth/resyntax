#lang racket

(require resyntax/private/text-tree)


(define (oneline-yaml-entry k v)
  (text-tree (list k v) #:separators (list "" ": " "")))


(define (multiline-yaml-entry k v)
  (text-tree (list k v)
             #:separators (list "" ":\n  " "")
             #:hanging-line-prefixes (list "" "  ")))


(define (yaml-map . entries)
  (define seps (append (list "") (make-list (sub1 (length entries)) "\n") (list "")))
  (text-tree entries #:separators seps))


(define (yaml-list . items)
  (define seps (append (list "- ") (make-list (sub1 (length items)) "\n- ") (list "")))
  (text-tree items #:separators seps #:hanging-line-prefixes "  "))


(define whole-file-tree
  (yaml-map
   (oneline-yaml-entry "version" "2")
   (multiline-yaml-entry
    "updates"
    (yaml-list
     (yaml-map
      (multiline-yaml-entry "package-ecosystem" "\"github-actions\"")
      (oneline-yaml-entry "directory" "\"/\"")
      (multiline-yaml-entry "schedule" (yaml-map (oneline-yaml-entry "interval" "\"weekly\""))))))))


(displayln (text-tree-render whole-file-tree))


(define (oneline-j-tuple . items)
  (define seps
    (if (empty? items)
        (list "()")
        (append (list "(") (make-list (sub1 (length items)) ", ") (list ")"))))
  (text-tree items #:separators seps))


(define (multiline-j-tuple . items)
  (define seps (append (list "(\n    ") (make-list (sub1 (length items)) ",\n    ") (list ")")))
  (text-tree items #:separators seps #:hanging-line-prefixes "    "))


(define oneline-empty-j-block (text-tree (list) #:separators (list "{}")))
(define multiline-empty-j-block (text-tree (list) #:separators (list "{\n}")))


(define (oneline-j-block . items)
  (define seps (append (list "{ ") (make-list (sub1 (length items)) "; ") (list " }")))
  (text-tree items #:separators seps))


(define (multiline-j-block . items)
  (define seps (append (list "{\n  ") (make-list (sub1 (length items)) ";\n  ") (list ";\n}")))
  (text-tree items #:separators seps #:hanging-line-prefixes "  "))


(define (loose-j-chain . items)
  (define seps (append (list "") (make-list (sub1 (length items)) " ") (list "")))
  (text-tree items #:separators seps))


(define (tight-j-chain . items)
  (text-tree items))


(pretty-print
 (list
  "if"
  (oneline-j-tuple "x")
  (multiline-j-block
   (tight-j-chain "doSomeStuff" (multiline-j-tuple "a" "b" "c"))
   (tight-j-chain "doOtherStuff" (oneline-j-tuple)))
  "else"
  (multiline-j-block (tight-j-chain "nevermind" (oneline-j-tuple)))))


(yaml-map (oneline-yaml-entry "foo" "1") (oneline-yaml-entry "bar" "2"))
