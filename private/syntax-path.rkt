#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [syntax-path? (-> any/c boolean?)]
  [syntax-path<=> (comparator/c syntax-path?)]
  [empty-syntax-path? (-> any/c boolean?)]
  [nonempty-syntax-path? (-> any/c boolean?)]
  [proper-syntax-path? (-> any/c boolean?)]
  [empty-syntax-path syntax-path?]
  [syntax-path (-> (sequence/c exact-nonnegative-integer?) syntax-path?)]
  [syntax-path-elements (-> syntax-path? (treelist/c exact-nonnegative-integer?))]
  [syntax-path-parent (-> nonempty-syntax-path? syntax-path?)]
  [syntax-path-next-neighbor (-> syntax-path? (or/c syntax-path? #false))]
  [syntax-path-last-element (-> nonempty-syntax-path? exact-nonnegative-integer?)]
  [syntax-path-add (-> syntax-path? exact-nonnegative-integer? syntax-path?)]
  [syntax-path-remove-prefix (-> syntax-path? syntax-path? syntax-path?)]
  [syntax-path-neighbors? (-> syntax-path? syntax-path? boolean?)]
  [syntax-path->string (-> syntax-path? string?)]
  [string->syntax-path (-> string? syntax-path?)]
  [syntax-ref (-> syntax? syntax-path? syntax?)]
  [syntax-set (-> syntax? syntax-path? syntax? syntax?)]
  [syntax-remove-splice
   (-> syntax? (and/c proper-syntax-path? nonempty-syntax-path?) exact-nonnegative-integer? syntax?)]
  [syntax-insert-splice
   (-> syntax? (and/c proper-syntax-path? nonempty-syntax-path?) (sequence/c syntax?) syntax?)]
  [syntax-label-paths (-> syntax? symbol? syntax?)]))


(require (for-syntax racket/base
                     racket/list
                     racket/match
                     racket/sequence
                     racket/syntax)
         data/order
         guard
         racket/sequence
         racket/string
         racket/struct
         racket/treelist
         racket/list
         racket/match
         racket/vector
         rebellion/base/comparator
         rebellion/type/singleton
         resyntax/private/matching-comparator
         syntax/parse/define)


(module+ test
  (require (submod "..")
           racket/syntax
           rackunit))


;@----------------------------------------------------------------------------------------------------

(struct syntax-path (elements)
  #:transparent
  #:sealed
  #:guard (λ (elements _) (sequence->treelist elements)))


(define empty-syntax-path (syntax-path (treelist)))


(define (empty-syntax-path? v)
  (and (syntax-path? v) (treelist-empty? (syntax-path-elements v))))


(module+ test
  (test-case "empty-syntax-path?"
    (check-true (empty-syntax-path? empty-syntax-path))
    (check-false (empty-syntax-path? (syntax-path (list 0))))
    (check-false (empty-syntax-path? 42))))


(define (nonempty-syntax-path? v)
  (and (syntax-path? v) (not (treelist-empty? (syntax-path-elements v)))))


(module+ test
  (test-case "nonempty-syntax-path?"
    (check-false (nonempty-syntax-path? empty-syntax-path))
    (check-true (nonempty-syntax-path? (syntax-path (list 0))))
    (check-false (nonempty-syntax-path? 42))))


(define (proper-syntax-path? v)
  (and (syntax-path? v)
       (for/and ([elem (in-treelist (syntax-path-elements v))])
         (exact-nonnegative-integer? elem))))


; TODO: add tests for proper-syntax-path?


(define (syntax-path-add path element)
  (syntax-path (treelist-add (syntax-path-elements path) element)))


(module+ test
  (test-case "syntax-path-add"
    (check-equal? (syntax-path-add empty-syntax-path 0) (syntax-path (list 0)))
    (check-equal? (syntax-path-add (syntax-path (list 0)) 1)
                  (syntax-path (list 0 1)))))


(define (syntax-path-parent path)
  (syntax-path (treelist-drop-right (syntax-path-elements path) 1)))


(module+ test
  (test-case "syntax-path-parent"
    (check-equal? (syntax-path-parent (syntax-path (list 0))) empty-syntax-path)
    (check-equal? (syntax-path-parent (syntax-path (list 0 1)))
                  (syntax-path (list 0)))
    (check-exn exn:fail:contract? (λ () (syntax-path-parent empty-syntax-path)))
    (check-exn #rx"expected: nonempty-syntax-path?" (λ () (syntax-path-parent empty-syntax-path)))))


(define/guard (syntax-path-next-neighbor path)
  (define elements (syntax-path-elements path))
  (guard (not (treelist-empty? elements)) #:else #false)
  (define parent-elems (treelist-drop-right elements 1))
  (define i (treelist-last elements))
  (syntax-path (treelist-add parent-elems (add1 i))))


(module+ test
  (test-case "syntax-path-next-neighbor"

    (test-case "empty path"
      (check-false (syntax-path-next-neighbor empty-syntax-path)))

    (test-case "first child"
      (define path (syntax-path (list 0)))
      (define expected (syntax-path (list 1)))
      (check-equal? (syntax-path-next-neighbor path) expected))

    (test-case "nth child"
      (define path (syntax-path (list 42)))
      (define expected (syntax-path (list 43)))
      (check-equal? (syntax-path-next-neighbor path) expected))

    (test-case "nested list child"
      (define path (syntax-path (list 1 2 3 42)))
      (define expected (syntax-path (list 1 2 3 43)))
      (check-equal? (syntax-path-next-neighbor path) expected))

    ; TODO: handle non-list element children
    (void)))


(define (syntax-path-remove-prefix path prefix)
  (define elems (syntax-path-elements path))
  (define prefix-elems (syntax-path-elements prefix))
  (unless (>= (treelist-length elems) (treelist-length prefix-elems))
    (raise-arguments-error
     'syntax-path-remove-prefix "path is shorter than prefix" "path" path "prefix" prefix))
  (define elems-up-to (treelist-take elems (treelist-length prefix-elems)))
  (unless (equal? elems-up-to prefix-elems)
    (raise-arguments-error
     'syntax-path-remove-prefix "path does not start with given prefix" "path" path "prefix" prefix))
  (syntax-path (treelist-drop elems (treelist-length prefix-elems))))


(module+ test
  (test-case "syntax-path-remove-prefix"

    (test-case "remove empty"
      (define path (syntax-path (list 1 2 3)))
      (check-equal? (syntax-path-remove-prefix path empty-syntax-path) path))

    (test-case "remove one elem"
      (define path (syntax-path (list 1 2 3)))
      (define prefix (syntax-path (list 1)))
      (define expected (syntax-path (list 2 3)))
      (check-equal? (syntax-path-remove-prefix path prefix) expected))

    (test-case "remove multiple elems"
      (define path (syntax-path (list 1 2 3 4 5)))
      (define prefix (syntax-path (list 1 2 3)))
      (define expected (syntax-path (list 4 5)))
      (check-equal? (syntax-path-remove-prefix path prefix) expected))))


(define (syntax-path-last-element path)
  (treelist-last (syntax-path-elements path)))


(module+ test
  (test-case "syntax-path-last-element"
    (check-equal? (syntax-path-last-element (syntax-path (list 0))) 0)
    (check-equal? (syntax-path-last-element (syntax-path (list 0 1)))
                  1)
    (check-exn exn:fail:contract? (λ () (syntax-path-last-element empty-syntax-path)))
    (check-exn #rx"expected: nonempty-syntax-path?"
               (λ () (syntax-path-last-element empty-syntax-path)))))


(define (syntax-path-neighbors? leading-path trailing-path)
  (and (nonempty-syntax-path? leading-path)
       (nonempty-syntax-path? trailing-path)
       (equal? (syntax-path-parent leading-path) (syntax-path-parent trailing-path))
       (syntax-path-element-neighbors?
        (syntax-path-last-element leading-path) (syntax-path-last-element trailing-path))))


(define (syntax-path-element-neighbors? leading trailing)
  (and (exact-nonnegative-integer? leading)
       (exact-nonnegative-integer? trailing)
       (equal? leading (sub1 trailing))))


(define (syntax-path->string path)
  (string-join
   (for/list ([elem (in-treelist (syntax-path-elements path))])
     (number->string elem))
   "/"
   #:before-first "/"))


(module+ test
  (test-case "syntax-path->string"
    (test-case "empty path"
      (check-equal? (syntax-path->string empty-syntax-path) "/"))
    
    (test-case "single element"
      (check-equal? (syntax-path->string (syntax-path (list 0))) "/0"))
    
    (test-case "multiple elements"
      (check-equal? (syntax-path->string (syntax-path (list 0 1 2))) "/0/1/2"))
    
    (test-case "large numbers"
      (check-equal? (syntax-path->string (syntax-path (list 42 99 1000))) "/42/99/1000"))))


(define (string->syntax-path str)
  (unless (string-prefix? str "/")
    (raise-arguments-error
     'string->syntax-path
     "syntax path string must start with /"
     "given" str))
  (when (and (> (string-length str) 1) (string-suffix? str "/"))
    (raise-arguments-error
     'string->syntax-path
     "syntax path string must not end with / (except for root path)"
     "given" str))
  (if (equal? str "/")
      empty-syntax-path
      (let* ([parts (string-split (substring str 1) "/")]
             [numbers (for/list ([part (in-list parts)])
                        (define num (string->number part))
                        (unless (and num (exact-nonnegative-integer? num))
                          (raise-arguments-error
                           'string->syntax-path
                           "syntax path string contains invalid element (must be nonnegative integer)"
                           "given" str
                           "invalid element" part))
                        num)])
        (syntax-path numbers))))


(module+ test
  (test-case "string->syntax-path"
    (test-case "empty path"
      (check-equal? (string->syntax-path "/") empty-syntax-path))
    
    (test-case "single element"
      (check-equal? (string->syntax-path "/0") (syntax-path (list 0))))
    
    (test-case "multiple elements"
      (check-equal? (string->syntax-path "/0/1/2") (syntax-path (list 0 1 2))))
    
    (test-case "large numbers"
      (check-equal? (string->syntax-path "/42/99/1000") (syntax-path (list 42 99 1000))))
    
    (test-case "error on missing leading slash"
      (define input "0/1/2")
      (define thrown
        (with-handlers ([(λ (_) #true) values])
          (string->syntax-path input)
          #false))
      (check-pred exn:fail:contract? thrown)
      (check-regexp-match #rx"string->syntax-path:" (exn-message thrown))
      (check-regexp-match #rx"given: \"0/1/2\"" (exn-message thrown))
      (check-regexp-match #rx"syntax path string must start with /" (exn-message thrown)))
    
    (test-case "error on trailing slash"
      (define input "/0/1/")
      (define thrown
        (with-handlers ([(λ (_) #true) values])
          (string->syntax-path input)
          #false))
      (check-pred exn:fail:contract? thrown)
      (check-regexp-match #rx"string->syntax-path:" (exn-message thrown))
      (check-regexp-match #rx"given: \"/0/1/\"" (exn-message thrown))
      (check-regexp-match #rx"syntax path string must not end with /" (exn-message thrown)))
    
    (test-case "error on invalid element"
      (define input "/0/abc/2")
      (define thrown
        (with-handlers ([(λ (_) #true) values])
          (string->syntax-path input)
          #false))
      (check-pred exn:fail:contract? thrown)
      (check-regexp-match #rx"string->syntax-path:" (exn-message thrown))
      (check-regexp-match #rx"given: \"/0/abc/2\"" (exn-message thrown))
      (check-regexp-match #rx"syntax path string contains invalid element" (exn-message thrown))
      (check-regexp-match #rx"invalid element: \"abc\"" (exn-message thrown)))
    
    (test-case "error on negative number"
      (define input "/0/-1/2")
      (define thrown
        (with-handlers ([(λ (_) #true) values])
          (string->syntax-path input)
          #false))
      (check-pred exn:fail:contract? thrown)
      (check-regexp-match #rx"string->syntax-path:" (exn-message thrown))
      (check-regexp-match #rx"given: \"/0/-1/2\"" (exn-message thrown))
      (check-regexp-match #rx"syntax path string contains invalid element" (exn-message thrown))
      (check-regexp-match #rx"invalid element: \"-1\"" (exn-message thrown)))
    
    (test-case "error on float"
      (define input "/0/1.5/2")
      (define thrown
        (with-handlers ([(λ (_) #true) values])
          (string->syntax-path input)
          #false))
      (check-pred exn:fail:contract? thrown)
      (check-regexp-match #rx"string->syntax-path:" (exn-message thrown))
      (check-regexp-match #rx"given: \"/0/1.5/2\"" (exn-message thrown))
      (check-regexp-match #rx"syntax path string contains invalid element" (exn-message thrown))
      (check-regexp-match #rx"invalid element: \"1.5\"" (exn-message thrown)))))


(module+ test
  (test-case "round-trip conversion"
    (test-case "empty path"
      (check-equal? (string->syntax-path (syntax-path->string empty-syntax-path))
                    empty-syntax-path))
    
    (test-case "single element path"
      (define path (syntax-path (list 5)))
      (check-equal? (string->syntax-path (syntax-path->string path)) path))
    
    (test-case "multiple element path"
      (define path (syntax-path (list 1 2 3 4 5)))
      (check-equal? (string->syntax-path (syntax-path->string path)) path))
    
    (test-case "path with large numbers"
      (define path (syntax-path (list 0 100 999 1234567)))
      (check-equal? (string->syntax-path (syntax-path->string path)) path))))


(define (syntax-ref init-stx path)
  (define result
    (for/fold ([stx init-stx])
              ([element (in-treelist (syntax-path-elements path))])
      (define unwrapped (syntax-e stx))
      (cond
        ; Handle improper lists - flatten them so the tail is treated as the last element
        [(and (pair? unwrapped) (not (list? unwrapped)))
         (define flattened (flatten-improper-list unwrapped))
         (unless (< element (length flattened))
           (raise-arguments-error 'syntax-ref
                                  "syntax path is inconsistent with the syntax's shape"
                                  "syntax" init-stx
                                  "path" path
                                  "malformed subform" stx
                                  "path element" element))
         (list-ref flattened element)]
        ; Handle proper lists
        [(list? unwrapped)
         (unless (< element (length unwrapped))
           (raise-arguments-error 'syntax-ref
                                  "syntax path is inconsistent with the syntax's shape"
                                  "syntax" init-stx
                                  "path" path
                                  "malformed subform" stx
                                  "path element" element))
         (list-ref unwrapped element)]
        ; Handle vectors
        [(vector? unwrapped)
         (unless (< element (vector-length unwrapped))
           (raise-arguments-error 'syntax-ref
                                  "syntax path is inconsistent with the syntax's shape"
                                  "syntax" init-stx
                                  "path" path
                                  "malformed subform" stx
                                  "path element" element))
         (vector-ref unwrapped element)]
        ; Handle boxes - treat as single-element list
        [(box? unwrapped)
         (unless (zero? element)
           (raise-arguments-error 'syntax-ref
                                  "syntax path is inconsistent with the syntax's shape"
                                  "syntax" init-stx
                                  "path" path
                                  "malformed subform" stx
                                  "path element" element))
         (unbox unwrapped)]
        ; Handle prefab structs - treat as list of fields
        [(prefab-struct? unwrapped)
         (define fields (struct->list unwrapped))
         (unless (< element (length fields))
           (raise-arguments-error 'syntax-ref
                                  "syntax path is inconsistent with the syntax's shape"
                                  "syntax" init-stx
                                  "path" path
                                  "malformed subform" stx
                                  "path element" element))
         (list-ref fields element)]
        ; Hashes are unsupported
        [(hash? unwrapped)
         (raise-arguments-error 'syntax-ref
                                "syntax paths cannot traverse hash datums"
                                "syntax" init-stx
                                "path" path
                                "hash subform" stx)]
        ; Other datums don't have children
        [else
         (raise-arguments-error 'syntax-ref
                                "syntax path is inconsistent with the syntax's shape"
                                "syntax" init-stx
                                "path" path
                                "malformed subform" stx
                                "path element" element)])))
  (when (or (pair? result) (empty? result))
    (raise-arguments-error 'syntax-ref
                           "syntax path refers to a non-syntax component"
                           "syntax" init-stx
                           "path" path
                           "component" result))
  result)


; Helper function to check if a structure is truly improper (has an atom tail)
; vs. dotted syntax (wraps proper lists with syntax objects)
(define (has-improper-tail? lst)
  (cond
    [(null? lst) #false]
    [(pair? lst)
     (define cdr-elem (cdr lst))
     (cond
       [(syntax? cdr-elem)
        (define unwrapped (syntax-e cdr-elem))
        (cond
          [(list? unwrapped) #false]  ; Dotted syntax with proper list
          [(pair? unwrapped) (has-improper-tail? unwrapped)]  ; Keep checking
          [else #true])]  ; Atom tail
       [(null? cdr-elem) #false]
       [(pair? cdr-elem) (has-improper-tail? cdr-elem)]
       [else #true])]  ; Non-syntax atom tail
    [else #false]))


; Helper function to flatten improper lists and dotted syntax
; e.g., '(a b . c) becomes '(a b c)
; e.g., '(a . (b . (c . d))) becomes '(a b c d)
; e.g., pair with syntax object cdr like (a . #'(b c)) becomes (a b c)
(define (flatten-improper-list lst)
  (cond
    [(null? lst) '()]
    [(pair? lst)
     (define car-elem (car lst))
     (define cdr-elem (cdr lst))
     ; If cdr is a syntax object, unwrap it to handle dotted syntax
     (define cdr-unwrapped
       (if (syntax? cdr-elem)
           (syntax-e cdr-elem)
           cdr-elem))
     ; Check if the unwrapped cdr is a list
     (cond
       [(list? cdr-unwrapped)
        ; It's a proper list, so append it (flattens dotted syntax)
        (cons car-elem cdr-unwrapped)]
       [(pair? cdr-unwrapped)
        ; It's an improper list, recursively flatten
        (cons car-elem (flatten-improper-list cdr-unwrapped))]
       [else
        ; It's an atom (the improper tail)
        (list car-elem cdr-elem)])]
    [else (list lst)]))


(module+ test
  (test-case "syntax-ref"

    (test-case "empty path"
      (define stx #'a)
      (define actual (syntax-ref stx empty-syntax-path))
      (check-equal? actual stx))

    (test-case "list element path"
      (define stx #'(a b c))
      (define actual (syntax-ref stx (syntax-path (list 1))))
      (check-equal? (syntax->datum actual) 'b))

    (test-case "improper list - dotted syntax treated as proper list"
      ; #'(a . (b c)) is dotted syntax equivalent to #'(a b c)
      ; After flattening, we have: (a b c), so index 1 gives us b
      (define stx #'(a . (b c)))
      (define actual (syntax-ref stx (syntax-path (list 1))))
      (check-equal? (syntax->datum actual) 'b))

    (test-case "improper list with atom tail"
      (define stx #'(a b . c))
      (define actual (syntax-ref stx (syntax-path (list 2))))
      (check-equal? (syntax->datum actual) 'c))

    (test-case "vector element path"
      (define stx #'#[a b c])
      (define actual (syntax-ref stx (syntax-path (list 1))))
      (check-equal? (syntax->datum actual) 'b))

    (test-case "box element path"
      (define stx #'#&a)
      (define actual (syntax-ref stx (syntax-path (list 0))))
      (check-equal? (syntax->datum actual) 'a))

    (test-case "hash value path - should error"
      (define stx #'#hash((a . 1) (b . 2) (c . 3)))
      (define thrown
        (with-handlers ([(λ (_) #true) values])
          (syntax-ref stx (syntax-path (list 0)))
          #false))
      (check-pred exn:fail:contract? thrown)
      (check-regexp-match #rx"syntax-ref:" (exn-message thrown))
      (check-regexp-match #rx"cannot traverse hash" (exn-message thrown)))

    (test-case "nested list path"
      (define stx #'(a b c (m (FOO x y z) n)))
      (define actual (syntax-ref stx (syntax-path (list 3 1 0))))
      (check-equal? (syntax->datum actual) 'FOO))

    (test-case "improper list with dotted syntax - nested access"
      ; #'(a b . (c FOO e)) is dotted syntax equivalent to #'(a b c FOO e)
      ; After flattening: (a b c FOO e), so index 3 gives us FOO
      (define stx #'(a b . (c FOO e)))
      (define actual (syntax-ref stx (syntax-path (list 3))))
      (check-equal? (syntax->datum actual) 'FOO))

    (test-case "prefab struct field path"
      (define stx #'#s(point 10 20))
      (define actual (syntax-ref stx (syntax-path (list 0))))
      (check-equal? (syntax->datum actual) 10))

    (test-case "list element on syntax that's too short"
      (define stx #'(a b c))
      (define thrown
        (with-handlers ([(λ (_) #true) values])
          (syntax-ref stx (syntax-path (list 10)))
          #false))
      (check-pred exn:fail:contract? thrown)
      (check-regexp-match #rx"syntax-ref:" (exn-message thrown))
      (check-regexp-match #rx"path is inconsistent" (exn-message thrown)))))






(define (syntax-set init-stx path new-subform)
  (let loop ([stx init-stx] [elements (syntax-path-elements path)])
    (guarded-block
      (guard (not (treelist-empty? elements)) #:else new-subform)
      (define i (treelist-first elements))
      (define remaining-elements (treelist-rest elements))
      (define unwrapped (syntax-e stx))
      (cond
        ; Handle improper lists and dotted syntax - flatten, update, and potentially reconstruct
        ; Note: Dotted syntax like #'(a . (b c)) flattens to (a b c) and stays flattened.
        ; Only truly improper lists with atom tails need to be reconstructed.
        [(and (pair? unwrapped) (not (list? unwrapped)))
         (define is-truly-improper (has-improper-tail? unwrapped))
         (define flattened (flatten-improper-list unwrapped))
         (define updated-elem (loop (list-ref flattened i) remaining-elements))
         (define updated-flattened (list-set flattened i updated-elem))
         ; Check if the original had a true improper tail (atom) vs dotted syntax (wraps lists)
         (define updated-datum
           (if is-truly-improper
               ; Truly improper list, reconstruct the structure
               (unflatten-improper-list updated-flattened unwrapped)
               ; Dotted syntax that flattened to proper list, keep it proper
               updated-flattened))
         (datum->syntax stx updated-datum stx stx)]
        ; Handle proper lists
        [(list? unwrapped)
         (define updated-child (loop (list-ref unwrapped i) remaining-elements))
         (define updated-datum (list-set unwrapped i updated-child))
         (datum->syntax stx updated-datum stx stx)]
        ; Handle vectors
        [(vector? unwrapped)
         (define updated-child (loop (vector-ref unwrapped i) remaining-elements))
         (define updated-vector (vector-copy unwrapped))
         (vector-set! updated-vector i updated-child)
         (datum->syntax stx updated-vector stx stx)]
        ; Handle boxes - treat as single-element list
        [(box? unwrapped)
         (define updated-child (loop (unbox unwrapped) remaining-elements))
         (define updated-datum (box-immutable updated-child))
         (datum->syntax stx updated-datum stx stx)]
        ; Handle prefab structs - treat as list of fields
        [(prefab-struct? unwrapped)
         (define key (prefab-struct-key unwrapped))
         (define fields (struct->list unwrapped))
         (define updated-child (loop (list-ref fields i) remaining-elements))
         (define updated-fields (list-set fields i updated-child))
         (define updated-datum (apply make-prefab-struct key updated-fields))
         (datum->syntax stx updated-datum stx stx)]
        ; Hashes are unsupported
        [(hash? unwrapped)
         (raise-arguments-error 'syntax-set
                                "syntax paths cannot traverse hash datums"
                                "syntax" init-stx
                                "path" path)]
        ; Other datums don't have children
        [else
         (raise-arguments-error 'syntax-set
                                "syntax path is inconsistent with the syntax's shape"
                                "syntax" init-stx
                                "path" path
                                "malformed subform" stx
                                "path element" i)]))))


; Helper function to unflatten an improper list, preserving the original structure
; Takes the flattened list and the original improper list structure
(define (unflatten-improper-list flattened original)
  (cond
    [(null? flattened) '()]
    [(and (pair? original) (not (list? original)))
     ; Reconstruct the improper structure
     (let unflatten-with-tail ([flat flattened] [orig original])
       (cond
         [(or (null? (cdr flat)) (not (pair? orig)))
          ; Base case: last element or reached non-pair in original
          (car flat)]
         [else
          ; Recursive case: continue building the improper list
          (cons (car flat) (unflatten-with-tail (cdr flat) (cdr orig)))]))]
    [else 
     ; For proper lists or single elements, return as-is
     flattened]))


(module+ test
  (test-case "unflatten-improper-list"
    
    (test-case "empty list"
      (check-equal? (unflatten-improper-list '() '()) '()))
    
    (test-case "proper list stays proper"
      (define flat '(a b c))
      (define orig '(x y z))  ; proper list
      (check-equal? (unflatten-improper-list flat orig) '(a b c)))
    
    (test-case "reconstruct simple improper list"
      (define flat '(a b c))
      (define orig '(x y . z))  ; improper list structure
      (define result (unflatten-improper-list flat orig))
      (check-equal? result '(a b . c)))
    
    (test-case "reconstruct nested improper list"
      (define flat '(a b c d))
      (define orig '(w . (x . (y . z))))  ; nested improper structure
      (define result (unflatten-improper-list flat orig))
      (check-equal? result '(a . (b . (c . d)))))))


(module+ test
  (test-case "syntax-set"

    (define new-subform #'FOO)

    (test-case "empty path"
      (define stx #'a)
      (define actual (syntax-set stx empty-syntax-path new-subform))
      (check-equal? actual new-subform))

    (test-case "list element path"
      (define stx #'(a b c))
      (define actual (syntax-set stx (syntax-path (list 1)) new-subform))
      (check-equal? (syntax->datum actual) '(a FOO c)))

    (test-case "dotted syntax - replacing middle element"
      ; #'(a . (b c)) is equivalent to #'(a b c), so index 1 is b
      (define stx #'(a . (b c)))
      (define actual (syntax-set stx (syntax-path (list 1)) new-subform))
      (check-equal? (syntax->datum actual) '(a FOO c)))

    (test-case "improper list with atom tail"
      (define stx #'(a b . c))
      (define actual (syntax-set stx (syntax-path (list 2)) new-subform))
      (check-equal? (syntax->datum actual) '(a b . FOO)))

    (test-case "vector element path"
      (define stx #'#[a b c])
      (define actual (syntax-set stx (syntax-path (list 1)) new-subform))
      (check-equal? (syntax->datum actual) '#[a FOO c]))

    (test-case "box element path"
      (define stx #'#&a)
      (define actual (syntax-set stx (syntax-path (list 0)) new-subform))
      (check-equal? (syntax->datum actual) '#&FOO))

    (test-case "hash value path - should error"
      (define stx #'#hash((a . 1) (b . 2) (c . 3)))
      (check-exn exn:fail:contract?
                 (λ () (syntax-set stx (syntax-path (list 0)) new-subform))))

    (test-case "prefab field path"
      (define stx #'#s(point 1 2))
      (define actual (syntax-set stx (syntax-path (list 0)) new-subform))
      (check-equal? (syntax->datum actual) '#s(point FOO 2)))

    (test-case "nested list path"
      (define stx #'(a b c (m (OLD x y z) n)))
      (define actual (syntax-set stx (syntax-path (list 3 1 0)) new-subform))
      (check-equal? (syntax->datum actual) '(a b c (m (FOO x y z) n))))

    (test-case "dotted syntax - replacing in flattened list"
      ; #'(a b . (c OLD e)) is equivalent to #'(a b c OLD e)
      ; Index 3 is OLD in the flattened list
      (define stx #'(a b . (c OLD e)))
      (define actual (syntax-set stx (syntax-path (list 3)) new-subform))
      (check-equal? (syntax->datum actual) '(a b c FOO e)))))






(define/guard (syntax-remove-splice stx path children-count)
  (guard (positive? children-count) #:else stx)
  (define parent (syntax-ref stx (syntax-path-parent path)))
  (define updated
    (list-remove-splice (syntax->list parent) (syntax-path-last-element path) children-count))
  (define new-parent (datum->syntax parent updated parent parent))
  (syntax-set stx (syntax-path-parent path) new-parent))


(module+ test
  (test-case "syntax-remove-splice"
    (test-case "empty splice"
      (define stx #'(a b c))
      (define actual (syntax-remove-splice stx (syntax-path (list 1)) 0))
      (check-eq? actual stx))

    (test-case "singleton splice"
      (define stx #'(a b c))
      (define actual (syntax-remove-splice stx (syntax-path (list 1)) 1))
      (check-equal? (syntax->datum actual) '(a c)))

    (test-case "remove multiple elements"
      (define stx #'(a b c d e))
      (define actual (syntax-remove-splice stx (syntax-path (list 1)) 2))
      (check-equal? (syntax->datum actual) '(a d e)))

    (test-case "remove from start"
      (define stx #'(a b c d))
      (define actual (syntax-remove-splice stx (syntax-path (list 0)) 2))
      (check-equal? (syntax->datum actual) '(c d)))

    (test-case "remove from end"
      (define stx #'(a b c d))
      (define actual (syntax-remove-splice stx (syntax-path (list 2)) 2))
      (check-equal? (syntax->datum actual) '(a b)))

    (test-case "remove more than available - should error"
      (define stx #'(a b c))
      (check-exn exn:fail?
                 (λ () (syntax-remove-splice stx (syntax-path (list 1)) 10))))

    (test-case "nested list removal"
      (define stx #'(a (x y z) b))
      (define actual (syntax-remove-splice stx (syntax-path (list 1 1)) 1))
      (check-equal? (syntax->datum actual) '(a (x z) b)))

    (test-case "remove from empty list - should error"
      (define stx #'())
      (check-exn exn:fail?
                 (λ () (syntax-remove-splice stx (syntax-path (list 0)) 1))))

    (test-case "error on empty path with non-zero count"
      (define stx #'(a b c))
      (check-exn exn:fail:contract?
                 (λ () (syntax-remove-splice stx empty-syntax-path 1))))

    (test-case "error on non-list target"
      (define stx #'#(a b c))
      (check-exn exn:fail:contract?
                 (λ () (syntax-remove-splice stx (syntax-path (list 0)) 1))))))


(define/guard (syntax-insert-splice stx path new-children)
  (guard (not (empty? new-children)) #:else stx)
  (define parent (syntax-ref stx (syntax-path-parent path)))
  (define updated
    (list-insert-splice (syntax->list parent) (syntax-path-last-element path) new-children))
  (define new-parent (datum->syntax parent updated parent parent))
  (syntax-set stx (syntax-path-parent path) new-parent))


(module+ test
  (test-case "syntax-insert-splice"
    (test-case "empty splice"
      (define stx #'(a b c))
      (define actual (syntax-insert-splice stx (syntax-path (list 1)) '()))
      (check-eq? actual stx))

    (test-case "singleton splice"
      (define stx #'(a b c))
      (define actual (syntax-insert-splice stx (syntax-path (list 1)) (list #'foo)))
      (check-equal? (syntax->datum actual) '(a foo b c)))

    (test-case "multiple element splice"
      (define stx #'(a b c))
      (define actual (syntax-insert-splice stx (syntax-path (list 1)) (list #'foo #'bar)))
      (check-equal? (syntax->datum actual) '(a foo bar b c)))

    (test-case "insert at start"
      (define stx #'(a b c))
      (define actual (syntax-insert-splice stx (syntax-path (list 0)) (list #'x #'y)))
      (check-equal? (syntax->datum actual) '(x y a b c)))

    (test-case "insert at end"
      (define stx #'(a b c))
      (define actual (syntax-insert-splice stx (syntax-path (list 3)) (list #'x #'y)))
      (check-equal? (syntax->datum actual) '(a b c x y)))

    (test-case "insert beyond end"
      (define stx #'(a b c))
      (check-exn exn:fail:contract?
                 (λ () (syntax-insert-splice stx (syntax-path (list 10)) (list #'x #'y)))))

    (test-case "nested list insertion"
      (define stx #'(a (x z) b))
      (define actual (syntax-insert-splice stx (syntax-path (list 1 1)) (list #'y)))
      (check-equal? (syntax->datum actual) '(a (x y z) b)))

    (test-case "insert into empty list"
      (define stx #'())
      (define actual (syntax-insert-splice stx (syntax-path (list 0)) (list #'x)))
      (check-equal? (syntax->datum actual) '(x)))

    (test-case "error on empty path"
      (define stx #'(a b c))
      (check-exn exn:fail:contract?
                 (λ () (syntax-insert-splice stx empty-syntax-path (list #'x)))))

    (test-case "error on non-list target"
      (define stx #'#(a b c))
      (check-exn exn:fail:contract?
                 (λ () (syntax-insert-splice stx (syntax-path (list 0)) (list #'x)))))))


(define (list-remove-splice lst i splice-length)
  (append (take lst i) (drop lst (+ i splice-length))))


(module+ test
  (test-case "list-remove-splice"
    (test-case "remove from middle"
      (define lst '(a b c d e))
      (define actual (list-remove-splice lst 2 2))
      (check-equal? actual '(a b e)))
    
    (test-case "remove from beginning"
      (define lst '(a b c d))
      (define actual (list-remove-splice lst 0 2))
      (check-equal? actual '(c d)))
    
    (test-case "remove from end"
      (define lst '(a b c d))
      (define actual (list-remove-splice lst 2 2))
      (check-equal? actual '(a b)))
    
    (test-case "remove single element"
      (define lst '(a b c))
      (define actual (list-remove-splice lst 1 1))
      (check-equal? actual '(a c)))
    
    (test-case "remove zero elements"
      (define lst '(a b c))
      (define actual (list-remove-splice lst 1 0))
      (check-equal? actual '(a b c)))
    
    (test-case "remove all elements"
      (define lst '(a b c))
      (define actual (list-remove-splice lst 0 3))
      (check-equal? actual '()))
    
    (test-case "remove from single-element list"
      (define lst '(a))
      (define actual (list-remove-splice lst 0 1))
      (check-equal? actual '()))
    
    (test-case "remove from empty list"
      (define lst '())
      (define actual (list-remove-splice lst 0 0))
      (check-equal? actual '()))
    
    (test-case "error on index out of bounds"
      (define lst '(a b c))
      (check-exn exn:fail:contract?
                 (λ () (list-remove-splice lst 5 1))))
    
    (test-case "error on splice length too large"
      (define lst '(a b c))
      (check-exn exn:fail:contract?
                 (λ () (list-remove-splice lst 1 5))))))


(define (list-insert-splice lst i splice)
  (append (take lst i) splice (drop lst i)))


(module+ test
  (test-case "list-insert-splice"
    (test-case "insert in middle"
      (define lst '(a b c))
      (define actual (list-insert-splice lst 1 '(x y)))
      (check-equal? actual '(a x y b c)))
    
    (test-case "insert at beginning"
      (define lst '(a b c))
      (define actual (list-insert-splice lst 0 '(x y)))
      (check-equal? actual '(x y a b c)))
    
    (test-case "insert at end"
      (define lst '(a b c))
      (define actual (list-insert-splice lst 3 '(x y)))
      (check-equal? actual '(a b c x y)))
    
    (test-case "insert single element"
      (define lst '(a b c))
      (define actual (list-insert-splice lst 1 '(x)))
      (check-equal? actual '(a x b c)))
    
    (test-case "insert empty splice"
      (define lst '(a b c))
      (define actual (list-insert-splice lst 1 '()))
      (check-equal? actual '(a b c)))
    
    (test-case "insert into empty list"
      (define lst '())
      (define actual (list-insert-splice lst 0 '(x y)))
      (check-equal? actual '(x y)))
    
    (test-case "insert into single-element list"
      (define lst '(a))
      (define actual (list-insert-splice lst 0 '(x y)))
      (check-equal? actual '(x y a)))
    
    (test-case "insert after single element"
      (define lst '(a))
      (define actual (list-insert-splice lst 1 '(x y)))
      (check-equal? actual '(a x y)))
    
    (test-case "error on index out of bounds"
      (define lst '(a b c))
      (check-exn exn:fail:contract?
                 (λ () (list-insert-splice lst 5 '(x y)))))))


(define (syntax-label-paths stx property-name)
  (let loop ([stx stx] [path (syntax-path '())])
    (define unwrapped (syntax-e stx))
    (define datum-with-children-labeled
      (match unwrapped
        ; Handle proper lists
        [(list children ...)
         (for/list ([child (in-list children)]
                    [i (in-naturals)])
           (loop child (syntax-path-add path i)))]
        ; Handle improper lists and dotted syntax
        ; We flatten for path indexing but preserve structure in output
        [(cons _ _) #:when (not (list? unwrapped))
         (define flattened-children (flatten-improper-list unwrapped))
         (define labeled-flat
           (for/list ([child (in-list flattened-children)]
                      [i (in-naturals)])
             (loop child (syntax-path-add path i))))
         ; Reconstruct the ORIGINAL structure with labeled children
         ; This is tricky: we need to preserve dotted syntax
         (let rebuild ([orig unwrapped] [flat labeled-flat])
           (cond
             [(pair? orig)
              (define car-labeled (car flat))
              (define cdr-elem (cdr orig))
              (cond
                [(syntax? cdr-elem)
                 (define cdr-unwrapped (syntax-e cdr-elem))
                 (cond
                   [(list? cdr-unwrapped)
                    ; Dotted syntax - cdr wraps a list, so rest of flat goes there
                    (cons car-labeled (cdr flat))]
                   [(pair? cdr-unwrapped)
                    ; Nested improper - recurse
                    (cons car-labeled (rebuild cdr-unwrapped (cdr flat)))]
                   [else
                    ; Atom tail - single element
                    (cons car-labeled (car (cdr flat)))])]
                [(pair? cdr-elem)
                 ; Direct pair - recurse
                 (cons car-labeled (rebuild cdr-elem (cdr flat)))]
                [else
                 ; Atom tail
                 (cons car-labeled (car (cdr flat)))])]
             [else orig]))]
        ; Handle vectors - treat like lists
        [(vector children ...)
         (for/vector ([child (in-list children)]
                      [i (in-naturals)])
           (loop child (syntax-path-add path i)))]
        ; Handle boxes - treat as single-element list (index 0)
        [(box child)
         (box-immutable (loop child (syntax-path-add path 0)))]
        ; Handle hashes - skip them, return original
        [(? hash? ht) ht]
        ; Handle prefab structs - treat as list of fields
        [(? prefab-struct? s)
         (define key (prefab-struct-key s))
         (define labeled-children
           (for/list ([child (in-list (struct->list s))]
                      [i (in-naturals)])
             (loop child (syntax-path-add path i))))
         (apply make-prefab-struct key labeled-children)]
        ; Atoms have no children
        [_ stx]))
    (define stx-with-children-labeled (datum->syntax stx datum-with-children-labeled stx stx))
    (syntax-property stx-with-children-labeled property-name path)))




(module+ test
  (test-case "syntax-label-paths"
    (define stx #'(foo (a b . c) bar (baz) #(x y) #&z #s(point n m)))
    (define labeled (syntax-label-paths stx 'path))
    (check-equal? (syntax->datum labeled) (syntax->datum stx))
    (define/with-syntax (foo* (a* b* . c*) bar* (baz*) #(x* y*) #&z* #s(point n* m*)) labeled)
    (check-equal? (syntax-property #'foo* 'path) (syntax-path (treelist 0)))
    (check-equal? (syntax-property #'a* 'path) (syntax-path (treelist 1 0)))
    (check-equal? (syntax-property #'b* 'path) (syntax-path (treelist 1 1)))
    ; c is at index 2 in the flattened improper list (a b . c)
    (check-equal? (syntax-property #'c* 'path) (syntax-path (treelist 1 2)))
    (check-equal? (syntax-property #'bar* 'path) (syntax-path (treelist 2)))
    (check-equal? (syntax-property #'baz* 'path) (syntax-path (treelist 3 0)))
    ; vectors now use integer indices
    (check-equal? (syntax-property #'x* 'path) (syntax-path (treelist 4 0)))
    (check-equal? (syntax-property #'y* 'path) (syntax-path (treelist 4 1)))
    ; boxes use index 0
    (check-equal? (syntax-property #'z* 'path) (syntax-path (treelist 5 0)))
    ; prefab fields use integer indices
    (check-equal? (syntax-property #'n* 'path) (syntax-path (treelist 6 0)))
    (check-equal? (syntax-property #'m* 'path) (syntax-path (treelist 6 1)))
    (for ([id (in-syntax #'(foo* a* b* c* bar* baz* x* y* z* n* m*))])
      (define path (syntax-property id 'path))
      (check-equal? (syntax->datum (syntax-ref stx path)) (syntax->datum id)))))




(define (prefab-struct? v)
  (and (prefab-struct-key v) #true))


(define (prefab-struct-ref s i)
  (unless (prefab-struct? s)
    (raise-argument-error 'prefab-struct-ref "prefab-struct?" s))
  (list-ref (struct->list s) i))



(define datum<=>
  (make-comparator
   (λ (left right)
     (match (datum-order left right)
       ['= equivalent]
       ['> greater]
       ['< lesser]))))


(define syntax-path<=>
  (comparator-map (lexicographic-comparator natural<=>) syntax-path-elements
                  #:name 'syntax-path<=>))
