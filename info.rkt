#lang info

(define collection "resyntax")

(define scribblings
  (list (list "main.scrbl"
              (list 'multi-page)
              (list 'library)
              "resyntax")))

(define deps
  (list "base"))

(define build-deps
  (list "racket-doc"
        "rackunit-lib"
        "scribble-lib"))
