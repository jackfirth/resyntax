#lang info


(define collection "resyntax")


(define scribblings
  (list (list "main.scrbl"
              (list 'multi-page)
              (list 'library)
              "resyntax")))


(define deps
  (list "gui-lib"
        "fancy-app"
        "rebellion"
        "base"))


(define build-deps
  (list "racket-doc"
        "rackunit-lib"
        "scribble-lib"))


(define racket-launcher-names
  (list "resyntax-github-action"))


(define racket-launcher-libraries
  (list "github-action.rkt"))
