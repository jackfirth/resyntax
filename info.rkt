#lang info


(define collection "resyntax")


(define scribblings
  (list (list "main.scrbl"
              (list 'multi-page)
              (list 'library)
              "resyntax")))


(define deps
  (list "compatibility-lib"
        "base"
        "br-parser-tools-lib"
        "brag-lib"
        "fancy-app"
        "fmt"
        "guard"
        "rackunit-lib"
        "rebellion"))


(define build-deps
  (list "racket-doc"
        "rackunit-lib"
        "scribble-lib"))


(define racket-launcher-names
  (list "resyntax"))


(define racket-launcher-libraries
  (list "cli.rkt"))
