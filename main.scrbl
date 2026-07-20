#lang scribble/manual


@(require (for-label racket/base
                     syntax/parse))


@title{Resyntax}
@defmodule[resyntax]


Resyntax is a refactoring tool for Racket. The tool can be guided by @deftech{refactoring rules},
which are macro-like functions defined in terms of @racket[syntax-parse] that specify how to search
for and refactor different coding patterns. Resyntax comes with a standard set of refactoring rules
that improve code written in @racket[@#,hash-lang[] @#,racketmodname[racket]] or
@racket[@#,hash-lang[] @#,racketmodname[racket/base]]. For example, consider the following program:

@(racketmod
  #:file "my-program.rkt"
  racket/base

  (define (swap x y)
    (let ([t (unbox x)])
      (set-box! x (unbox y))
      (set-box! y t))))

This program uses @racket[let] unnecessarily. The @racket[let] expression can be replaced with a
@racket[define] form, reducing the indentation of the code. Resyntax is capable of detecting and
automatically fixing this issue. Running @exec{resyntax fix --file my-program.rkt} rewrites the above
to the following:

@(racketmod
  #:file "my-program.rkt"
  racket/base

  (define (swap x y)
    (define t (unbox x))
    (set-box! x (unbox y))
    (set-box! y t)))

To see a list of suggestions that Resyntax would apply, use @exec{resyntax analyze} instead of
@exec{resyntax fix}. Each suggestion includes an explanation of why the change is being recommended.


@table-of-contents[]


@include-section[(lib "resyntax/cli.scrbl")]
@include-section[(lib "resyntax/github.scrbl")]
@include-section[(lib "resyntax/refactoring-rules.scrbl")]
@include-section[(lib "resyntax/testing.scrbl")]
@include-section[(lib "resyntax/grimoire.scrbl")]
