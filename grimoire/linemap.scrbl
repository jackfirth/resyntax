#lang scribble/manual


@(require (for-label racket/base
                     racket/contract/base
                     rebellion/base/comparator
                     rebellion/base/range
                     resyntax/grimoire/linemap
                     resyntax/grimoire/source)
          scribble/example
          (submod resyntax/private/scribble-evaluator-factory doc))


@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'resyntax/grimoire/linemap)
    #:private (list 'racket/base)))


@title[#:tag "linemap"]{Linemaps}
@defmodule[resyntax/grimoire/linemap]

A @deftech{linemap} is a precomputed index of a string's line structure that supports converting
between character positions and line numbers. Source code is frequently viewed from one of two
perspectives:

@itemlist[
 @item{A @emph{human} perspective, either reader or writer, who looks at code as a 2D grid composed of
  lines and columns.}

 @item{A @emph{machine} perspective, that looks at code as one linear sequence of characters (or
  perhaps even just plain bytes).}]

Tools that serve as a human-machine interface for code often have to juggle these two perspectives.
Linemaps are Resyntax's tool for doing so. They are used in various places where human concerns
related to viewing and describing source code and source edits come up, such as displaying or
consuming line-based diffs in the command-line interface. See @secref["cli"] for further details on
that matter.

@bold{Positions in a linemap are zero-based, but line numbers are one-based.} Positions
are character indices into the string. This follows the same convention as Racket's string operations
such as @racket[string-ref], as well as Resyntax's conventions for @tech{string replacements}. Line
numbers, however, follow the conventions outlined in
@secref["linecol" #:doc '(lib "scribblings/reference/reference.scrbl")]: source files begin at line
@racket[1]. This matches @racket[syntax-line] and the conventions of code editors --- line numbers
are almost exclusively useful in user interfaces, where one-based numbering is expected.

@bold{Beware that @racket[syntax-position] and file port positions are @emph{one-based}, unlike
 linemap positions.} The @racket[syntax-line-range] operation performs that conversion itself, but
positions obtained from syntax objects by other means must be converted before use with a linemap.

The lines of a string are the segments separated by newline characters. The terminating newline is
not part of a line's contents, but positions of newline characters belong to the lines they
terminate. A string that ends with a newline has a final empty line after it, and the empty string
consists of a single empty line. Note that there are multiple distinct byte sequences that Racket
treats as a newline when reading source code --- for further details on how Resyntax handles these
cases, see the notes in @racket[with-input-from-source].


@defproc[(linemap? [v any/c]) boolean?]{
 A predicate that recognizes @tech{linemaps}.}


@defproc[(string-linemap [str string?]) linemap?]{
 Constructs a @tech{linemap} of the lines in @racket[str]. Only @racket[#\newline] characters are
 treated as line separators. In particular, Windows-style @racket["\r\n"] line endings are not
 understood. This never arises in practice, because Resyntax normalizes all newlines to
 @racket[#\newline] when reading @tech{source code} --- see @racket[with-input-from-source] for
 details on that normalization and why it matters.

 @(examples
   #:eval (make-evaluator) #:once
   (eval:no-prompt (define lines (string-linemap "hello\nworld\n")))
   (linemap-position-to-line lines 0)
   (code:comment "The trailing newline creates an empty third line.")
   (linemap-position-to-line lines 12))}


@defproc[(linemap-position-to-line [map linemap?] [position exact-nonnegative-integer?])
         exact-positive-integer?]{
 Returns the line number of the line containing @racket[position]. The position of a newline
 character is considered contained by the line that the newline terminates. The position equal to
 the length of the string --- the string's exclusive end position --- is allowed, and belongs to
 the last line. Positions greater than that raise a contract error.

 @(examples
   #:eval (make-evaluator) #:once
   (eval:no-prompt (define lines (string-linemap "hello\nworld\n")))
   (code:comment "Position 5 is line 1's terminating newline, so it belongs to line 1.")
   (linemap-position-to-line lines 5)
   (linemap-position-to-line lines 6)
   (code:comment "Positions past the end of the string are out of bounds.")
   (eval:error (linemap-position-to-line lines 100)))}


@defproc[(linemap-position-to-start-of-line [map linemap?] [position exact-nonnegative-integer?])
         exact-nonnegative-integer?]{
 Returns the position of the first character of the line containing @racket[position]. If the
 string ends with a newline and @racket[position] is on the final, empty line after it, that
 line's start position is equal to the length of the string. Like
 @racket[linemap-position-to-line], positions greater than the length of the string raise a
 contract error.

 @(examples
   #:eval (make-evaluator) #:once
   (eval:no-prompt (define lines (string-linemap "hello\nworld\n")))
   (linemap-position-to-start-of-line lines 8)
   (code:comment "The final empty line starts at the very end of the string.")
   (linemap-position-to-start-of-line lines 12))}


@defproc[(linemap-position-to-end-of-line [map linemap?] [position exact-nonnegative-integer?])
         exact-nonnegative-integer?]{
 Returns the position just past the last character of the contents of the line containing
 @racket[position] --- that is, the position of the line's terminating newline, or the length of
 the string if the line is the last one. Like @racket[linemap-position-to-line], positions greater
 than the length of the string raise a contract error.

 @(examples
   #:eval (make-evaluator) #:once
   (eval:no-prompt (define lines (string-linemap "hello\nworld\n")))
   (code:comment "Line 1's contents end just before its newline at position 5.")
   (linemap-position-to-end-of-line lines 2)
   (linemap-position-to-end-of-line lines 8))}


@defproc[(syntax-line-range [stx syntax?] [#:linemap map linemap?]) range?]{
 Returns a closed range (with @racket[natural<=>] as its comparator) containing the line numbers
 of every line that @racket[stx] spans, from the line on which it begins to the line on which it
 ends. The @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{source location} of
 @racket[stx] must refer to positions within the string that @racket[map] was built from. If the
 source location extends past the end of that string, a contract error is raised. This is only
 partial protection against accidentally pairing a syntax object with a linemap built from some
 other, unrelated string, but it's better than nothing.

 @(examples
   #:eval (make-evaluator) #:once
   (eval:no-prompt
    (define src "(define (f x)\n  (* x 2))\n")
    (define in (open-input-string src))
    (port-count-lines! in)
    (define stx (read-syntax 'example in)))
   (syntax-line-range stx #:linemap (string-linemap src))
   (code:comment "Pairing a syntax object with the wrong linemap is caught (sometimes).")
   (eval:error (syntax-line-range stx #:linemap (string-linemap "some other string"))))}
