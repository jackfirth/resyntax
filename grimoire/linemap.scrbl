#lang scribble/manual


@(require (for-label racket/base
                     racket/contract/base
                     rebellion/base/comparator
                     rebellion/base/range
                     resyntax/grimoire/linemap))


@title[#:tag "linemap"]{Linemaps}
@defmodule[resyntax/grimoire/linemap]

A @deftech{linemap} is a precomputed index of a string's line structure that supports converting
between character positions and line numbers. Resyntax straddles two views of source code: the
line-oriented view, used by @tech{source groups} and the command-line interface (see
@secref["cli"]) to select which code to analyze, and the character-oriented view, used by
@tech{string replacements} and syntax object source locations. Linemaps are the bridge between them --- Resyntax uses linemaps to compute which
lines a refactoring suggestion modifies, to render string replacements as line-based diffs, and to
restrict analysis to the requested lines.

@bold{All positions and line numbers in a linemap are one-based}, matching the conventions of
@racket[syntax-position] and @racket[syntax-line]: the first character of a string is at position
@racket[1], on line @racket[1]. Beware that this is the @emph{opposite} of the convention used by
@tech{string replacements}, which address characters with @emph{zero-based} indices. Converting
between the two worlds requires adding or subtracting one, as discussed in
@secref["string-replacement"].

The lines of a string are the segments separated by newline characters. The terminating newline is
not part of a line's contents, but positions of newline characters belong to the lines they
terminate. A string that ends with a newline has a final empty line after it, and the empty string
consists of a single empty line.


@defproc[(linemap? [v any/c]) boolean?]{
 A predicate that recognizes @tech{linemaps}.}


@defproc[(string-linemap [str string?]) linemap?]{
 Constructs a @tech{linemap} of the lines in @racket[str]. Only @racket[#\newline] characters are
 treated as line separators.}


@defproc[(linemap-position-to-line [map linemap?] [position exact-positive-integer?])
         exact-positive-integer?]{
 Returns the line number of the line containing @racket[position]. The position of a newline
 character is considered contained by the line that the newline terminates. Positions beyond the
 end of the string do not raise an error; they are all treated as belonging to the last line.}


@defproc[(linemap-position-to-start-of-line [map linemap?] [position exact-positive-integer?])
         exact-positive-integer?]{
 Returns the position of the first character of the line containing @racket[position]. If the
 string ends with a newline and @racket[position] is on the final, empty line after it, that
 line's start position is one past the end of the string.}


@defproc[(linemap-position-to-end-of-line [map linemap?] [position exact-positive-integer?])
         exact-positive-integer?]{
 Returns the position just past the last character of the contents of the line containing
 @racket[position] --- that is, the position of the line's terminating newline, or one past the
 end of the string if the line is the last one.}


@defproc[(syntax-line-range [stx syntax?] [#:linemap map linemap?]) range?]{
 Returns a closed range (with @racket[natural<=>] as its comparator) containing the line numbers
 of every line that @racket[stx] spans, from the line on which it begins to the line on which it
 ends. The @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{source location} of
 @racket[stx] must refer to positions within the string that @racket[map] was built from.}
