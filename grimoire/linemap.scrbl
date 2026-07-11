#lang scribble/manual


@(require (for-label racket/base
                     racket/contract/base
                     rebellion/base/comparator
                     rebellion/base/range
                     resyntax/grimoire/linemap
                     resyntax/grimoire/source))


@title[#:tag "linemap"]{Linemaps}
@defmodule[resyntax/grimoire/linemap]

A @deftech{linemap} is a precomputed index of a string's line structure that supports converting
between character positions and line numbers. Resyntax straddles two views of source code: the
line-oriented view, used by @tech{source groups} and the command-line interface (see
@secref["cli"]) to select which code to analyze, and the character-oriented view, used by
@tech{string replacements} and syntax object source locations. Linemaps are the bridge between them --- Resyntax uses linemaps to compute which
lines a refactoring suggestion modifies, to render string replacements as line-based diffs, and to
restrict analysis to the requested lines.

@bold{Positions in a linemap are zero-based, but line numbers are one-based.} Positions are
character indices into the string, following the same convention as Racket's string operations and
as @tech{string replacements}: the first character of a string is at position @racket[0]. Line
numbers instead begin at line @racket[1], matching @racket[syntax-line] and the conventions of
code editors --- line numbers are almost exclusively useful in user interfaces, where one-based
numbering is expected. Beware that @racket[syntax-position] and file port positions are
@emph{one-based}, unlike linemap positions. The @racket[syntax-line-range] operation performs that
conversion itself, but positions obtained from syntax objects by other means must be converted
before use with a linemap.

The lines of a string are the segments separated by newline characters. The terminating newline is
not part of a line's contents, but positions of newline characters belong to the lines they
terminate. A string that ends with a newline has a final empty line after it, and the empty string
consists of a single empty line.


@defproc[(linemap? [v any/c]) boolean?]{
 A predicate that recognizes @tech{linemaps}.}


@defproc[(string-linemap [str string?]) linemap?]{
 Constructs a @tech{linemap} of the lines in @racket[str]. Only @racket[#\newline] characters are
 treated as line separators. In particular, Windows-style @racket["\r\n"] line endings are not
 understood. This never arises in practice, because Resyntax normalizes all newlines to
 @racket[#\newline] when reading @tech{source code} --- see @racket[with-input-from-source] for
 details on that normalization and why it matters.}


@defproc[(linemap-position-to-line [map linemap?] [position exact-nonnegative-integer?])
         exact-positive-integer?]{
 Returns the line number of the line containing @racket[position]. The position of a newline
 character is considered contained by the line that the newline terminates. Positions beyond the
 end of the string do not raise an error; they are all treated as belonging to the last line.}


@defproc[(linemap-position-to-start-of-line [map linemap?] [position exact-nonnegative-integer?])
         exact-nonnegative-integer?]{
 Returns the position of the first character of the line containing @racket[position]. If the
 string ends with a newline and @racket[position] is on the final, empty line after it, that
 line's start position is equal to the length of the string.}


@defproc[(linemap-position-to-end-of-line [map linemap?] [position exact-nonnegative-integer?])
         exact-nonnegative-integer?]{
 Returns the position just past the last character of the contents of the line containing
 @racket[position] --- that is, the position of the line's terminating newline, or the length of
 the string if the line is the last one.}


@defproc[(syntax-line-range [stx syntax?] [#:linemap map linemap?]) range?]{
 Returns a closed range (with @racket[natural<=>] as its comparator) containing the line numbers
 of every line that @racket[stx] spans, from the line on which it begins to the line on which it
 ends. The @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{source location} of
 @racket[stx] must refer to positions within the string that @racket[map] was built from.}
