#lang scribble/manual


@(require (for-label racket/base
                     racket/contract/base
                     racket/math
                     racket/sequence
                     rebellion/base/immutable-string
                     rebellion/collection/range-set
                     rebellion/streaming/reducer
                     rebellion/streaming/transducer
                     resyntax/grimoire/string-replacement))


@title[#:tag "string-replacement"]{String Replacements}
@defmodule[resyntax/grimoire/string-replacement]

A @deftech{string replacement} is a value describing an edit to a string. A replacement identifies a
region of the string to replace --- the characters between a start position and an end position ---
and describes the new contents of that region as a list of @tech{string pieces}. There are two kinds
of pieces:

@itemlist[
 @item{@emph{Inserted strings}, constructed with @racket[inserted-string], containing brand new text
  to insert.}

 @item{@emph{Copied strings}, constructed with @racket[copied-string], containing a range of
  positions to copy from the original string. Copied pieces may copy from @emph{anywhere} in the
  original string, not just from within the replaced region, so a replacement can move text around
  in addition to inserting and deleting it.}]

All positions are zero-based character offsets, and regions are half-open: a replacement from
position @racket[_start] to position @racket[_end] covers the characters at positions
@racket[_start] through @racket[(sub1 _end)].

String replacements are the final, lowest-level form that Resyntax's refactoring suggestions take
before being written to files. The distinction between inserted and copied pieces is the string-level
end of the formatting preservation mechanism described in @secref["original-syntax-paths"]: when
Resyntax decides that a piece of refactored code is unchanged from the original program, the
suggestion copies its text rather than regenerating it, and that decision ultimately takes the form
of a @racket[copied-string] piece.


@defproc[(string-replacement? [v any/c]) boolean?]{
 A predicate that recognizes @tech{string replacements}.}


@defproc[(string-replacement
          [#:start start natural?]
          [#:end end natural?]
          [#:contents contents (sequence/c string-piece?)])
         string-replacement?]{
 Constructs a @tech{string replacement} that replaces the characters between @racket[start] and
 @racket[end] with the given @racket[contents]. Raises a contract error if @racket[end] is before
 @racket[start].

 The contents are normalized during construction: pieces that span zero characters are dropped,
 adjacent inserted strings are merged into one, and adjacent copied strings that copy contiguous
 regions are merged into one. As a consequence, two replacements constructed from differently
 divided piece lists describing the same content are @racket[equal?], and
 @racket[string-replacement-contents] may return a different list than the one the replacement was
 constructed with.}


@defproc[(string-replacement-start [replacement string-replacement?]) natural?]{
 Returns the position of the first character replaced by @racket[replacement].}


@defproc[(string-replacement-original-end [replacement string-replacement?]) natural?]{
 Returns the position just past the last character replaced by @racket[replacement], in terms of
 positions within the @emph{original} string.}


@defproc[(string-replacement-original-span [replacement string-replacement?]) natural?]{
 Returns the number of characters of the original string that @racket[replacement] replaces.}


@defproc[(string-replacement-new-end [replacement string-replacement?]) natural?]{
 Returns the position just past the replaced region within the @emph{edited} string produced by
 applying @racket[replacement], equal to the replacement's start position plus its new span.}


@defproc[(string-replacement-new-span [replacement string-replacement?]) natural?]{
 Returns the total number of characters that @racket[replacement]'s contents span, which is the
 length of the text that the replaced region contains after the replacement is applied.}


@defproc[(string-replacement-contents [replacement string-replacement?])
         (listof string-piece?)]{
 Returns the @tech{string pieces} making up the new contents of @racket[replacement]'s replaced
 region, in normalized form.}


@defproc[(string-replacement-preserved-locations [replacement string-replacement?]) range-set?]{
 Returns a range set of the positions in the original string whose characters are preserved by
 @racket[replacement]: every position before the replaced region, every position after it, and every
 position within the source range of one of the replacement's @racket[copied-string] pieces.}


@defproc[(string-replacement-overlaps? [replacement string-replacement?]
                                       [other-replacement string-replacement?])
         boolean?]{
 Returns @racket[#true] if the replaced regions of @racket[replacement] and
 @racket[other-replacement] overlap. Replacements whose regions merely touch at a boundary do not
 overlap, since regions are half-open.}


@defproc[(string-replacement-union [replacement1 string-replacement?]
                                   [replacement2 string-replacement?])
         string-replacement?]{
 Combines @racket[replacement1] and @racket[replacement2] into a single @tech{string replacement}
 whose replaced region covers both of their regions. The text between the two regions is copied from
 the original string unchanged. The order of the arguments doesn't matter. Raises a contract error
 if the two replacements overlap (in the sense of @racket[string-replacement-overlaps?]).}


@defthing[union-into-string-replacement (reducer/c string-replacement? string-replacement?)]{
 A @tech[#:doc '(lib "rebellion/main.scrbl")]{reducer} that combines a sequence of pairwise
 non-overlapping @tech{string replacements} into one, as with @racket[string-replacement-union], for
 use with @racket[transduce]. The reduction starts from an empty replacement at position @racket[0],
 so the combined replacement's region always starts at position @racket[0].}


@defproc[(string-replacement-normalize
          [replacement string-replacement?]
          [original-string string?]
          [#:preserve-start preserve-start (or/c exact-nonnegative-integer? #false) #false]
          [#:preserve-end preserve-end (or/c exact-nonnegative-integer? #false) #false])
         string-replacement?]{
 Returns a @tech{string replacement} equivalent to @racket[replacement] --- applying it to
 @racket[original-string] produces the same result --- but whose replaced region is as small as
 possible. Leading and trailing portions of the replacement that leave the original text unchanged
 are trimmed away. If @racket[preserve-start] is provided, the normalized region is never trimmed
 past it: the region always starts at or before @racket[preserve-start]. Likewise, if
 @racket[preserve-end] is provided, the normalized region always ends at or after
 @racket[preserve-end]. Replacements that don't change @racket[original-string] at all are returned
 unchanged.}


@defproc[(string-replacement-render [replacement string-replacement?] [original-string string?])
         immutable-string?]{
 Returns the new text of @racket[replacement]'s replaced region --- just the rendered contents, not
 the entire edited string. The original string is needed to render the contents of
 @racket[copied-string] pieces. Raises a contract error if @racket[original-string] is too short to
 contain the replaced region or the positions that the replacement's copied pieces refer to.}


@defproc[(string-apply-replacement [string string?] [replacement string-replacement?])
         immutable-string?]{
 Applies @racket[replacement] to @racket[string], returning the entire edited string. Text outside
 the replaced region is unchanged. Raises a contract error if @racket[string] is too short to
 contain the replaced region or the positions that the replacement's copied pieces refer to.}


@defproc[(file-apply-string-replacement! [path path-string?] [replacement string-replacement?])
         void?]{
 Reads the file at @racket[path], applies @racket[replacement] to its contents as with
 @racket[string-apply-replacement], and overwrites the file with the result.}


@section{String Pieces}

A @deftech{string piece} describes a segment of text, either brand new or copied from some original
string. String pieces primarily serve as the contents of a @tech{string replacement}'s replaced
region, but they are also occasionally useful on their own as standalone descriptions of text.


@defproc[(string-piece? [v any/c]) boolean?]{
 A predicate that recognizes @tech{string pieces} of either kind.}


@defproc[(inserted-string? [v any/c]) boolean?]{
 A predicate that recognizes inserted-string @tech{string pieces}. Implies @racket[string-piece?].}


@defproc[(inserted-string [contents string?]) inserted-string?]{
 Constructs a @tech{string piece} containing @racket[contents] as brand new text.}


@defproc[(inserted-string-contents [piece inserted-string?]) immutable-string?]{
 Returns the text that @racket[piece] inserts.}


@defproc[(copied-string? [v any/c]) boolean?]{
 A predicate that recognizes copied-string @tech{string pieces}. Implies @racket[string-piece?].}


@defproc[(copied-string [start natural?] [end natural?]) copied-string?]{
 Constructs a @tech{string piece} that copies the characters between @racket[start] and
 @racket[end] from the original string. The copied range may lie anywhere within the original
 string, including entirely outside the replaced region. Raises a contract error if @racket[end] is
 before @racket[start].}


@defproc[(copied-string-start [piece copied-string?]) natural?]{
 Returns the position of the first character that @racket[piece] copies.}


@defproc[(copied-string-end [piece copied-string?]) natural?]{
 Returns the position just past the last character that @racket[piece] copies.}


@defproc[(string-piece-span [piece string-piece?]) exact-nonnegative-integer?]{
 Returns the number of characters that @racket[piece] spans: the length of an inserted string's
 text, or the size of a copied string's range.}
