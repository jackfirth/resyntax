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

All positions are zero-based @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{character}
offsets, and regions are half-open: a replacement from
position @racket[_start] to position @racket[_end] covers the characters at positions
@racket[_start] through @racket[(sub1 _end)]. @bold{Beware that string replacements do NOT use
 one-based position indices}, unlike positions in syntax object source locations such as those
returned by @racket[syntax-position].

When applied to a string, a string replacement first deletes everything in the range it describes.
Then it inserts and copies text into that range, according to the string replacement's piece list.
The replacement is applied atomically: character positions in copied strings always refer to the
positions of characters in the original, unedited string, prior to any deletions, insertions, or
copying operations being applied.

String replacements are the final, lowest-level form that Resyntax's refactoring suggestions take
before being written to files. The distinction between inserted and copied pieces is the string-level
end of the formatting preservation mechanism described in @secref["original-syntax-paths"]: when
Resyntax decides that a piece of refactored code is unchanged from the original program, the
suggestion copies its text rather than regenerating it, and that decision ultimately takes the form
of a @racket[copied-string] piece. This mechanism is also used by Resyntax to preserve comments where
possible and discard suggestions that can't preserve them; see
@racket[string-replacement-preserved-locations] for details.


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

 The contents are lightly normalized during construction: pieces that span zero characters are
 dropped, adjacent inserted strings are merged into one, and adjacent copied strings that copy
 contiguous regions are merged into one. As a consequence, two replacements constructed from
 differently divided piece lists describing the same content are @racket[equal?], and
 @racket[string-replacement-contents] may return a different list than the one the replacement was
 constructed with.

 Beware that this normalization does @bold{not} remove redundant no-op @racket[copied-string]
 pieces at the start or end of the replacement. That is the job of the stricter
 @tech{replacement focusing} operation implemented by @racket[string-replacement-focus]. For
 example, consider a replacement that starts at position @racket[0], ends at position @racket[_n],
 and contains a single @racket[(copied-string 0 _n)] piece. This replacement is functionally a
 no-op that makes no changes at all to the string. Construction does not remove the
 @racket[copied-string] piece, even though it behaves identically to an empty string replacement.
 Focusing the replacement, on the other hand, @emph{does} remove the copied string piece --- see
 @racket[string-replacement-focus] for details.}


@defproc[(string-replacement-start [replacement string-replacement?]) natural?]{
 Returns the position of the first character replaced by @racket[replacement].}


@defproc[(string-replacement-original-end [replacement string-replacement?]) natural?]{
 Returns the position just past the last character replaced by @racket[replacement], in terms of
 positions within the @emph{original} string. Note that this is a zero-indexed position, with zero
 being before the first character.}


@defproc[(string-replacement-original-span [replacement string-replacement?]) natural?]{
 Returns the size in characters of the range of the original string that @racket[replacement]
 replaces. This is always equal to @racket[string-replacement-start] subtracted from
 @racket[string-replacement-original-end].}


@defproc[(string-replacement-new-end [replacement string-replacement?]) natural?]{
 Returns the position just past the replaced region within the @emph{edited} string produced by
 applying @racket[replacement], equal to the replacement's start position plus its new span.}


@defproc[(string-replacement-new-span [replacement string-replacement?]) natural?]{
 Returns the total number of characters that @racket[replacement]'s contents span, which is the
 length of the text that the replaced region contains after the replacement is applied. This is
 always equal to @racket[string-replacement-start] subtracted from
 @racket[string-replacement-new-end].}


@defproc[(string-replacement-contents [replacement string-replacement?])
         (listof string-piece?)]{
 Returns the @tech{string pieces} making up the new contents of @racket[replacement]'s replaced
 region, in normalized form.}


@defproc[(string-replacement-preserved-locations [replacement string-replacement?]) range-set?]{
 Returns a range set of the positions in the original string whose characters are preserved by
 @racket[replacement]: every position before the replaced region, every position after it, and every
 position within the source range of one of the replacement's @racket[copied-string] pieces.

 This is used by Resyntax to determine whether or not a replacement preserves comments: if a
 replacement's @emph{unpreserved} locations have any overlap with @racket[source-comment-locations]
 for the source being edited, then the replacement @emph{drops comments} and is discarded by
 Resyntax.}


@defproc[(string-replacement-overlaps? [replacement string-replacement?]
                                       [other-replacement string-replacement?])
         boolean?]{
 Returns @racket[#true] if the replaced regions of @racket[replacement] and
 @racket[other-replacement] overlap. Replacements whose regions merely touch at a boundary do not
 overlap, since regions are half-open character ranges.

 Overlap detection is conservatively imperfect. There exist string replacements that can be applied
 together safely, but which @racket[string-replacement-overlaps?] reports @racket[#true] for. However,
 there are @emph{never} cases where @racket[string-replacement-overlaps?] issues a false negative ---
 only false positives. Further improvement here would likely require adjusting the representation of
 string replacements using a range map structure instead of a single range and a list of pieces.}


@defproc[(string-replacement-union [replacement1 string-replacement?]
                                   [replacement2 string-replacement?])
         string-replacement?]{
 Combines @racket[replacement1] and @racket[replacement2] into a single @tech{string replacement}
 whose replaced region covers both of their regions. The text between the two regions is copied from
 the original string unchanged. The order of the arguments doesn't matter. Raises a contract error
 if the two replacements overlap (in the sense of @racket[string-replacement-overlaps?]).

 A quirk of the current implementation is that empty replacements are not treated specially. The union
 of an empty replacement at position @racket[0] and a replacement at position @racket[_n] will produce
 an expanded replacement that starts at position @racket[0]. A no-op @racket[copied-string] piece that
 copies the contents between @racket[0] and @racket[_n] will be included. This results in a
 replacement that behaves identically, but which reports an earlier start position and larger spans
 from @racket[string-replacement-original-span] and @racket[string-replacement-new-span]. Like the
 deficiencies of @racket[string-replacement-overlaps?], this could be fixed by using a range map
 representation for string replacements instead of a single range representation.

 Note that even for non-overlapping replacements, applying the union of two string replacements is
 @bold{not} the same as applying one replacement and then applying the other. This is because of
 copied strings --- the first replacement may change the size of the string, which can cause the
 character positions referenced by copied string pieces in the second replacement to refer to
 different text than it would have if the replacements were applied in the opposite order. This is
 because although individual string replacements can be applied atomically, @emph{multiple}
 replacements cannot.

 When Resyntax wants to apply multiple string replacements at once, it always combines their
 replacements into a single union replacement. Conflicting replacements are dropped; rather than try
 to apply them in a second stage, Resyntax throws them out and re-analyzes the entire modified source
 file instead to generate fresh suggested replacements. As a result, Resyntax has to decide which
 replacements it wants to apply in each analysis round @emph{before} it can begin the next analysis
 round. The result of each analysis round edits the source code in-memory using
 @racket[modified-source] so that Resyntax can interleave editing and analysis in this manner without
 actually committing its edits to the filesystem.}


@defthing[union-into-string-replacement (reducer/c string-replacement? string-replacement?)]{
 A @tech[#:doc '(lib "rebellion/main.scrbl")]{reducer} that combines a sequence of pairwise
 non-overlapping @tech{string replacements} into one, as with @racket[string-replacement-union], for
 use with @racket[transduce]. The reduction starts from an empty replacement at position @racket[0],
 so the combined replacement's region always starts at position @racket[0].

 Note that because of the quirk with how @racket[string-replacement-union] handles empty replacements,
 this implies that a replacement produced by @racket[union-into-string-replacement] @bold{always}
 starts at position @racket[0] and includes a large no-op @racket[copied-string] piece between
 position @racket[0] and the lowest-position replacement that was included in the union.}


@defproc[(string-replacement-focus
          [replacement string-replacement?]
          [original-string string?]
          [#:preserve-start preserve-start (or/c exact-nonnegative-integer? #false) #false]
          [#:preserve-end preserve-end (or/c exact-nonnegative-integer? #false) #false])
         string-replacement?]{
 Returns a @tech{string replacement} equivalent to @racket[replacement] --- applying it to
 @racket[original-string] produces the same result --- but whose replaced region is as small as
 possible. Leading and trailing portions of the replacement that leave the original text unchanged
 are trimmed away. If @racket[preserve-start] is provided, the focused region is never trimmed
 past it: the region always starts at or before @racket[preserve-start]. Likewise, if
 @racket[preserve-end] is provided, the focused region always ends at or after
 @racket[preserve-end]. Replacements that don't change @racket[original-string] at all are shrunk to
 the smallest no-op replacements possible given the constraints of @racket[preserve-start] and
 @racket[preserve-end], and shrunk to empty replacements starting at their original start if those
 constraints are not provided.

 This operation is called @deftech{replacement focusing}, and is more aggressive than the
 normalization performed automatically by the @racket[string-replacement] constructor. This is the
 low-level mechanism used by Resyntax to implement the replacement focusing behavior described in
 @secref["replacement-focusing"].}

@defproc[(string-replacement-new-text [replacement string-replacement?] [original-string string?])
         immutable-string?]{
 Returns the new text of @racket[replacement]'s replaced region --- just the rendered contents, not
 the entire edited string. The length of the returned string is always equal to
 @racket[string-replacement-new-span], and it's the text that occupies the region ending at
 @racket[string-replacement-new-end] once the replacement is applied. The original string is needed
 to render the contents of @racket[copied-string] pieces. Raises a contract error if
 @racket[original-string] is too short to contain the replaced region or the positions that the
 replacement's copied pieces refer to.}


@defproc[(string-replacement-apply [replacement string-replacement?] [string string?])
         immutable-string?]{
 Applies @racket[replacement] to @racket[string], returning the entire edited string. Text outside
 the replaced region is unchanged. Raises a contract error if @racket[string] is too short to
 contain the replaced region or the positions that the replacement's copied pieces refer to.}


@defproc[(string-replacement-apply-to-file! [replacement string-replacement?]
                                            [path path-string?])
         void?]{
 Reads the file at @racket[path], applies @racket[replacement] to its contents as with
 @racket[string-replacement-apply], and overwrites the file with the result.}


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
