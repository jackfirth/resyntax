#lang scribble/manual


@(require (for-label racket/base
                     racket/contract/base
                     racket/sequence
                     racket/treelist
                     rebellion/base/comparator
                     rebellion/base/immutable-string
                     resyntax/grimoire/source
                     resyntax/grimoire/syntax-path))


@title[#:tag "syntax-path"]{Syntax Paths}
@defmodule[resyntax/grimoire/syntax-path]

A @deftech{syntax path} identifies the location of a subform within a syntax object. Syntax paths
are sequences of zero-based indices: the @emph{root} path, which contains no indices, refers to an
entire syntax object, and a path whose first element is @racket[_i] refers to a location within the
syntax object's @racket[_i]-th child. Syntax paths are similar in spirit to filesystem paths, and they
print in a filesystem-like notation --- the path @racket[(syntax-path (list 1 2 3))] prints as
@racketresultfont{#<syntax-path:/1/2/3>}. Resyntax uses syntax paths to refer to subforms of a
program in a way that doesn't depend on exact source locations or syntax object identity, and which is
relatively stable when structure-preserving edits are made to source text.

The children of a syntax object are determined by the shape of its datum:

@itemlist[
 @item{The children of a pair-based form are the elements of its @emph{normalized} shape: the
  proper list obtained by treating the trailing atom of an improper list, if any, as a final
  element. Only the form's own pair structure is normalized --- nested forms remain distinct
  children --- but how the underlying pairs and syntax objects nest has no effect on paths:
  @tt{#'(a b c)}, @tt{#'(a . (b . (c . ())))}, @tt{#'(a . (b c))}, @tt{#'(a b . c)},
  and @tt{#'(a . (b . c))} all have three children, and in each case the child at index
  @racket[2] is @racket[#'c].}

 @item{The children of a vector are its elements, in order.}

 @item{A box has a single child, its contents, at index @racket[0].}

 @item{The children of a prefab struct are its fields, in order.}

 @item{Hash datums @emph{cannot} be traversed by syntax paths. Operations that would need to
  traverse a hash raise contract errors instead. More about this constraint is explained in
  @secref["original-syntax-paths"].}

 @item{All other datums have no children.}]


@section[#:tag "original-syntax-paths"]{Original Syntax Paths and Formatting Preservation}

When Resyntax calls the Racket reader to turn @tech{source code} into unexpanded syntax objects using
@racket[source-read-syntax], Resyntax recursively labels each syntax object with its
@deftech{original syntax path}. This label is attached to each syntax object via a
@tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{syntax property} named
@indexed-racket['original-syntax-path].

As these syntax objects are expanded by the Racket macro expander and further manipulated by
@tech{refactoring rules}, subforms get transformed and moved, but they preserve their original syntax
properties. Resyntax then inspects these properties on the refactored syntax objects produced by rules
in order to determine where each syntax object originated @emph{structurally}, in addition to
inspecting source locations to determine where it originated @emph{textually}. Resyntax uses this
information to determine when refactored code contains unchanged subexpressions whose text should be
@emph{copied} from the input rather than reproduced wholesale and reformatted. This mechanism is what
powers the automatic comment preservation described in @secref["comment-preservation"], and the
template metafunctions described in that section give rule authors a way to guide it.

Unchanged syntax objects have their original text copied by Resyntax, and if two unchanged syntax
objects start and end adjacent to each other (and without swapping their order) then the original text
between them is also copied. This allows Resyntax to minimize the amount of text that a refactoring
has to change. However, in order for this process to work, Resyntax makes a few assumptions about the
relationship between syntax paths and the source locations of reader-produced syntax objects. These
assumptions are:

@itemlist[
 @item{If one syntax path is a prefix of another, then the source location of a syntax object
  originally at the first path (the prefix path) should @emph{fully contain} the source location of a
  syntax object originally at the second path. This need not be a @emph{proper subset} relation: it's
  acceptable for the two syntax objects to have the exact same source location. That allowance doesn't
  affect code written in @hash-lang[] @racketmodname[racket], but it matters for other languages which
  sometimes use "invisible" wrappers around syntax objects to reflect grouping implied by how the text
  was originally parsed by the language's reader.}

 @item{If two syntax paths are disjoint, meaning neither is a prefix of the other, then the source
  locations of two syntax objects originally at those paths should contain no overlap (but they may
  be adjacent). Furthermore, the lesser syntax path (in the sense of @racket[syntax-path<=>]) should
  correspond to the source location that occurs earlier in the source code (by character position).}]

It is the second of these two requirements that prevents Resyntax from traversing literal hash datums.
A hash datum such as @racket[#hash((a . 1) ("foo" . 2) (b . 3))], when parsed by the Racket reader,
@bold{does not preserve source ordering in the resulting hash datum's iteration order}. Two literal
hash datums with the same keys will always iterate in the same order when inspected with
@racket[syntax-e]. As a result, Resyntax cannot uphold the assumptions it makes about source locations
and syntax paths, and cannot preserve text between adjacent hash entries correctly. For this reason,
Resyntax does not allow editing expressions inside hash datums.


@section{Basic Syntax Path Operations}


@defproc[(syntax-path? [v any/c]) boolean?]{
 A predicate that recognizes @tech{syntax paths}.}


@defproc[(root-syntax-path? [v any/c]) boolean?]{
 A predicate that recognizes the root @tech{syntax path}. Implies @racket[syntax-path?].}


@defproc[(child-syntax-path? [v any/c]) boolean?]{
 A predicate that recognizes @tech{syntax paths} that refer to a child of some enclosing form ---
 that is, any path except the root. Implies @racket[syntax-path?].}


@defthing[root-syntax-path syntax-path?]{
 The root @tech{syntax path}, which contains no indices and refers to an entire syntax object
 rather than to any subform within it.}


@defproc[(syntax-path [elements (sequence/c exact-nonnegative-integer?)]) syntax-path?]{
 Constructs a @tech{syntax path} from a sequence of zero-based child indices.}


@defproc[(syntax-path-elements [path syntax-path?]) (treelist/c exact-nonnegative-integer?)]{
 Returns the child indices that make up @racket[path], as a
 @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{treelist}.}


@defproc[(syntax-path-add [path syntax-path?] [element exact-nonnegative-integer?])
         syntax-path?]{
 Extends @racket[path] with one additional child index. The resulting path refers to the
 @racket[element]-th child of the subform that @racket[path] refers to.}


@defproc[(syntax-path-parent [path child-syntax-path?]) syntax-path?]{
 Returns the path to the parent of @racket[path], which refers to the form that immediately encloses
 the form referred to by @racket[path].}


@defproc[(syntax-path-last-element [path child-syntax-path?]) exact-nonnegative-integer?]{
 Returns the final child index of @racket[path], which is the position of the subform that
 @racket[path] refers to within its enclosing form.}


@defproc[(syntax-path-next-neighbor [path syntax-path?]) (or/c syntax-path? #false)]{
 Returns the path to the sibling immediately following @racket[path] within its enclosing form, or
 @racket[#false] if @racket[path] is the root path (the root of a syntax object has no siblings).
 Note that this is pure path arithmetic: the returned path is not guaranteed to actually exist within
 any particular syntax object.}


@defproc[(syntax-path-neighbors? [leading-path syntax-path?] [trailing-path syntax-path?])
         boolean?]{
 Returns @racket[#true] if @racket[leading-path] and @racket[trailing-path] refer to immediately
 adjacent siblings, meaning they share the same parent path and @racket[trailing-path]'s final
 child index is one greater than @racket[leading-path]'s. @bold{Warning:} the order of
 @racket[leading-path] and @racket[trailing-path] is significant --- this operation returns
 @racket[#false] if @racket[leading-path] and @racket[trailing-path] are adjacent siblings where the
 @emph{trailing} path comes first.}


@defproc[(syntax-path-remove-prefix [path syntax-path?] [prefix syntax-path?]) syntax-path?]{
 Returns @racket[path] with the leading elements of @racket[prefix] removed, producing a path
 relative to the subform that @racket[prefix] refers to. Raises a contract error if @racket[path]
 does not start with @racket[prefix].}


@defthing[syntax-path<=> (comparator/c syntax-path?)]{
 A comparator that orders @tech{syntax paths} lexicographically by their child indices. A path that
 is a prefix of another path sorts before it, so ancestors precede their descendants and sorting a
 collection of paths produces a depth-first preorder traversal.}


@defproc[(syntax-path->string [path syntax-path?]) immutable-string?]{
 Returns a string notation for @racket[path] in which each child index is preceded by a slash, like
 a filesystem path. The root path is rendered as @racket["/"].}


@defproc[(string->syntax-path [str string?]) syntax-path?]{
 Parses @racket[str] as a @tech{syntax path}. This is the inverse of @racket[syntax-path->string].
 The string must start with a slash, must not end with a slash (except for the root path
 @racket["/"]), and must contain only slash-separated nonnegative integers. Raises a contract error
 otherwise.}


@section{Operating on Syntax Objects with Syntax Paths}


@defproc[(syntax-ref [stx syntax?] [path syntax-path?]) syntax?]{
 Returns the subform of @racket[stx] that @racket[path] refers to. The root path returns
 @racket[stx] itself. Raises a contract error if @racket[path] is inconsistent with the shape of
 @racket[stx], which can occur if @racket[path] refers to children of an atomic subform that has no
 children, or if @racket[path] contains a child index that's too large for the number of children
 actually contained by the parent subform of that index.}


@defproc[(syntax-contains-path? [stx syntax?] [path syntax-path?]) boolean?]{
 Returns @racket[#true] if @racket[path] refers to a subform of @racket[stx], meaning
 @racket[syntax-ref] would succeed.}


@defproc[(syntax-set [stx syntax?] [path syntax-path?] [new-subform syntax?]) syntax?]{
 Returns a copy of @racket[stx] in which the subform that @racket[path] refers to is replaced with
 @racket[new-subform]. Passing the root path returns @racket[new-subform] itself. The lexical
 context, source locations, and syntax properties of the enclosing forms are preserved.}


@defproc[(syntax-remove-splice [stx syntax?]
                               [path child-syntax-path?]
                               [children-count exact-nonnegative-integer?])
         syntax?]{
 Returns a copy of @racket[stx] with @racket[children-count] consecutive children removed from the
 form enclosing @racket[path], starting with the child that @racket[path] refers to. The enclosing
 form must be a proper list. Removing zero children returns @racket[stx] unchanged. Removing one child
 removes @emph{only} the subform referred to by @racket[path]. If @racket[stx] does not contain
 @racket[path] (in the sense of @racket[syntax-contains-path?]) a contract error is raised.}


@defproc[(syntax-insert-splice [stx syntax?]
                               [path child-syntax-path?]
                               [new-children (sequence/c syntax?)])
         syntax?]{
 Returns a copy of @racket[stx] with each syntax object in @racket[new-children] inserted as
 consecutive children of the form enclosing @racket[path], such that the first inserted child is
 located at @racket[path]. Existing children at or after @racket[path] are shifted over ---
 @racket[new-children] are inserted @emph{just before} @racket[path]. The enclosing form must be a
 proper list. Inserting an empty sequence returns @racket[stx] unchanged.}


@defproc[(syntax-label-paths [stx syntax?] [property-name symbol?]) syntax?]{
 Returns a copy of @racket[stx] in which every subform, including @racket[stx] itself, has a
 @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{syntax property} named
 @racket[property-name] whose value is that subform's @tech{syntax path} within @racket[stx].
 Subforms of hash datums are not labeled, as syntax paths cannot refer to them --- see
 @secref["original-syntax-paths"] for further explanation.}


@defproc[(in-syntax-paths [stx syntax?] [#:base-path base-path syntax-path? root-syntax-path])
         (sequence/c syntax-path?)]{
 Returns a sequence of the @tech{syntax paths} of every subform in @racket[stx], in depth-first
 preorder. Each returned path is prefixed with @racket[base-path], so the first path in the
 sequence is always @racket[base-path] itself. The @racket[base-path] argument is useful when
 @racket[stx] is itself a subform of some larger syntax object, such as the root syntax object for the
 entire source file that @racket[stx] originates from.}
