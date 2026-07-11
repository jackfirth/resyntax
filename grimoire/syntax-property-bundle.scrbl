#lang scribble/manual


@(require (for-label racket/base
                     racket/contract/base
                     racket/mutability
                     racket/sequence
                     rebellion/base/symbol
                     rebellion/collection/entry
                     rebellion/collection/sorted-map
                     rebellion/streaming/reducer
                     resyntax/grimoire/syntax-path
                     resyntax/grimoire/syntax-property-bundle))


@title[#:tag "syntax-property-bundle"]{Syntax Property Bundles}
@defmodule[resyntax/grimoire/syntax-property-bundle]

A @deftech{syntax property bundle} is an immutable tree of
@tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{syntax properties} that has been
detached from any particular syntax object tree. Each property in a bundle is addressed by the
combination of a @tech{syntax path}, identifying the subform the property belongs to, and a symbol
property key. A bundle contains at most one value for each path and key combination. Property bundles
are a plain data representation of metadata that can be attached to and extracted from arbitrary
syntax objects and their subforms.

Ordinarily, syntax properties live directly on syntax objects. Representing them separately as plain
data lets Resyntax manipulate the properties themselves: bundles can be filtered, merged, inspected,
and rearranged without ever touching a syntax object. Resyntax's @tech{expansion analyzers} work this
way --- each analyzer examines a fully expanded module and reports its findings as a syntax property
bundle. Resyntax then combines each analyzer's output bundle together, rearranges the combined bundle,
and grafts it onto the original, unexpanded syntax object before analyzing it with
@tech{refactoring rules}.

Although syntax objects themselves permit arbitrary property keys, property keys in a bundle are
more limited. A syntax property bundle key @emph{must} be an interned symbol. This is for two reasons,
one specific and one general:

@itemlist[
 @item{Interned symbol keys are the only syntax property keys that can be iterated by
  @racket[syntax-property-symbol-keys], so such keys are the only kind that bundles can faithfully
  transfer between syntax objects. Since the entire purpose of property bundles is to transfer
  properties between syntax objects, allowing uninterned symbol keys would have no purpose.}

 @item{More generally, @bold{syntax objects are not plain serializable data}. Syntax objects are not
  safe to transfer from one Racket process to another. This is relied upon by various layers of the
  Racket macro system and similar low-level systems for encapsulation purposes. Some syntax properties
  are only used during macro expansion with opaque keys to provide private communication channels
  between macros that implement some library abstraction. Exposing such keys directly would allow
  intermediate macros in client code using that library to violate its abstractions. These sorts of
  opaque keys are often @racket[gensym]s or similar unserializable unique values, and the Racket macro
  system takes care to make sure no API exists that provides uncontrollable access to them.}]


@section{Constructing Syntax Property Bundles}


@defproc[(syntax-property-bundle? [v any/c]) boolean?]{
 A predicate that recognizes @tech{syntax property bundles}.}


@defstruct*[syntax-property-entry ([path syntax-path?] [key interned-symbol?] [value any/c])
            #:transparent]{
 A @deftech{syntax property entry} pairs a syntax property key and value with the
 @tech{syntax path} of the subform that the property belongs to. Bundles are built out of these
 entries.}


@defproc[(syntax-property-bundle [entry syntax-property-entry?] ...) syntax-property-bundle?]{
 Constructs a @tech{syntax property bundle} containing each @racket[entry]. The entries may be given
 in any order, but no two entries may share both a path and a key --- a contract error is raised if
 they do.}


@defproc[(sequence->syntax-property-bundle [entries (sequence/c syntax-property-entry?)])
         syntax-property-bundle?]{
 Like @racket[syntax-property-bundle], but the entries are supplied as a sequence instead of as
 individual arguments.}


@defthing[into-syntax-property-bundle (reducer/c syntax-property-entry? syntax-property-bundle?)]{
 A @tech[#:doc '(lib "rebellion/main.scrbl")]{reducer} that collects a sequence of
 @tech{syntax property entries} into a @tech{syntax property bundle}. Like
 @racket[syntax-property-bundle], no two reduced entries may share both a path and a key.}


@defthing[property-hashes-into-syntax-property-bundle
          (reducer/c (entry/c syntax-path? (hash/c interned-symbol? any/c #:immutable #true))
                     syntax-property-bundle?)]{
 A @tech[#:doc '(lib "rebellion/main.scrbl")]{reducer} that collects a sequence of
 @racket[entry] values, each mapping a @tech{syntax path} to a hash of the properties at that path,
 into a @tech{syntax property bundle}. Entries with empty property
 hashes are ignored. Each path may occur at most once in the reduced sequence --- unlike
 @racket[into-syntax-property-bundle], this reducer does not merge multiple entries that refer to
 the same path, and instead raises a contract error. This is because this reducer is intended to be
 used when a syntax object and its subforms are iterated, and each syntax object's properties are all
 extracted at once using @racket[syntax-property-symbol-keys]. Such a client would expect to never
 iterate two syntax objects with the same syntax path, even if they had disjoint property keys.}


@section{Querying Syntax Property Bundles}


@;TODO: claude, that failure-result behavior is definitely not what I intended. Please fix
@; it with a hidden sentinel value (i.e. a gensym).
@defproc[(syntax-property-bundle-get-property [bundle syntax-property-bundle?]
                                              [path syntax-path?]
                                              [key interned-symbol?]
                                              [failure-result failure-result/c #false])
         any/c]{
 Returns the value of the property with key @racket[key] at @racket[path] within @racket[bundle].
 If no such property exists, then @racket[failure-result] determines the result: if it's a
 procedure, it's called with no arguments to produce the result, and otherwise it's returned
 directly, following the same protocol as @racket[hash-ref]. If @racket[failure-result] is omitted
 or @racket[#false], a contract error is raised instead. Note that this means @racket[#false]
 cannot be used directly as a failure result --- use @racket[(λ () #false)] to make a missing
 property produce @racket[#false].}


@;TODO: claude, what does this do if the bundle doesn't contain path? I think it should return an
@; empty hash rather than error, similar to how I handle this case in Rebellion's multimaps. That
@; should be documented explicitly too.
@defproc[(syntax-property-bundle-get-immediate-properties [bundle syntax-property-bundle?]
                                                          [path syntax-path?])
         immutable-hash?]{
 Returns a hash of every property in @racket[bundle] located at exactly @racket[path], mapping
 property keys to property values. Properties located at @bold{descendants} of @racket[path] are
 @bold{not} included. Returns an empty hash if @racket[bundle] contains no properties at
 @racket[path].}


@;TODO: claude, same question as above.
@defproc[(syntax-property-bundle-get-all-properties [bundle syntax-property-bundle?]
                                                    [path syntax-path?])
         syntax-property-bundle?]{
 Returns a @tech{syntax property bundle} of every property in @racket[bundle] located at
 @racket[path] or at any descendant of @racket[path]. The paths of the returned bundle are made
 relative to @racket[path], as though by @racket[syntax-path-remove-prefix]: properties located at
 exactly @racket[path] appear at @racket[root-syntax-path] in the returned bundle. Passing
 @racket[root-syntax-path] returns @racket[bundle] unchanged.}


@;TODO: claude, since keys are always interned symbols, should they maybe be represented with a sorted
@; map instead of a hash? Would there be a point to that? I don't think I have any use cases where I
@; care about "all syntax properties with key names less than or greater than key 'foo"
@defproc[(syntax-property-bundle-entries [bundle syntax-property-bundle?])
         (sequence/c syntax-property-entry?)]{
 Returns a lazy sequence of every @tech{syntax property entry} in @racket[bundle]. Entries are
 produced in ascending order of their paths, in the sense of @racket[syntax-path<=>]. The relative
 order of multiple entries at the same path is unspecified.}


@defproc[(syntax-property-bundle-as-map [bundle syntax-property-bundle?]) immutable-sorted-map?]{
 Returns a view of @racket[bundle] as a sorted map whose keys are @tech{syntax paths}, ordered by
 @racket[syntax-path<=>], and whose values are hashes mapping property keys to property values.
 Only paths with at least one property appear in the map.}


@section{Moving Properties Between Bundles and Syntax Objects}


@defproc[(syntax-immediate-properties [stx syntax?]
                                      [#:base-path base-path syntax-path? root-syntax-path])
         syntax-property-bundle?]{
 Returns a @tech{syntax property bundle} of the properties attached directly to @racket[stx],
 ignoring any properties attached to its subforms. Only properties whose keys are interned symbols
 are extracted, as determined by @racket[syntax-property-symbol-keys]. The extracted properties are
 located at @racket[base-path] in the returned bundle, which is useful when @racket[stx] is itself
 a subform of some larger syntax object.}


@defproc[(syntax-all-properties [stx syntax?]
                                [#:base-path base-path syntax-path? root-syntax-path])
         syntax-property-bundle?]{
 Returns a @tech{syntax property bundle} of the properties attached to @racket[stx] and to every
 subform within it. Only properties whose keys are interned symbols are extracted, as determined by
 @racket[syntax-property-symbol-keys]. Each extracted property is located at the @tech{syntax path}
 of the subform it was attached to, prefixed with @racket[base-path]. Subforms of hash datums are
 not traversed, as syntax paths cannot refer to them --- see @secref["original-syntax-paths"] for
 further explanation.}


@defproc[(syntax-add-all-properties [stx syntax?] [bundle syntax-property-bundle?]) syntax?]{
 Returns a copy of @racket[stx] in which, for each @tech{syntax property entry} in @racket[bundle],
 the subform at the entry's path is given the entry's property key and value as though by
 @racket[syntax-property]. Properties already attached to @racket[stx] and its subforms are
 retained, except where an entry in @racket[bundle] overwrites them. Every path in @racket[bundle]
 must refer to a subform of @racket[stx], in the sense of @racket[syntax-contains-path?] ---
 otherwise a contract error is raised.}
