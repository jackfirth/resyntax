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

A @deftech{syntax property bundle} is an immutable collection of
@tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{syntax properties} that has been
detached from any particular syntax object. Each property in a bundle is addressed by the
combination of a @tech{syntax path}, identifying the subform the property belongs to, and a property
key. A bundle contains at most one value for each path and key combination.

Ordinarily, syntax properties live directly on syntax objects. Representing them separately as plain
data lets Resyntax manipulate the properties themselves: bundles can be filtered, merged, translated
from one syntax object's paths to another's, and inspected without ever touching a syntax object.
Resyntax's @tech{expansion analyzers} work this way --- each analyzer examines a fully expanded program and
reports its findings as a syntax property bundle whose paths refer to subforms of the expanded
program. Resyntax then translates those paths back to the corresponding locations in the original
unexpanded program and grafts the translated bundle onto the unexpanded syntax object with
@racket[syntax-add-all-properties], making the analyzers' findings visible to
@tech{refactoring rules}.

Although syntax objects themselves permit arbitrary property keys, property keys in a bundle are
required to be interned symbols. The interned-symbol-keyed portion of a syntax object's property
table is the only portion that Racket exposes via @racket[syntax-property-symbol-keys], so it is
the only portion that bundles can faithfully represent.


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
 the same path, and instead raises a contract error.}


@section{Querying Syntax Property Bundles}


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


@defproc[(syntax-property-bundle-get-immediate-properties [bundle syntax-property-bundle?]
                                                          [path syntax-path?])
         immutable-hash?]{
 Returns a hash of every property in @racket[bundle] located at exactly @racket[path], mapping
 property keys to property values. Properties located at descendants of @racket[path] are not
 included. Returns an empty hash if @racket[bundle] contains no properties at @racket[path].}


@defproc[(syntax-property-bundle-get-all-properties [bundle syntax-property-bundle?]
                                                    [path syntax-path?])
         syntax-property-bundle?]{
 Returns a @tech{syntax property bundle} of every property in @racket[bundle] located at
 @racket[path] or at any descendant of @racket[path]. The paths of the returned bundle are made
 relative to @racket[path], as though by @racket[syntax-path-remove-prefix]: properties located at
 exactly @racket[path] appear at @racket[root-syntax-path] in the returned bundle. Passing
 @racket[root-syntax-path] returns @racket[bundle] unchanged.}


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
