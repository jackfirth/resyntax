#lang scribble/manual


@title[#:tag "grimoire" #:style 'toc]{The Resyntax Grimoire}

Resyntax's implementation is complex. This document serves as a reference manual for many of the
internal libraries and abstractions contained within Resyntax. @bold{The APIs documented here are
 unstable and not meant for public consumption at this time.} This grimoire is intended for those
seeking to understand how Resyntax operates under the hood. Danger awaits those who come to rely
programmatically on anything found here.

@local-table-of-contents[]


@include-section[(lib "resyntax/grimoire/source.scrbl")]
@include-section[(lib "resyntax/grimoire/source-group.scrbl")]
@include-section[(lib "resyntax/grimoire/syntax-path.scrbl")]
@include-section[(lib "resyntax/grimoire/syntax-property-bundle.scrbl")]
@include-section[(lib "resyntax/grimoire/expansion-analyzers.scrbl")]
@include-section[(lib "resyntax/grimoire/string-replacement.scrbl")]
