#lang scribble/manual


@title[#:tag "expansion-analyzers"]{Expansion Analyzers}

An @deftech{expansion analyzer} examines the fully expanded form of a program and reports facts
about it as a @tech{syntax property bundle} whose @tech{syntax paths} refer to subforms of the
expanded program. Resyntax translates those paths back to the corresponding subforms of the
original unexpanded program and attaches the reported properties to them, making information that
is only discoverable after macro expansion available to @tech{refactoring rules}. Expansion
analyzers are an as-yet undocumented work in progress; a future edition of this grimoire will
describe them properly.
