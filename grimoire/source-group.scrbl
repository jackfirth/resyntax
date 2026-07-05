#lang scribble/manual


@(require (for-label pkg/lib
                     racket/base
                     racket/contract/base
                     racket/path
                     racket/sequence
                     rebellion/collection/range-set
                     resyntax/grimoire/source-group
                     resyntax/grimoire/source))


@title[#:tag "source-group"]{Source Groups}
@defmodule[resyntax/grimoire/source-group]

A @deftech{source group} is a specification of what @tech{source code} Resyntax should analyze, along
with which lines within those sources Resyntax is allowed to suggest changes to. Source groups come in
four kinds, each corresponding to one of the target flags accepted by the
@seclink["cli"]{command-line interface}:

@itemlist[
 @item{@emph{Single-source groups}, constructed with @racket[single-source-group], containing one
  file restricted to a given set of lines. The @exec{--file} flag constructs these, with all lines
  allowed. Note that the CLI doesn't include a way to specify which lines should be modified at this
 time, despite the fact that the @racket[single-source-group] constructor accepts that information.
 The only difference between a single-source group and a @racket[file-source?] value is that the
 source group may contain information about which lines to analyze.}

 @item{@emph{Directory groups}, constructed with @racket[directory-source-group], containing every
  source file within a directory, including files in subdirectories. The @exec{--directory} flag
  constructs these.}

 @item{@emph{Package groups}, constructed with @racket[package-source-group], containing every file
  of a @emph{locally installed} Racket package. The @exec{--package} flag constructs these. This does
 @bold{not} refer to remote packages on the package catalog; Resyntax cannot analyze a package unless
 it's currently installed.}

 @item{@emph{Git repository groups}, constructed with @racket[git-repository-source-group],
  containing the files of a @emph{local} Git repository that have changed relative to some base
  reference. The @exec{--local-git-repository} flag constructs these. As with package groups,
  Resyntax can only analyze Git repositories that have already been cloned onto the current machine.
  Git repository groups are the only source groups that take advantage of Resyntax's ability to
  restrict which lines are analyzed --- only the lines actually touched in the diff against the
  specified base reference, plus a small margin of surrounding context lines (see
  @racket[git-repository-source-group]), will be included.}]

A source group is only a description: it must be @emph{resolved} with @racket[source-groups-resolve]
to produce the actual @tech{source code} values that Resyntax analyzes. Resolution is when the
filesystem, the local package system, or the local Git repository is actually consulted. Resolution
does not consult external networked sources; only local information is considered. After resolution,
Resyntax "locks in" the set of sources it's editing. If, after this point, new files are added to a
directory group (or a similar edit is made to the files described by a different kind of source group)
they will be ignored by Resyntax. However, edits to files that @emph{were} included in the source set,
but which Resyntax has @emph{not} started to analyze, will be perceived by Resyntax. This is because
source group resolution does not read the @emph{contents} of each source file into memory yet. That
occurs at a later step, on a per-file basis, as Resyntax is analyzing each file.


@defproc[(source-group? [v any/c]) boolean?]{
 A predicate that recognizes @tech{source groups} of any kind.}


@defproc[(single-source-group? [v any/c]) boolean?]{
 A predicate that recognizes single-source groups.}


@defproc[(single-source-group [path path-string?] [lines immutable-range-set?])
         single-source-group?]{
 Constructs a @tech{source group} containing only the file at @racket[path], with suggestions
 restricted to the line numbers in @racket[lines]. The path is normalized with
 @racket[simple-form-path] upon construction.}


@defproc[(directory-source-group? [v any/c]) boolean?]{
 A predicate that recognizes directory groups.}


@defproc[(directory-source-group [path path-string?]) directory-source-group?]{
 Constructs a @tech{source group} containing every file within the directory at @racket[path],
 including files within subdirectories, with all lines of each file eligible for suggestions. The
 path is normalized with @racket[simple-form-path] upon construction.}


@defproc[(package-source-group? [v any/c]) boolean?]{
 A predicate that recognizes package groups.}


@defproc[(package-source-group [package-name string?]) package-source-group?]{
 Constructs a @tech{source group} containing every file of the installed Racket package named
 @racket[package-name], with all lines of each file eligible for suggestions. The package's
 installation directory is located with @racket[pkg-directory] during resolution, and resolution
 raises a user error if no such package is installed.}


@defproc[(git-repository-source-group? [v any/c]) boolean?]{
 A predicate that recognizes Git repository groups.}


@defproc[(git-repository-source-group [repository-path path-string?] [base-ref string?])
         git-repository-source-group?]{
 Constructs a @tech{source group} containing the files of the Git repository at
 @racket[repository-path] that have changed relative to @racket[base-ref], as determined by
 @exec{git diff} during resolution. The repository path is normalized with
 @racket[simple-form-path] upon construction.

 Only the modified lines of each changed file are eligible for suggestions, expanded to include the
 three lines before and after each modified region. The three-line margin matches what GitHub
 allows in pull request reviews: comments may only be placed on modified lines and the three lines
 of context surrounding them, so suggestions within the margin can still be posted as review
 comments.}


@defproc[(source-groups-resolve [groups (sequence/c source-group?)])
         (hash/c file-source? immutable-range-set?)]{
 Resolves each @tech{source group} in @racket[groups] into concrete files, returning a hash whose
 keys are @racket[file-source?] values and whose values are the line numbers eligible for
 suggestions in each file. When multiple groups include the same file, their line sets are unioned.

 Resolution discards all files that don't have the @exec{.rkt} extension. This is where the
 @seclink["cli"]{command-line interface}'s restriction to @exec{.rkt} files is implemented.}

