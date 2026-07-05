#lang scribble/manual


@(require (for-label pkg/lib
                     racket/base
                     racket/contract/base
                     racket/path
                     racket/sequence
                     rebellion/collection/range-set
                     resyntax/grimoire/file-group
                     resyntax/grimoire/source))


@title[#:tag "file-group"]{File Groups}
@defmodule[resyntax/grimoire/file-group]

A @deftech{file group} is a specification of a set of files that Resyntax should analyze, along with
which lines within those files Resyntax is allowed to suggest changes to. File groups come in four
kinds, each corresponding to one of the target flags accepted by the @seclink["cli"]{command-line
 interface}:

@itemlist[
 @item{@emph{Single-file groups}, constructed with @racket[single-file-group], containing one file
  restricted to a given set of lines. The @exec{--file} flag constructs these, with all lines
  allowed.}

 @item{@emph{Directory groups}, constructed with @racket[directory-file-group], containing every
  file within a directory, including files in subdirectories. The @exec{--directory} flag
  constructs these.}

 @item{@emph{Package groups}, constructed with @racket[package-file-group], containing every file
  of an installed Racket package. The @exec{--package} flag constructs these.}

 @item{@emph{Git repository groups}, constructed with @racket[git-repository-file-group],
  containing the files of a Git repository that have changed relative to some base reference. The
  @exec{--local-git-repository} flag constructs these.}]

A file group is only a description: it must be @emph{resolved} with @racket[file-groups-resolve] to
produce the actual @tech{source code} values that Resyntax analyzes. Resolution is when the
filesystem, the package catalog, or the Git repository is actually consulted.


@defproc[(file-group? [v any/c]) boolean?]{
 A predicate that recognizes @tech{file groups} of any kind.}


@defproc[(single-file-group? [v any/c]) boolean?]{
 A predicate that recognizes single-file groups.}


@defproc[(single-file-group [path path-string?] [lines immutable-range-set?])
         single-file-group?]{
 Constructs a @tech{file group} containing only the file at @racket[path], with suggestions
 restricted to the line numbers in @racket[lines]. The path is normalized with
 @racket[simple-form-path] upon construction.}


@defproc[(directory-file-group? [v any/c]) boolean?]{
 A predicate that recognizes directory groups.}


@defproc[(directory-file-group [path path-string?]) directory-file-group?]{
 Constructs a @tech{file group} containing every file within the directory at @racket[path],
 including files within subdirectories, with all lines of each file eligible for suggestions. The
 path is normalized with @racket[simple-form-path] upon construction.}


@defproc[(package-file-group? [v any/c]) boolean?]{
 A predicate that recognizes package groups.}


@defproc[(package-file-group [package-name string?]) package-file-group?]{
 Constructs a @tech{file group} containing every file of the installed Racket package named
 @racket[package-name], with all lines of each file eligible for suggestions. The package's
 installation directory is located with @racket[pkg-directory] during resolution, and resolution
 raises a user error if no such package is installed.}


@defproc[(git-repository-file-group? [v any/c]) boolean?]{
 A predicate that recognizes Git repository groups.}


@defproc[(git-repository-file-group [repository-path path-string?] [base-ref string?])
         git-repository-file-group?]{
 Constructs a @tech{file group} containing the files of the Git repository at
 @racket[repository-path] that have changed relative to @racket[base-ref], as determined by
 @exec{git diff} during resolution. The repository path is normalized with
 @racket[simple-form-path] upon construction.

 Only the modified lines of each changed file are eligible for suggestions, expanded to include the
 three lines before and after each modified region. The three-line margin matches what GitHub
 allows in pull request reviews: comments may only be placed on modified lines and the three lines
 of context surrounding them, so suggestions within the margin can still be posted as review
 comments.}


@defproc[(file-groups-resolve [groups (sequence/c file-group?)])
         (hash/c file-source? immutable-range-set?)]{
 Resolves each @tech{file group} in @racket[groups] into concrete files, returning a hash whose
 keys are @racket[file-source?] values and whose values are the line numbers eligible for
 suggestions in each file. When multiple groups include the same file, their line sets are unioned.

 Resolution discards all files that don't have the @exec{.rkt} extension. This is where the
 @seclink["cli"]{command-line interface}'s restriction to @exec{.rkt} files is implemented.}

