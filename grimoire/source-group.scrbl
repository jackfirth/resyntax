#lang scribble/manual


@(require scribble/example
          (for-label pkg/lib
                     racket/base
                     racket/contract/base
                     racket/path
                     racket/sequence
                     rebellion/base/comparator
                     rebellion/base/range
                     rebellion/collection/range-set
                     resyntax/grimoire/source-group
                     resyntax/grimoire/source)
          (submod resyntax/private/scribble-evaluator-factory doc))


@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'resyntax/grimoire/source-group
                   'resyntax/grimoire/source
                   'rebellion/base/comparator
                   'rebellion/base/range
                   'rebellion/collection/range-set)
    #:private (list 'racket/base)))


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

Additionally, any number of source groups can be combined into a single group with
@racket[source-group-union]. This is how the command-line interface handles multiple target flags:
each flag becomes a source group, and all of them are unioned into one group describing the entire
analysis.

A source group is only a description: it must be @emph{resolved} with @racket[source-group-resolve]
to produce the actual @tech{source code} values that Resyntax analyzes. Resolution is when the
filesystem, the local package system, or the local Git repository is actually consulted. Resolution
does not consult external networked sources; only local information is considered. After resolution,
Resyntax "locks in" the set of sources it's editing. If, after this point, new files are added to a
directory group (or a similar addition is made to the files described by a different kind of source
group) they will be ignored by Resyntax. However, edits to the @emph{contents} of files that were
included in the source set, but which Resyntax has @emph{not} started to analyze, will be perceived by
Resyntax. This is because source group resolution does not read the contents of each source file into
memory yet. That occurs at a later step, on a per-file basis, as Resyntax is analyzing each file.


@defproc[(source-group? [v any/c]) boolean?]{
 A predicate that recognizes @tech{source groups} of any kind.

 @(examples
   #:eval (make-evaluator) #:once
   (source-group? (directory-source-group "/tmp/my-project"))
   (source-group? empty-source-group)
   (source-group? "/tmp/my-project"))}


@defthing[empty-source-group source-group?]{
 The empty @tech{source group}, which specifies no sources at all. Resolving it produces an empty
 hash, and unioning it with any other source group has no effect --- it is the identity element of
 @racket[source-group-union].

 @(examples
   #:eval (make-evaluator) #:once
   (source-group-resolve empty-source-group)
   (source-group-union empty-source-group (directory-source-group "/tmp/my-project")))}


@defproc[(source-group-union [group source-group?] ...) source-group?]{
 Combines each @racket[group] into a single @tech{source group} specifying all of their sources.
 Given no groups, the result is @racket[empty-source-group].

 Unioning is commutative, associative, and idempotent, and @racket[empty-source-group] is its
 identity element: source groups form a commutative monoid under union (in fact, a bounded
 join-semilattice, thanks to idempotence). These laws hold up to @racket[equal?]:

 @itemlist[
  @item{@racket[(source-group-union _g1 _g2)] is always @racket[equal?] to
   @racket[(source-group-union _g2 _g1)].}

  @item{@racket[(source-group-union (source-group-union _g1 _g2) _g3)] is always @racket[equal?] to
   @racket[(source-group-union _g1 (source-group-union _g2 _g3))].}

  @item{@racket[(source-group-union _g _g)] and @racket[(source-group-union _g empty-source-group)]
   are both always @racket[equal?] to @racket[_g].}]

 This operation is a convenience wrapper around @racket[source-group-union-all].

 @(examples
   #:eval (make-evaluator) #:once
   (define project (directory-source-group "/tmp/my-project"))
   (define pkg (package-source-group "my-package"))
   (define main
     (single-source-group "/tmp/other/main.rkt"
                          (range-set (closed-open-range 1 20 #:comparator natural<=>))))
   (eval:check (equal? (source-group-union project pkg) (source-group-union pkg project)) #true)
   (eval:check (equal? (source-group-union (source-group-union project pkg) main)
                       (source-group-union project (source-group-union pkg main)))
               #true)
   (eval:check (equal? (source-group-union project project) project) #true)
   (eval:check (equal? (source-group-union project empty-source-group) project) #true)
   (eval:check (equal? (source-group-union) empty-source-group) #true)
   (source-group-union project))}


@defproc[(source-group-union-all [groups (sequence/c source-group?)]) source-group?]{
 Combines every source group in @racket[groups] into a single @tech{source group}, exactly as
 @racket[source-group-union] does for its arguments, but accepting the groups as a single sequence
 of any kind. An empty sequence produces @racket[empty-source-group]. This is how the
 @seclink["cli"]{command-line interface} combines its collection of target flags into one group.

 @(examples
   #:eval (make-evaluator) #:once
   (define project (directory-source-group "/tmp/my-project"))
   (define pkg (package-source-group "my-package"))
   (eval:check (equal? (source-group-union-all (list project pkg))
                       (source-group-union project pkg))
               #true)
   (eval:check (equal? (source-group-union-all (vector project pkg))
                       (source-group-union project pkg))
               #true)
   (eval:check (equal? (source-group-union-all (list)) empty-source-group) #true))}


@defproc[(single-source-group [path path-string?] [lines immutable-range-set?])
         source-group?]{
 Constructs a @tech{source group} containing only the file at @racket[path], with suggestions
 restricted to the line numbers in @racket[lines]. The path is normalized with
 @racket[simple-form-path] upon construction.

 @(examples
   #:eval (make-evaluator) #:once
   (single-source-group "/tmp/my-project/main.rkt"
                        (range-set (closed-open-range 1 20 #:comparator natural<=>))))}


@defproc[(directory-source-group [path path-string?]) source-group?]{
 Constructs a @tech{source group} containing every file within the directory at @racket[path],
 including files within subdirectories, with all lines of each file eligible for suggestions. The
 path is normalized with @racket[simple-form-path] upon construction.

 @(examples
   #:eval (make-evaluator) #:once
   (directory-source-group "/tmp/my-project"))}


@defproc[(package-source-group [package-name string?]) source-group?]{
 Constructs a @tech{source group} containing every file of the installed Racket package named
 @racket[package-name], with all lines of each file eligible for suggestions. The package's
 installation directory is located with @racket[pkg-directory] during resolution, and resolution
 raises a user error if no such package is installed.

 @(examples
   #:eval (make-evaluator) #:once
   (package-source-group "my-package"))}


@defproc[(git-repository-source-group [repository-path path-string?] [base-ref string?])
         source-group?]{
 Constructs a @tech{source group} containing the files of the Git repository at
 @racket[repository-path] that have changed relative to @racket[base-ref], as determined by
 @exec{git diff} during resolution. The repository path is normalized with
 @racket[simple-form-path] upon construction.

 Only the modified lines of each changed file are eligible for suggestions, expanded to include the
 three lines before and after each modified region. The three-line margin matches what GitHub
 allows in pull request reviews: comments may only be placed on modified lines and the three lines
 of context surrounding them, so suggestions within the margin can still be posted as review
 comments. More generally, a three-line margin is the default amount of extra context that many Unix
 tools choose when interoperating via the unified diff format, particularly the
 @hyperlink["https://en.wikipedia.org/wiki/Diff#Unified_format"]{diff} tool.

 @(examples
   #:eval (make-evaluator) #:once
   (git-repository-source-group "/tmp/my-project" "main"))}


@defproc[(source-group-resolve [group source-group?])
         (hash/c file-source? immutable-range-set?)]{
 Resolves @racket[group] into concrete files, returning a hash whose keys are @racket[file-source?]
 values and whose values are the line numbers eligible for suggestions in each file. When the same
 file is included multiple times by a unioned group, its line sets are unioned.

 @(examples
   #:eval (make-evaluator) #:once
   (source-group-resolve
    (single-source-group "/tmp/my-project/main.rkt"
                         (range-set (closed-open-range 1 20 #:comparator natural<=>))))
   (source-group-resolve
    (source-group-union
     (single-source-group "/tmp/my-project/main.rkt"
                          (range-set (closed-open-range 1 20 #:comparator natural<=>)))
     (single-source-group "/tmp/my-project/main.rkt"
                          (range-set (closed-open-range 50 60 #:comparator natural<=>))))))

 Resolution discards all files that don't have the @exec{.rkt} extension. This is where the
 @seclink["cli"]{command-line interface}'s restriction to @exec{.rkt} files is implemented.

 @(examples
   #:eval (make-evaluator) #:once
   (source-group-resolve
    (single-source-group "/tmp/my-project/notes.txt"
                         (range-set (closed-open-range 1 20 #:comparator natural<=>)))))}
