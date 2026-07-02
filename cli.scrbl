#lang scribble/manual


@(require (for-label racket/base
                     resyntax/base)
          scribble/bnf)


@title[#:tag "cli"]{The Resyntax Command-Line Interface}


Resyntax provides a command-line @exec{resyntax} tool for analyzing and refactoring code. The tool has
two commands: @exec{resyntax analyze} for analyzing code without changing it, and @exec{resyntax fix}
for fixing code by applying Resyntax's suggestions.

Note that at present, Resyntax is limited in what files it can fix. Resyntax only analyzes files with
the @exec{.rkt} extension where @tt{#lang racket/base} is the first line in the file.


@section[#:tag "install"]{Installation}

Use the Racket package manager to install Resyntax in the installation scope:

@verbatim{
 % raco pkg install --installation resyntax
}

The @exec{--installation} flag (shorthand for @exec{--scope installation}) installs packages for
all users of a Racket installation, ensuring @exec{resyntax} is in your @envvar{PATH}.

e.g.
@verbatim{
 % resyntax analyze --file example.rkt
 resyntax: --- analyzing code ---
 resyntax: --- displaying results ---
 %
}


@section{Running @exec{resyntax analyze}}


The @exec{resyntax analyze} command accepts flags for specifying what modules to analyze. After
analysis, suggestions are printed in the console. Any of the following flags can be specified any
number of times:


@itemlist[

 @item{@exec{--file} @nonterm{file-path} --- A file to analyze.}

 @item{@exec{--directory} @nonterm{directory-path} --- A directory to analyze, including
  subdirectories.}

 @item{@exec{--package} @nonterm{package-name} --- An installed package to analyze.}

 @item{@exec{--local-git-repository} @nonterm{repository-path} @nonterm{base-ref} --- A local Git
  repository to analyze the changed files of. Only files which have changed relative to
  @nonterm{base-ref} are analyzed. Base references must be given in the form
  @exec{remotename/branchname}, for example @exec{origin/main} or @exec{upstream/my-feature-branch}.}

 @item{@exec{--refactoring-suite} @nonterm{module-path} @nonterm{suite-name} --- A
  @tech{refactoring suite} to use instead of Resyntax's default recommendations. Custom refactoring
  suites can be created with @racket[define-refactoring-suite].}]


@section{Running @exec{resyntax fix}}


The @exec{resyntax fix} command accepts the same flags as @exec{resyntax analyze} for specifying what
modules to fix. After analysis, fixes are applied and a summary is printed.


@itemlist[

 @item{@exec{--file} @nonterm{file-path} --- A file to fix.}

 @item{@exec{--directory} @nonterm{directory-path} --- A directory to fix, including
  subdirectories.}

 @item{@exec{--package} @nonterm{package-name} --- An installed package to fix.}

 @item{@exec{--local-git-repository} @nonterm{repository-path} @nonterm{base-ref} --- A local Git
  repository to fix the changed files of. Only files which have changed relative to @nonterm{base-ref}
  are fixed. Base references must be given in the form @exec{remotename/branchname}, for example
  @exec{origin/main} or @exec{upstream/my-feature-branch}.}

 @item{@exec{--refactoring-suite} @nonterm{module-path} @nonterm{suite-name} --- A
  @tech{refactoring suite} to use instead of Resyntax's default recommendations. Custom refactoring
  suites can be created with @racket[define-refactoring-suite].}]


If two suggestions try to fix the same code, one of them will be rejected. At present, the best way to
handle overlapping fixes is to run Resyntax multiple times until no fixes are rejected.
