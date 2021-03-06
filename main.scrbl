#lang scribble/manual


@(require (for-label syntax/parse)
          scribble/bnf)


@title{Resyntax}
@defmodule[resyntax]


Resyntax is a refactoring tool for Racket. The tool can be guided by @deftech{refactoring rules},
which are macro-like functions defined in terms of @racket[syntax-parse] that specify how to search
for and refactor different coding patterns.

@bold{This tool is extremely experimental.} Do not attempt to incorporate it into your projects yet.
For now, the refactoring suggestions produced by @racketmodname[resyntax] are best viewed as glimpses
into one possible distant future of static analysis for Racket. Feedback, questions, and ideas are all
greatly appreciated and are best directed at the @hyperlink[github-repository-url]{GitHub repository}.


@(define github-repository-url "https://github.com/jackfirth/resyntax/")


@section{The Resyntax Command-Line Interface}


Resyntax provides a command-line @exec{resyntax} tool for analyzing and refactoring code. The tool has
two commands: @exec{resyntax analyze} for analyzing code without changing it, and @exec{resyntax fix}
for fixing code by applying Resyntax's suggestions.

Note that at present, Resyntax is limited in what files it can fix. Resyntax only analyzes files with
the @exec{.rkt} extension where @tt{#lang racket/base} is the first line in file.


@subsection{Running @exec{resyntax analyze}}


The @exec{resyntax analyze} command accepts flags for specifying what modules to analyze. After
analysis, suggestions are printed in the console. Any of the following flags can be specified any
number of times:


@itemlist[

 @item{@exec{--file} @nonterm{file-path} --- A file to anaylze.}

 @item{@exec{--directory} @nonterm{directory-path} --- A directory to anaylze, including
  subdirectories.}

 @item{@exec{--package} @nonterm{package-name} --- An installed package to analyze.}]


@subsection{Running @exec{resyntax fix}}


The @exec{resyntax fix} command accepts the same flags as @exec{resyntax analyze} for specifying what
modules to fix. After analysis, fixes are applied and a summary is printed.


@itemlist[

 @item{@exec{--file} @nonterm{file-path} --- A file to fix.}

 @item{@exec{--directory} @nonterm{directory-path} --- A directory to fix, including
  subdirectories.}

 @item{@exec{--package} @nonterm{package-name} --- An installed package to fix.}]


If two suggestions try to fix the same code, one of them will be rejected. At present, the best way to
handle overlapping fixes is to run Resyntax multiple times until no fixes are rejected.
