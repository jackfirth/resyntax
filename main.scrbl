#lang scribble/manual

@(require (for-label syntax/parse))

@title{Resyntax}
@defmodule[resyntax]

Resyntax is a refactoring tool for Racket. The tool can be guided by @deftech{refactoring rules},
which are macro-like functions defined in terms of @racket[syntax-parse] that specify how to search
for and refactor different coding patterns.

@bold{This tool is extremely experimental.} Do not attempt to incorporate it into your projects yet.
For now, the refactoring suggestions produced by @racketmodname[resyntax] are best viewed as glimpses
into one possible distant future of static analysis for Racket. Feedback, questions, and ideas are all
greatly appreciated and are best directed at the @hyperlink[github-repository-url]{GitHub repository}.

Resyntax does not have anything approaching a public API yet. If you want to actually try using it,
open the source code of the @racketmodname[resyntax] module using DrRacket's "Open Require Path" menu
option, change the file path given to the @racket[refactor-file!] function in the main submodule, then
run it. Choosing a file checked into Git (or another version control system) is highly recommended in
order to make it easier to view the diff and easier to undo the changes.

@(define github-repository-url "https://github.com/jackfirth/resyntax/")
