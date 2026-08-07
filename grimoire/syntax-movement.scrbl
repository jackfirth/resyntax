#lang scribble/manual


@(require (for-label racket/base
                     racket/contract/base
                     rebellion/collection/sorted-map
                     rebellion/collection/sorted-set
                     resyntax/grimoire/source
                     resyntax/grimoire/syntax-movement
                     resyntax/grimoire/syntax-path)
          scribble/example
          (submod resyntax/private/scribble-evaluator-factory doc))


@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'rebellion/collection/sorted-map
                   'resyntax/grimoire/source
                   'resyntax/grimoire/syntax-movement
                   'resyntax/grimoire/syntax-path)
    #:private (list 'racket/base)))


@title[#:tag "syntax-movement"]{Syntax Movement Tables}
@defmodule[resyntax/grimoire/syntax-movement]

A @deftech{syntax movement table} records where each piece of a program's original syntax ended up
after the program was transformed, as a mapping between @tech{syntax paths}. The table's keys are
@tech{original syntax paths} --- the paths that subforms of the untransformed program were located
at when it was first read, as recorded by the @racket['original-syntax-path] property that
@racket[source-read-syntax] attaches. The table's values are sets of paths identifying every
location in the @emph{transformed} program where a subform claiming that original path can be
found. Resyntax primarily builds movement tables for fully expanded programs, in which case the
table describes where the macro expander moved each piece of the original program.

@; TODO(@notjack.space): broader overview goes here. Why movement tables exist: the role they play
@;  in the analysis pipeline (translating expansion analyzer findings and expansion-time lexical
@;  context back onto the original unexpanded program) and the role they're expected to play in the
@;  pluggable whole-module analyzer system.

Each original path maps to a @emph{set} of paths in the transformed program, rather than to a
single path, because a single piece of original syntax can end up in several places at once ---
macros are free to copy their subforms. A @racket[struct] definition, for example, copies the
struct name into the several identifiers it defines, so the original path of the name maps to
every expanded location those copies ended up at. The reverse situation also occurs: original
syntax that the transformation discarded entirely appears nowhere in the transformed program, and
its path is simply absent from the table's keys.

@; TODO(@notjack.space): this one-to-many ambiguity is the crux of a lot of behavior worth
@;  explaining in your own words: when several expanded forms claim the same original path,
@;  Resyntax's analysis refuses to pick a winner, discarding expansion analyzer properties and
@;  expansion-time lexical context for that path (the "multiple expanded forms claim to originate
@;  from that path" log messages). Worth mentioning how that policy blocks things like issue #688
@;  and what a more principled disambiguation might look like.

@; TODO(@notjack.space): possibly also worth hinting here: movement tables currently describe what
@;  the macro expander did, but the same input-paths-to-output-paths shape describes what a
@;  refactoring rule's transformation did. If tables are ever built for rule outputs, they'd be the
@;  basis for automatically matching up input and output shape tags in UTS syntax deltas.


@defproc[(syntax-movement-table [result-stx syntax?]) immutable-sorted-map?]{
 Builds a @tech{syntax movement table} for @racket[result-stx] by traversing it and collecting
 every subform that carries an @tech{original syntax path}, including subforms nested within other
 collected subforms. The returned table is an immutable sorted map whose keys are the original
 syntax paths that were found, ordered by @racket[syntax-path<=>], and whose values are immutable
 sorted sets of the paths within @racket[result-stx] at which the claiming subforms are located.

 @(examples
   #:eval (make-evaluator) #:once
   (eval:no-prompt
    (define expanded (source-expand (string-source "#lang racket/base\n(void)\n"))))
   (syntax-movement-table expanded))

 When the transformation copies a piece of original syntax into several places, every copy's
 location appears in that original path's set:

 @(examples
   #:eval (make-evaluator) #:once
   (eval:no-prompt
    (define expanded
      (source-expand (string-source "#lang racket/base\n(struct point (x y))\n")))
    (define table (syntax-movement-table expanded)))
   (code:comment "The original path of the identifier `point` within `(struct point (x y))`:")
   (define point-path (syntax-path (list 3 1 1)))
   (code:comment "Expansion copied that identifier into four different definitions.")
   (sorted-map-get table point-path))}
