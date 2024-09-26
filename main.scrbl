#lang scribble/manual


@(require (for-label racket/base
                     resyntax/base
                     resyntax/default-recommendations
                     syntax/parse
                     syntax/parse/define)
          scribble/bnf
          scribble/example
          (submod resyntax/private/scribble-evaluator-factory doc))


@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'resyntax/base
                   'syntax/parse)
    #:private (list 'racket/base)))


@title{Resyntax}
@defmodule[resyntax]


Resyntax is a refactoring tool for Racket. The tool can be guided by @deftech{refactoring rules},
which are macro-like functions defined in terms of @racket[syntax-parse] that specify how to search
for and refactor different coding patterns. Resyntax comes with a standard set of refactoring rules
that improve code written in @racket[@#,hash-lang[] @#,racketmodname[racket]] or
@racket[@#,hash-lang[] @#,racketmodname[racket/base]]. For example, consider the following program:

@(racketmod
  #:file "my-program.rkt"
  racket/base
  
  (define (swap x y)
    (let ([t (unbox x)])
      (set-box! x (unbox y))
      (set-box! y t))))

This program uses @racket[let] unnecessarily. The @racket[let] expression can be replaced with a
@racket[define] form, reducing the indentation of the code. Resyntax is capable of detecting and
automatically fixing this issue. Running @exec{resyntax fix --file my-program.rkt} rewrites the above
to the following:

@(racketmod
  #:file "my-program.rkt"
  racket/base
  
  (define (swap x y)
    (define t (unbox x))
    (set-box! x (unbox y))
    (set-box! y t)))

To see a list of suggestions that Resyntax would apply, use @exec{resyntax analyze} instead of
@exec{resyntax fix}. Each suggestion includes an explanation of why the change is being recommended.


@table-of-contents[]


@(define github-repository-url "https://github.com/jackfirth/resyntax/")

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


@section[#:tag "cli"]{The Resyntax Command-Line Interface}


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

 @item{@exec{--package} @nonterm{package-name} --- An installed package to analyze.}

 @item{@exec{--local-git-repository} @nonterm{repository-path} @nonterm{base-ref} --- A local Git
  repository to analyze the changed files of. Only files which have changed relative to
  @nonterm{base-ref} are analyzed. Base references must be given in the form
  @exec{remotename/branchname}, for example @exec{origin/main} or @exec{upstream/my-feature-branch}.}

 @item{@exec{--refactoring-suite} @nonterm{module-path} @nonterm{suite-name} --- A
  @tech{refactoring suite} to use instead of Resyntax's default recommendations. Custom refactoring
  suites can be created with @racket[define-refactoring-suite].}]


@subsection{Running @exec{resyntax fix}}


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


@section{Refactoring Rules and Suites}
@defmodule[resyntax/base]


Resyntax derives its suggestions from @tech{refactoring rules}, which can be grouped into a
@deftech{refactoring suite}. Resyntax ships with a default refactoring suite consisting of many rules
that cover various scenarios related to Racket's standard libraries. However, you may also define your
own refactoring suite and rules using the forms below. Knowledge of Racket macros, and of
@racket[syntax-parse] in particular, is especially useful for understanding how to create effective
refactoring rules.


@defproc[(refactoring-rule? [v any/c]) boolean?]{
 A predicate that recognizes @tech{refactoring rules}.}


@defproc[(refactoring-suite? [v any/c]) boolean?]{
 A predicate that recognizes @tech{refactoring suites}.}


@defform[(define-refactoring-rule id
           #:description description
           parse-option ...
           syntax-pattern
           pattern-directive ...
           template)
         #:contracts ([description string?])]{

 Defines a @tech{refactoring rule} named @racket[id]. Refactoring rules are defined in terms of
 @racket[syntax-parse]. The rule matches syntax objects that match @racket[syntax-pattern], and
 @racket[template] is a @racket[syntax] template that defines what the matched code is refactored
 into. The message in @racket[description] is presented to the user when Resyntax makes a suggestion
 based on the rule. Refactoring rules function roughly like macros defined with
 @racket[define-syntax-parse-rule]. For example, here is a simple rule that flattens nested
 @racket[or] expressions:

 @(examples
   #:eval (make-evaluator) #:once
   (define-refactoring-rule nested-or-to-flat-or
     #:description "This nested `or` expression can be flattened."
     #:literals (or)
     (or a (or b c))
     (or a b c)))

 Like @racket[syntax-parse] and @racket[define-syntax-parse-rule],
 @tech[#:doc '(lib "syntax/scribblings/syntax.scrbl")]{pattern directives} can be used to aid in
 defining rules. Here is a rule that uses the @racket[#:when] directive to only refactor @racket[or]
 expressions that have a duplicate condition:

 @(examples
   #:eval (make-evaluator) #:once
   (define-refactoring-rule or-with-duplicate-subterm
     #:description "This `or` expression has a duplicate subterm."
     #:literals (or)
     (or before ... a:id between ... b:id after ...)
     #:when (free-identifier=? #'a #'b)
     (or before ... a between ... after ...)))}


@defform[(define-refactoring-suite id rules-list suites-list)

         #:grammar
         [(rules-list (code:line)
                      (code:line #:rules (rule ...)))
          (suites-list (code:line)
                       (code:line #:suites (suite ...)))]

         #:contracts ([rule refactoring-rule?]
                      [suite refactoring-suite?])]{

 Defines a @tech{refactoring suite} named @racket[id] containing each listed @racket[rule].
 Additionally, each @racket[suite] provided has its rules added to the newly defined suite.

 @(examples
   #:eval (make-evaluator) #:once
   (eval:alts
    (define-refactoring-suite my-suite
      #:rules (rule1 rule2 rule3)
      #:suites (subsuite1 subsuite2))
    (void)))}


@subsection{Resyntax's Default Rules}
@defmodule[resyntax/default-recommendations]


@(define default-recommendations-directory-link
   "https://github.com/jackfirth/resyntax/tree/master/default-recommendations")

@defthing[default-recommendations refactoring-suite?]{
 The refactoring suite containing all of Resyntax's default refactoring rules. These rules are further
 broken up into subsuites, with each subsuite corresponding to a module within the
 @racketmodname[resyntax/default-recommendations] collection. For example, all of Resyntax's rules
 related to @racket[for] loops are located in the
 @racketmodfont{resyntax/default-recommendations/for-loop-shortcuts} module. See
 @hyperlink[default-recommendations-directory-link]{this directory} for all of Resyntax's default
 refactoring rules.}


@subsection{What Makes a Good Refactoring Rule?}


If you'd like to add a new @tech{refactoring rule} to Resyntax, there are a few guidelines to keep in
mind:

@itemlist[

 @item{Refactoring rules should be @emph{safe}. Resyntax shouldn't break users' code, and it shouldn't
  require careful review to determine whether a suggestion from Resyntax is safe to apply. It's better
  for a rule to never make suggestions than to occasionally make broken suggestions.}

 @item{Refactoring rules can be shown to many different developers in a wide variety of different
  contexts. Therefore, it's important that Resyntax's default recommendations have some degree of
  @emph{consensus} among the Racket community. Highly divisive suggestions that many developers
  disagree with are not a good fit for Resyntax. Discussing your rule with the Racket community prior
  to developing it is encouraged, especially if it's likely to affect a lot of code. If necessary,
  consider narrowing the focus of your rule to just the cases that everybody agrees are clear
  improvements.}

 @item{Refactoring rules should @emph{explain themselves}. The description of a refactoring rule (as
  specified with the @racket[#:description] option) should state why the new code is an improvement
  over the old code. Refactoring rule descriptions are shown to Resyntax users at the command line, in
  GitHub pull request review comments, and in Git commit messages. The description is the only means
  you have of explaining to a potentially confused stranger why Resyntax wants to change their code,
  so make sure you use it!}

 @item{Refactoring rules should focus on cleaning up @emph{real-world code}. A refactoring rule that
  suggests improvements to hypothetical code that no human would write in the first place is not
  useful. Try to find examples of code "in the wild" that the rule would improve. The best candidates
  for new rules tend to be rules that help Racketeers clean up and migrate old Scheme code that
  doesn't take advantage of Racket's unique features and extensive standard library.}

 @item{Refactoring rules should try to preserve the @emph{intended behavior} of the refactored code,
  but not necessarily the @emph{actual behavior}. For instance, a rule that changes how code handles
  some edge case is acceptable if the original behavior of the code was likely confusing or surprising
  to the developer who wrote it. This is a judgment call that requires understanding what the original
  code communicates clearly and what it doesn't. A rule's @racket[#:description] is an excellent place
  to draw attention to potentially surprising behavior changes.}

 @item{Refactoring rules should be @emph{self-contained}, meaning they can operate locally on a single
  expression. Refactoring rules that require whole-program analysis are not a good fit for Resyntax,
  nor are rules that require global knowledge of the whole codebase.}]
