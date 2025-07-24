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

@defform[(define-definition-context-refactoring-rule id
           #:description description
           parse-option ...
           syntax-pattern
           pattern-directive ...
           template)
         #:contracts ([description string?])]{

 Defines a @tech{refactoring rule} named @racket[id], like @racket[define-refactoring-rule], except
 the rule is applied only in
 @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{internal-definition contexts}. The given
 @racket[syntax-pattern] must be a
 @tech[#:doc '(lib "syntax/scribblings/syntax.scrbl")]{proper head pattern}, and it is expected to
 match the entire sequence of body forms within the definition context. The output @racket[template]
 of the rule should be a single syntax object containing a sequence of refactored body forms. Like
 @racket[define-refactoring-rule], @racket[description] is used to generate a message presented to the
 user, and both @racket[parse-option] and @racket[pattern-directive] function the same as they do in
 @racket[syntax-parse]. For example, here is a simple rule that turns a series of @racket[define]
 forms unpacking a 2D @racket[point] structure into a single @racket[match-define] form:

 @(examples
   #:eval (make-evaluator) #:once
   (eval:no-prompt
    (struct point (x y) #:transparent))

   (define-definition-context-refactoring-rule point-define-to-match-define
     #:description "These definitions can be simplified with `match-define`."
     #:literals (define point-x point-y)
     (~seq body-before ...
           (define x:id (point-x pt:id))
           (define y:id (point-y pt2:id))
           body-after ...)
     #:when (free-identifier=? #'pt #'pt2)
     (body-before ...
      (match-define (point x y) pt)
      body-after ...)))

 Note that by default Resyntax will try to reformat the entire context. To reformat just the forms
 being modified, a few additional steps are required. First, use @racket[~replacement] (or
 @racket[~splicing-replacement]) to annotate which subpart of the context is being replaced:

 @(examples
   #:eval (make-evaluator) #:once
   (define-definition-context-refactoring-rule point-define-to-match-define
     #:description "These definitions can be simplified with `match-define`."
     #:literals (define point-x point-y)
     (~seq body-before ...
           (~and x-def (define x:id (point-x pt:id)))
           (~and y-def (define y:id (point-y pt2:id)))
           body-after ...)
     #:when (free-identifier=? #'pt #'pt2)
     (body-before ...
      (~replacement (match-define (point x y) pt)
                    #:original-splice (x-def y-def))
      body-after ...)))

 This ensures that Resyntax will preserve any comments at the end of @racket[body-before ...] and the
 beginning of @racket[body-after ...]. However, that alone doesn't prevent Resyntax from reformatting
 the whole context. To do that, use the @racket[~focus-replacement-on] metafunction, which tells
 Resyntax that if @emph{only} the focused forms are changed, Resyntax should "shrink" the replacement
 it generates down to just those forms and not reformat anything in the replacement syntax object
 that's outside of the focused syntax:

 @(examples
   #:eval (make-evaluator) #:once
   (define-definition-context-refactoring-rule point-define-to-match-define
     #:description "These definitions can be simplified with `match-define`."
     #:literals (define point-x point-y)
     (~seq body-before ...
           (~and x-def (define x:id (point-x pt:id)))
           (~and y-def (define y:id (point-y pt2:id)))
           body-after ...)
     #:when (free-identifier=? #'pt #'pt2)
     (body-before ...
      (~focus-replacement-on
       (~replacement (match-define (point x y) pt)
                     #:original-splice (x-def y-def)))
      body-after ...)))}


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


@subsection{Exercising Fine Control Over Comments}


Writing a rule with @racket[define-refactoring-rule] is usually enough for Resyntax to handle
commented code without issue, but in certain cases more precise control is desired. For instance,
consider the @racketidfont{nested-or-to-flat-or} rule from earlier:

@(racketblock
  (define-refactoring-rule nested-or-to-flat-or
    #:description "This nested `or` expression can be flattened."
    #:literals (or)
    (or a (or b c))
    (or a b c)))

As-is, this rule will @emph{fail} to refactor the following code:

@(racketblock
  (or (foo ...)
      (code:comment @#,elem{If that doesn't work, fall back to other approaches})
      (or (bar ...)
          (baz ...))))

Resyntax rejects the rule because applying it would produce this code, which loses the comment:

@(racketblock
  (or (foo ...)
      (bar ...)
      (baz ...)))

Resyntax is unable to preserve the comment automatically. Resyntax can preserve some comments without
programmer effort, but only in specific circumstances:

@itemlist[
 @item{Comments @emph{within} expressions that the rule left unchanged are preserved. If the comment
  were inside @racket[(foo ...)], @racket[(bar ...)], or @racket[(baz ...)], it would have been kept.}

 @item{Comments @emph{between} unchanged expressions are similarly preserved. If the comment were
  between @racket[(bar ...)] and @racket[(baz ...)], it would have been kept.}]

To fix this issue, rule authors can inject some extra markup into their suggested replacements using
@tech[#:doc '(lib "syntax/scribblings/syntax.scrbl")]{template metafunctions} provided by Resyntax. In
the case of @racketidfont{nested-or-to-flat-or}, we can use the @racket[~splicing-replacement]
metafunction to indicate that the nested @racket[or] expression should be considered @emph{replaced}
by its nested subterms:

@(racketblock
  (define-refactoring-rule nested-or-to-flat-or
    #:description "This nested `or` expression can be flattened."
    #:literals (or)
    (or a (~and nested-or (or b c)))
    #:with (nested-subterm ...) #'(~splicing-replacement (b c) #:original nested-or)
    (or a nested-subterm ...)))

This adds @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{syntax properties} to the nested
subterms that allow Resyntax to preserve the comment, producing this output:

@(racketblock
  (or (foo ...)
      (code:comment @#,elem{If that doesn't work, fall back to other approaches})
      (bar ...)
      (baz ...)))

When Resyntax sees that the @racket[(bar ...)] nested subterm comes immediately after the
@racket[(foo ...)] subterm, it notices that @racket[(bar ...)] has been annotated with replacement
properties. Then Resyntax observes that @racket[(bar ...)] is the first expression of a sequence of
expressions that replaces the @racket[or] expression which originally followed @racket[(foo ...)].
Based on this observation, Resyntax decides to preserve whatever text was originally between
@racket[(foo ...)] and the nested @racket[or] expression. This mechanism, exposed via
@racket[~replacement] and @racket[~splicing-replacement], offers a means for refactoring rules to
guide Resyntax's internal comment preservation system when the default behavior is not sufficient.

@defform[#:kind "template metafunction"
         (~replacement replacement-form original)
         #:grammar
         ([original
           (code:line #:original original-form)
           (code:line #:original-splice (original-form ...))])]{
 A @tech[#:doc '(lib "syntax/scribblings/syntax.scrbl")]{template metafunction} for use in
 @tech{refactoring rules}. The result of the metafunction is just the @racket[#'replacement-form]
 syntax object, except with some
 @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{syntax properties} added. Those
 properties inform Resyntax that this syntax object should be considered a replacement for
 @racket[original-form] (or in the splicing case, for the unparenthesized sequence
 @racket[original-form ...]). Resyntax uses this information to preserve comments and formatting near
 the original form(s).}

@defform[#:kind "template metafunction"
         (~splicing-replacement (replacement-form ...)
                                original)
         #:grammar
         ([original
           (code:line #:original original-form)
           (code:line #:original-splice (original-form ...))])]{
 A @tech[#:doc '(lib "syntax/scribblings/syntax.scrbl")]{template metafunction} for use in
 @tech{refactoring rules}. The result of the metafunction is the syntax object
 @racket[#'(replacement-form ...)], except with some
 @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{syntax properties} added. Those
 properties inform Resyntax that the replacement syntax objects --- as an unparenthesized sequence ---
 should be considered a replacement for @racket[original-form] (or @racket[original-form ...]).
 Resyntax uses this information to preserve comments and formatting near the original form(s).}


@subsection{Narrowing the Focus of Replacements}

@defform[#:kind "template metafunction"
         (~focus-replacement-on replacement-form)]{
 A @tech[#:doc '(lib "syntax/scribblings/syntax.scrbl")]{template metafunction} for use in
 @tech{refactoring rules}. The result of the metafunction is just the @racket[#'replacement-form]
 syntax object, except with some
 @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{syntax properties} added. Those
 properties inform Resyntax that the returned syntax object should be treated as the @emph{focus} of
 the entire refactoring rule's generated replacement. When a refactoring rule produces a replacement
 that has a focus, Resyntax checks that nothing outside the focus was modified. If this is the case,
 then Resyntax will @emph{shrink} the replacement it generates to only touch the focus. Crucially,
 this means Resyntax will @emph{only reformat the focused code}, not the entire generated replacement.
 This metafunction is frequently used with @racket[define-definition-context-refactoring-rule],
 because such rules often touch only a small series of forms in a much larger definition context.}


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
  disagree with are not a good fit for Resyntax. Technology is social before it is technical:
  discussing your rule with the Racket community prior to developing it is encouraged, especially if
  it's likely to affect a lot of code. If necessary, consider narrowing the focus of your rule to just
  the cases that everybody agrees are clear improvements.}

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


@section{Testing Refactoring Rules}
@defmodule[resyntax/test]


Testing @tech{refactoring rules} is crucial to ensure they work correctly and safely. Resyntax provides
a domain-specific testing language called @racketmodname[resyntax/test] that makes it easy to write
comprehensive tests for refactoring rules. This language is designed specifically for testing code
transformations and provides a clean, readable syntax for specifying before-and-after code examples.


@subsection{Basic Test Structure}


The basic structure of a test file is:

@itemlist[#:style 'ordered
 @item{Start with @racket[#,hash-lang[] resyntax/test]}
 @item{Use @racket[require:] to import the refactoring suite to test}
 @item{Use @racket[header:] to specify common code for all tests}  
 @item{Use @racket[test:] to define individual test cases}]

Here's an example:

@codeblock[#:keep-lang-line? #false]|{
require: module-name suite-name

header:
- #lang racket/base

test: "description of what is being tested"
------------------------------
original code here  
------------------------------
------------------------------
expected refactored code here
------------------------------
}|

The main components are:

@itemlist[
 @item{@racket[require:] --- Imports a refactoring suite to test. The @racket[module-name] specifies
  where the suite is defined, and @racket[suite-name] is the identifier of the suite.}

 @item{@racket[header:] --- Specifies a code block that will be prepended to all test cases. This
  typically contains @racket[@hash-lang[]] declarations and any necessary @racket[require] statements.}

 @item{@racket[test:] --- Defines a test case with a description. Test cases can specify either code
  that should be refactored or code that should not be refactored.}]


@subsection{Test Case Types}


There are two types of test cases you can write:

@subsubsection{Positive Test Cases (Code That Should Be Refactored)}

Positive test cases specify code that should be transformed by the refactoring rules. They consist of
two code blocks separated by a line of dashes:

@verbatim|{
test: "lambda variable definition to function definition"
------------------------------
(define f
  (λ (a b c)
    1))
------------------------------
------------------------------
(define (f a b c)
  1)
------------------------------
}|

@subsubsection{Negative Test Cases (Code That Should Not Be Refactored)}

Negative test cases specify code that should be left unchanged by the refactoring rules. They consist
of a single code block:

@verbatim|{
test: "class lambda variable definition not refactorable"
------------------------------
(require racket/class)
(class object%
  (define f
    (λ (a b c)
      1)))
------------------------------
}|


@subsection{Test Options}


Test cases can include options that modify how the test is executed:

@subsubsection{Line Range Restrictions}

The @racket[@lines] option restricts refactoring to specific line ranges, which is useful for testing
that rules only affect the intended parts of multi-definition code:

@verbatim|{
test: "refactoring respects requested line range"
@lines 2..3
------------------------------
(define (foo)
  (define-values (a b c)
    (values 1 2 3))
  (+ a b c))
(define (bar)
  (define-values (x y z)
    (values 4 5 6))
  (+ x y z))
------------------------------
------------------------------
(define (foo)
  (define a 1)
  (define b 2)
  (define c 3)
  (+ a b c))
(define (bar)
  (define-values (x y z)
    (values 4 5 6))
  (+ x y z))
------------------------------
}|

Line ranges use the syntax @racket[start..end] where @racket[start] and @racket[end] are line numbers
(starting from 1). Multiple ranges can be specified by separating them with commas.


@subsection{Writing Effective Tests}


When writing tests for refactoring rules, consider these guidelines:

@itemlist[
 @item{@emph{Test both positive and negative cases} --- Include tests for code that should be
  refactored and code that should be left alone. This ensures your rules are neither too aggressive
  nor too conservative.}

 @item{@emph{Test edge cases} --- Include tests for unusual or borderline cases, such as nested
  expressions, complex argument lists, or interactions with other language features.}

 @item{@emph{Use descriptive test names} --- Test descriptions should clearly explain what is being
  tested and why. This makes it easier to understand test failures and maintain the test suite.}

 @item{@emph{Test formatting preservation} --- Include tests that verify your rules preserve code
  formatting and comments appropriately. Resyntax aims to make minimal changes to code structure.}

 @item{@emph{Test line range restrictions} --- If your rule might be applied to part of a file,
  include tests with @racket[@lines] options to ensure it behaves correctly in that context.}]


@subsection{Running Tests}


Tests can be run using Racket's standard testing tools:

@verbatim|{
raco test my-rule-test.rkt
}|

This will execute all test cases in the file and report any failures. Test failures typically indicate
that a refactoring rule is not behaving as expected and needs to be fixed.
