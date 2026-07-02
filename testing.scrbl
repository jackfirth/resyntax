#lang scribble/manual


@(require (for-label (except-in racket/base require)
                     (except-in resyntax/test #%app #%module-begin)))


@title[#:tag "testing"]{Testing Refactoring Rules}
@defmodulelang[resyntax/test]


The @racketmodname[resyntax/test] language provides a convenient domain-specific language for testing
@tech{refactoring rules}. This language makes it easy to write comprehensive tests that verify
refactoring rules work correctly across a variety of inputs, and that they properly handle edge cases
without making unwanted transformations.

@section{Basic Test Syntax}

Tests are written using @racket[@#,hash-lang[] @#,racketmodname[resyntax/test]] and consist of a
series of @deftech{test statements}. Each statement begins with a keyword followed by a colon, and
the statement's body follows. Strings of code are written using @tech{code blocks}. Here's a simple
example:

@verbatim{
 #lang resyntax/test

 require: my-rules my-suite

 header:
 - #lang racket

 test: "my rule transforms code as expected"
 - (old-pattern 1 2 3)
 - (new-pattern 1 2 3)
}


@section{Code Blocks}

A @deftech{code block} is a delimited section of Racket code used within @tech{test statements}.
There are two types of code blocks:

@itemlist[

 @item{@deftech{Single-line code blocks} are preceded by a single dash and a space (@litchar{- }).}

 @item{@deftech{Multi-line code blocks} are delimited by lines of at least three consecutive dashes
  (@litchar{---}). As a convenience, two adjacent multi-line code blocks can be separated by a single
  line of equals signs (@litchar{===}) instead of two lines of dashes.}]

Code blocks are essentially string literals, and can contain code written in any language. For this
reason, it's common for Resyntax tests to include a @racket[header] test statement which specifies
what @hash-lang[] each code block in that file is written in.


@section{Test Statements}

The @racketmodname[resyntax/test] language supports four types of @tech{test statements}:

@itemlist[
 @item{@racket[require] statements for loading @tech{refactoring suites}}
 @item{@racket[header] statements for defining common code used in all tests}
 @item{@racket[test] and @racket[no-change-test] statements for defining individual test cases}]


@defform[#:kind "test statement" (require module-path suite-name)]{
 Loads the @tech{refactoring suite} named @racket[suite-name] from the module at
 @racket[module-path]. The refactoring suite will be used in all tests defined in the surrounding
 file. Multiple @racket[require] statements can be used to test rules from multiple different suites.

 @verbatim{
  #lang resyntax/test

  require: resyntax/default-recommendations list-shortcuts
  require: my-custom-rules my-suite
}}


@defform[#:kind "test statement" (header code-block)]{
 Defines a @tech{code block} that will be prepended to every test case code block in the file. This is
 useful for common setup code that all tests need, such as a @hash-lang[] line or library imports.

 @verbatim{
  #lang resyntax/test

  header:
  --------------------
  #lang racket/base
  (require racket/list)
  --------------------
}}


@defform[#:kind "test statement"
         (test description-string
               input-code-block ...+
               expected-code-block)]{
 Defines a test case named with the given @racket[description-string]. The test case checks that
 Resyntax refactors each @racket[input-code-block] block into the final @racket[expected-code-block]:

 @verbatim{
  #lang resyntax/test

  test: "should rewrite old function to new function"
  --------------------
  #lang racket
  (old-function 1 2 3)
  ====================
  #lang racket
  (new-function 1 2 3)
  --------------------

  test: "should remove old-condition from and expressions"
  - (and old-condition x)
  - (and x old-condition)
  - (and old-condition x old-condition)
  - x
 }}

@defform[#:kind "test statement" (no-change-test description-string input-code-block)]{
 Defines a test case named with the given @racket[description-string]. The test checks that Resyntax
 does @emph{not} make any changes to the @racket[input-code-block]:

 @verbatim{
  #lang resyntax/test

  no-change-test: "should not rewrite old function to new function in higher-order uses"
  --------------------
  #lang racket
  (map old-function (list 1 2 3))
  --------------------
}}


@section{Running Resyntax Tests}

Tests written in @racketmodname[resyntax/test] are integrated with RackUnit and can be run using
the standard @exec{raco test} command:

@verbatim{
 % raco test my-rule-test.rkt
 raco test: (submod "my-rule-test.rkt" test)
 5 tests passed
}

Each @racket[test] statement becomes a RackUnit test case, and the entire test file becomes a module
with a single submodule named @racket[test]. Clicking the Run button in DrRacket will execute each
test in the file. Just like in RackUnit, failing tests are highlighted by DrRacket and print failure
messages.

When executing tests, the @racketmodname[resyntax/test] language enables Resyntax's debug logging and
captures all of its logs. Failing test cases include the captured Resyntax logs in their printed
output. This output can be somewhat verbose, but it makes it much easier to tell why Resyntax did or
didn't refactor code and how Resyntax came to produce a malformed suggestion.
