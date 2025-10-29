# Overview

This repository is a Racket package called Resyntax, which is a refactoring and
static analysis tool for Racket code. It analyzes code using "refactoring
rules" written in a domain-specific sublanguage of Racket and implemented using
Racket's macro system. Resyntax then uses these rules to suggest ways people
can improve their Racket code. See the [Resyntax documentation][1] and
[Racket website][2] for more information.

## Adding New Refactoring Rules

When trying to add a new refactoring rule to Resyntax's default
recommendations, pay special attention to the sections in the Resyntax
documentation on [what makes a good refactoring rule][3] and on
[how to test refactoring rules][4]. Additionally, consider running Resyntax
itself on the files you touch before opening a pull request: this can help you
improve your code and ensure it follows Racket's best practices. Check the
[documentation on the Resyntax command line tool][5] for more information on
how to run it. Beware that Resyntax is *not* a `raco` command. Run it
using `resyntax fix` or `resyntax analyze`, not `raco resyntax fix` or
`raco resyntax analyze`.

If you want to experiment with new refactoring rules you've created, consider
doing so by cloning the [DrRacket][6], [Herbie][7], or [Typed Racket][8]
repositories and running Resyntax on them. These repositories contain a lot
of Racket code and are good candidates for testing new refactoring rules.

## Pull Request Style Conventions

When creating a pull request, avoid being overly verbose in the pull
request description. Keep descriptions to a single paragraph. If you need to
include example code, limit it to one or two small blocks. Do not write
lengthy, detailed explanations or documentation in the PR description. Avoid
mentioning things that are obvious from the code changes themselves, such as
lists of files changed. Reserve the PR description for only the most essential
information, and err on the side of omission. There is nothing wrong with a
pull request description that is just a single sentence and a mention of what
issue number is being addressed.

## Code Coverage

When writing tests, you can use the [`raco cover`][9] command to check the
code coverage of your test cases. The command `raco cover path/to/file.rkt`
will generate an HTML report showing what code is covered by the indicated test.
To check for all tests in the repository, you can run `raco cover --pkgs resyntax`.
Pull requests should aim for high code coverage, and an integration with Coveralls
is set up to help track coverage over time. Beware that `raco cover` has some
sharp edges and inconsistencies compared to `raco test`; see its documentation for
more details.

[1]: https://docs.racket-lang.org/resyntax/
[2]: https://racket-lang.org/
[3]: https://docs.racket-lang.org/resyntax/Refactoring_Rules_and_Suites.html#%28part._.What_.Makes_a_.Good_.Refactoring_.Rule_%29
[4]: https://docs.racket-lang.org/resyntax/Testing_Refactoring_Rules.html
[5]: https://docs.racket-lang.org/resyntax/cli.html
[6]: https://github.com/racket/drracket
[7]: https://github.com/herbie-fp/herbie
[8]: https://github.com/racket/typed-racket
[9]: https://docs.racket-lang.org/cover/
