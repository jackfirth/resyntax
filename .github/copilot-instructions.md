This repository is a Racket package called Resyntax, which is a refactoring and
static analysis tool for Racket code. It analyzes code using "refactoring
rules" written in a domain-specific sublanguage of Racket and implemented using
Racket's macro system. Resyntax then uses these rules to suggest ways people
can improve their Racket code. See the [Resyntax documentation][1] and
[Racket website][2] for more information.

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

When creating a pull request, avoid being overly verbose in the pull
request description. A paragraph or two at most is usually sufficient.

If you want to experiment with new refactoring rules you've created, consider
doing so by cloning the [DrRacket][6], [Herbie][7], or [Typed Racket][8]
repositories and running Resyntax on them. These repositories contain a lot
of Racket code and are good candidates for testing new refactoring rules.

To format your code, use the `raco fmt` command. This will ensure that your
code adheres to Racket's formatting standards.

[1]: https://docs.racket-lang.org/resyntax/
[2]: https://racket-lang.org/
[3]: https://docs.racket-lang.org/resyntax/Refactoring_Rules_and_Suites.html#%28part._.What_.Makes_a_.Good_.Refactoring_.Rule_%29
[4]: https://docs.racket-lang.org/resyntax/Testing_Refactoring_Rules.html
[5]: https://docs.racket-lang.org/resyntax/cli.html
[6]: https://github.com/racket/drracket
[7]: https://github.com/herbie-fp/herbie
[8]: https://github.com/racket/typed-racket
