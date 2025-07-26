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
improve your code and ensure it follows Racket's best practices.

[1]: https://docs.racket-lang.org/resyntax/
[2]: https://racket-lang.org/
[3]: https://docs.racket-lang.org/resyntax/Refactoring_Rules_and_Suites.html#%28part._.What_.Makes_a_.Good_.Refactoring_.Rule_%29
[4]: https://docs.racket-lang.org/resyntax/Testing_Refactoring_Rules.html
