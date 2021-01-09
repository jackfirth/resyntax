# racket-package-template [![CI Status][ci-status-badge]][ci-status] [![Documentation][docs-badge]][docs]

My personal template repository for Racket packages.

## How To Use

1. Search and replace `racket-package-template` with the programmatic name of
your package, i.e. the name you'd use when publishing it on the Racket package
catalog.

2. Search and replace `Racket Package Template` with the title of your package
that will be used in docs.

3. Search and replace `jackfirth` with your GitHub username.

4. Delete the How To Use section and the Assumptions section of this README file
and rewrite the summary at the top.

## Assumptions

This template makes several assumptions about what sort of Racket project you're
making. In particular, the template assumes that:

- You're making exactly one Racket package.

- You're *not* making a multi-collection package.

- You're using the same name for your package, the collection your package
provides, and the repository containing your package.

- You want to build and test your package using GitHub Actions with whatever the
latest Racket version is.

- You're going to put your package on the Racket package catalog and you're fine
with this README's documentation badge linking to the catalog's hosted docs for
your package.

- You want your package's documentation to be split into multiple pages instead
of putting everything on one page.

[ci-status]: https://github.com/jackfirth/racket-package-template/actions
[ci-status-badge]: https://github.com/jackfirth/racket-package-template/workflows/CI/badge.svg
[docs]: https://docs.racket-lang.org/racket-package-template/index.html
[docs-badge]: https://img.shields.io/badge/docs-published-blue.svg
