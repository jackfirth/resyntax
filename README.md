# resyntax [![CI Status][ci-status-badge]][ci-status] [![Documentation][docs-badge]][docs]

A refactoring tool for Racket built on top of `syntax-parse`.

[ci-status]: https://github.com/jackfirth/resyntax/actions
[ci-status-badge]: https://github.com/jackfirth/resyntax/workflows/CI/badge.svg
[docs]: https://docs.racket-lang.org/resyntax/index.html
[docs-badge]: https://img.shields.io/badge/docs-published-blue.svg

## Quickstart

Use the Racket package manager to install in the installation scope.  
```
raco pkg install --installation resyntax
```
The `--installation` flag (shorthand for `--scope installation`) installs packages for all users of a Racket installation and ensures `resyntax` is in your `$PATH`. 

e.g. 
```
% resyntax analyze --file coroutines-example.rkt
resyntax: --- analyzing code ---
resyntax: --- displaying results ---
%
```

See the documentation for more details on how to use `resyntax`.

## Examples

You can find suggestions Resyntax has left on GitHub pull requests "in the wild" using [this search](https://github.com/search?q=%22Resyntax%20analyzed%22%20%22added%20suggestions%22%20in%3Acomments%20is%3Apr%20sort%3Aupdated%20&type=pullrequests).
