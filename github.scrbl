#lang scribble/manual


@title[#:tag "github"]{Resyntax on GitHub}


Resyntax integrates with GitHub in two ways. The @emph{analyzer} reviews pull requests, leaving
Resyntax's suggestions as review comments on the lines they apply to. The @emph{autofixer}
periodically runs over an entire repository and opens a pull request applying the fixes it finds,
cleaning the repository up over time. Both are GitHub Actions workflows built on the
@exec{resyntax analyze} and @exec{resyntax fix} commands described in @secref["cli"].

Reviews the analyzer has left on pull requests in the wild can be found with
@hyperlink["https://github.com/search?q=%22Resyntax%20analyzed%22%20%22added%20suggestions%22%20in%3Acomments%20is%3Apr%20sort%3Aupdated%20&type=pullrequests"]{
 this GitHub search}, and pull requests opened by the autofixer can be found with
@hyperlink["https://github.com/search?q=author%3Aapp%2Fresyntax-ci&type=pullrequests"]{this one}.

The analyzer is split into two workflows: one that analyzes the code and uploads the analysis as an
artifact, and one that downloads the artifact and submits it as a pull request review. The analyzing
workflow checks out and compiles the pull request's branch, so it runs with read-only repository
permissions; the submitting workflow needs write permissions but never executes any of the
repository's code. This division lets pull requests from forks be analyzed safely. Resyntax's own
repository uses this setup, in
@hyperlink["https://github.com/jackfirth/resyntax/blob/master/.github/workflows/resyntax-analyze.yml"]{
 @tt{resyntax-analyze.yml}} and
@hyperlink["https://github.com/jackfirth/resyntax/blob/master/.github/workflows/resyntax-submit-review.yml"]{
 @tt{resyntax-submit-review.yml}}.
