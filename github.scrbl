#lang scribble/manual


@title[#:tag "github"]{Resyntax on GitHub}


In addition to the @secref["cli"], Resyntax integrates with GitHub in two ways: an @deftech{analyzer}
that reviews pull requests, and an @deftech{autofixer} that periodically opens pull requests cleaning
up a repository.

The @tech{analyzer} runs on each pull request, leaving review comments with the suggestions Resyntax
would make for the changed code. You can find reviews the analyzer has left on pull requests
@hyperlink["https://github.com/search?q=%22Resyntax%20analyzed%22%20%22added%20suggestions%22%20in%3Acomments%20is%3Apr%20sort%3Aupdated%20&type=pullrequests"]{
 in the wild} using GitHub search.

The @tech{autofixer} runs on a schedule, applying Resyntax's suggestions to a repository and opening a
pull request with the results. Pull requests created by the autofixer can be found
@hyperlink["https://github.com/search?q=author%3Aapp%2Fresyntax-ci&type=pullrequests"]{using this
 search}.
