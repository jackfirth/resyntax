#lang scribble/manual


@(require scribble/example
          (for-label racket/base
                     racket/contract/base
                     racket/path
                     racket/port
                     rebellion/base/comparator
                     rebellion/base/immutable-string
                     rebellion/base/range
                     rebellion/collection/range-set
                     resyntax/grimoire/source
                     syntax/modread)
          (submod resyntax/private/scribble-evaluator-factory doc))


@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'resyntax/grimoire/source
                   'rebellion/base/comparator
                   'rebellion/base/range
                   'rebellion/collection/range-set)
    #:private (list 'racket/base)))


@title[#:tag "source"]{Source Code}
@defmodule[resyntax/grimoire/source]

In Resyntax, @deftech{source code} refers to @racket[source?] values, which come in three types:

@itemlist[
 @item{@emph{Source files}, constructed with @racket[file-source], which don't contain the code
  directly but refer to it by a local filesystem path.}

 @item{@emph{Source strings}, constructed with @racket[string-source], which contain the source
  code directly as a string and don't exist anywhere on the local filesystem. (These are useful for
  testing Resyntax, and other scenarios where Resyntax needs to operate on code that doesn't exist on
  disk.)}

 @item{@emph{Modified sources}, constructed by passing another (unmodified) source to
  @racket[modified-source] along with a string representing what to replace the source's contents
  with. A modified source contains both its new updated contents and a reference to the original
  source.}]

Resyntax's basic architecture is to recursively take sources of any kind as input, produce
@racket[modified-source?] values as output, then re-analyze the modified sources again until no
further modifications are desired. Then, Resyntax decides whether to commit those final modifications
to the filesystem (as in @tt{resyntax fix}) or merely display them to users (as in
@tt{resyntax analyze}). This recursive loop approach allows Resyntax to "look ahead" and produce a
stack of dependent changes to commit in series without actually mutating the files on disk.


@section{Basic Source Operations}


@defproc[(source? [v any/c]) boolean?]{
 A predicate that recognizes @tech{source code} values of any kind --- file, string, or modified.

 @(examples
   #:eval (make-evaluator) #:once
   (source? (string-source "#lang racket/base\n(+ 1 2)"))
   (source? (file-source "/tmp/my-project/main.rkt"))
   (source? "just a string, not a source"))}


@defproc[(unmodified-source? [v any/c]) boolean?]{
 A predicate that recognizes @tech{source code} values that are @emph{not} modified sources, i.e.
 either file sources or string sources.

 @(examples
   #:eval (make-evaluator) #:once
   (unmodified-source? (string-source "#lang racket/base\n(+ 1 2)"))
   (unmodified-source? (modified-source (string-source "#lang racket/base\n1") "#lang racket/base\n2")))}


@defproc[(file-source? [v any/c]) boolean?]{
 A predicate that recognizes (unmodified) source files.

 @(examples
   #:eval (make-evaluator) #:once
   (file-source? (file-source "/tmp/my-project/main.rkt"))
   (file-source? (string-source "#lang racket/base\n(+ 1 2)")))}


@defproc[(file-source [path path-string?]) file-source?]{
 Constructs a source file that refers to the code stored on disk at @racket[path]. The path is
 normalized with @racket[simple-form-path] upon construction.

 @(examples
   #:eval (make-evaluator) #:once
   (file-source "/tmp/my-project/main.rkt"))}


@defproc[(string-source? [v any/c]) boolean?]{
 A predicate that recognizes source strings.

 @(examples
   #:eval (make-evaluator) #:once
   (string-source? (string-source "#lang racket/base\n(+ 1 2)"))
   (string-source? (file-source "/tmp/my-project/main.rkt")))}


@defproc[(string-source [contents string?]) string-source?]{
 Constructs a source string containing @racket[contents] directly. The newlines of
 @racket[contents] are normalized at construction time, in the same manner described in
 @racket[with-input-from-source]. Normalizing eagerly ensures that two string sources denoting the
 same text are @racket[equal?] even if they were constructed with different newline conventions.

 @(examples
   #:eval (make-evaluator) #:once
   (string-source "#lang racket/base\n(+ 1 2)")
   (eval:check (equal? (string-source "a\r\nb") (string-source "a\nb")) #true))}


@defproc[(modified-source? [v any/c]) boolean?]{
 A predicate that recognizes modified sources. A modified source is always a wrapper around an
 unmodified source plus a string containing the full replacement text that the modified source should
 contain instead of what the original source contains.

 @(examples
   #:eval (make-evaluator) #:once
   (modified-source? (modified-source (string-source "#lang racket/base\n1") "#lang racket/base\n2"))
   (modified-source? (string-source "#lang racket/base\n(+ 1 2)")))}


@defproc[(modified-source [original unmodified-source?] [new-contents string?]) modified-source?]{
 Constructs a modified source that replaces the contents of @racket[original] with
 @racket[new-contents]. This represents a whole-file replacement --- the @emph{complete} contents of
 @racket[original] are @emph{entirely} swapped out with @racket[new-contents]. Modified sources cannot
 represent partial edits on their own. Like @racket[string-source], the newlines of
 @racket[new-contents] are normalized at construction time.

 The original source is left untouched; only the modified source reports the new contents.

 @(examples
   #:eval (make-evaluator) #:once
   (define original (string-source "#lang racket/base\n(define x 1)"))
   (define updated (modified-source original "#lang racket/base\n(define x 2)"))
   (source->string updated)
   (source->string original))}


@defproc[(source-name [code source?]) (or/c path? symbol?)]{
 Returns a name identifying @racket[code], suitable for use as a syntax object's source location
 name. For file-based sources this is the source file's path, and for string-based sources this is the
 symbol @racket['string]. Modified sources always have the same name as their original unmodified
 sources.

 @(examples
   #:eval (make-evaluator) #:once
   (source-name (file-source "/tmp/my-project/main.rkt"))
   (source-name (string-source "#lang racket/base\n(+ 1 2)"))
   (source-name
    (modified-source (file-source "/tmp/my-project/main.rkt") "#lang racket/base\n(+ 1 3)")))}


@defproc[(source-path [code source?]) (or/c path? #false)]{
 Returns the filesystem path of @racket[code], or @racket[#false] if @racket[code] is not
 file-based. Modified sources are file-based if their original source is file-based.

 @(examples
   #:eval (make-evaluator) #:once
   (source-path (file-source "/tmp/my-project/main.rkt"))
   (source-path (string-source "#lang racket/base\n(+ 1 2)"))
   (source-path
    (modified-source (file-source "/tmp/my-project/main.rkt") "#lang racket/base\n(+ 1 3)")))}


@defproc[(source-directory [code source?]) (or/c path? #false)]{
 Returns the directory containing @racket[code], or @racket[#false] if @racket[code] is not
 file-based. Modified sources are file-based if their original source is file-based.

 @(examples
   #:eval (make-evaluator) #:once
   (source-directory (file-source "/tmp/my-project/main.rkt"))
   (source-directory (string-source "#lang racket/base\n(+ 1 2)")))}


@defproc[(source-original [code source?]) unmodified-source?]{
 Returns the original, unmodified source underlying @racket[code]. If @racket[code] is already
 unmodified, it is returned as-is.

 @(examples
   #:eval (make-evaluator) #:once
   (define original (string-source "#lang racket/base\n(define x 1)"))
   (eval:check
    (equal? (source-original (modified-source original "#lang racket/base\n(define x 2)")) original)
    #true)
   (eval:check (equal? (source-original original) original) #true))}


@defproc[(source->string [code source?]) immutable-string?]{
 Returns the full text of @racket[code], reading it from the filesystem if necessary. For
 @racket[modified-source?] values, this returns the new, updated text rather than the original
 unmodified text. The returned text has its newlines normalized, as described in
 @racket[with-input-from-source].

 @(examples
   #:eval (make-evaluator) #:once
   (source->string (string-source "#lang racket/base\n(+ 1 2)"))
   (source->string
    (modified-source (string-source "#lang racket/base\n1") "#lang racket/base\n2")))}


@defproc[(with-input-from-source [code source?] [proc (-> any)]) any]{
 Calls @racket[proc] with @racket[current-input-port] set to a freshly opened input port reading
 the contents of @racket[code]. For unmodified file sources, this opens a file port. For modified
 sources and string sources, this opens a string port without interacting with the filesystem.

 The opened port decodes the source as UTF-8 and @bold{normalizes newlines}, as in
 @racket[reencode-input-port]: Windows-style @racket["\r\n"] sequences (and other newline
 conventions) are converted into single @racket[#\newline] characters. Every operation that reads
 a source goes through this normalization, including @racket[source->string] and
 @racket[source-read-syntax]. This normalization mirrors the normalization Racket performs when
 reading from ports with line counting enabled, as described in
 @secref["linecol" #:doc '(lib "scribblings/reference/reference.scrbl")].

 Newline normalization avoids various problems in Resyntax. Primarily, it ensures that the character
 positions of source text strings match with the position numbers in syntax object source locations.
 Without normalization, replacements could be misapplied in files if they contain Windows-style
 newlines --- an especially difficult problem to diagnose given that newline characters are invisible
 and their conventions platform-specific. See @hyperlink["https://github.com/jackfirth/resyntax/issues/272"]{this motivating bug report}
 for an example.

 Note that string sources and modified sources apply this normalization eagerly, when the source value
 is constructed, so the port-level conversion only has a visible effect for unmodified file sources.

 @(examples
   #:eval (make-evaluator) #:once
   (with-input-from-source (string-source "first line\nsecond line") read-line))}


@section{Parsing, Expanding, and Compiling Sources}


The following operations allow treating @tech{source code} values as inputs to Racket's compiler.
For file sources, this is roughly the same as reading the file into a syntax object using
@racket[with-module-reading-parameterization] and expanding that syntax object using @racket[expand].
However, string sources and modified sources behave slightly differently, especially with regard to
source location information on derived syntax objects:

@itemlist[
 @item{A string source behaves as if it were an @emph{anonymous file} with no well-defined location on
  the filesystem. Relative file path imports will not work correctly. Source location information will
  still be present with line and column numbers, but will claim to be located in a source named
  @racket['string] instead of a file.}

 @item{A modified source behaves the same as its wrapped unmodified source, except as if its contents
  were completely replaced. Source location information will be present, @bold{but will not correspond
   to positions within the original source} as they will instead refer to positions in the modified
  contents. If the original source was a file source, accidentally using source locations from the
  modified source to make edits to the original file will produce malformed changes.}]

Note that while a file source is being read and expanded, the current directory is parameterized to
the file source's parent directory. This ensures that relative file path imports in source files can
still be resolved regardless of what the current directory is before the source is read or expanded.
This applies to both modified and unmodified file sources.


@defproc[(source-read-language [code source?]) (or/c module-path? #false)]{
 Detects the @hash-lang[] language of @racket[code] and returns the module path of that language.
 Returns @racket[#false] if @racket[code] does not begin with a @hash-lang[] line.

 @(examples
   #:eval (make-evaluator) #:once
   (source-read-language (string-source "#lang racket/base\n(+ 1 2)"))
   (source-read-language (string-source "#lang scribble/manual"))
   (source-read-language (string-source "(+ 1 2)")))}


@defproc[(source-read-syntax [code source?]) syntax?]{
 Reads @racket[code] as a syntax object, using the module reading parameterization to allow the
 source's @hash-lang[] to control the reader. Every syntax object within the result is labeled with
 its @tech{original syntax path} using the @racket['original-syntax-path] syntax property, as
 described in @secref["original-syntax-paths"].

 @(examples
   #:eval (make-evaluator) #:once
   (syntax->datum (source-read-syntax (string-source "#lang racket/base\n(+ 1 2)"))))}


@defproc[(source-expand [code source?]) syntax?]{
 Reads @racket[code] and fully expands it, as in @racket[expand]. Because the program is read with
 @racket[source-read-syntax], its subforms are labeled with @racket['original-syntax-path]
 properties before expansion occurs. Unlike @racket[source-read-syntax], the result is fully expanded,
 so its subforms are core syntactic forms rather than the ones written in the source.

 @(examples
   #:eval (make-evaluator) #:once
   (car (syntax->datum (source-expand (string-source "#lang racket/base\n(+ 1 2)")))))}


@defproc[(source-can-expand? [code source?]) boolean?]{
 Attempts to fully expand @racket[code], then returns @racket[#true] if expansion finished
 without raising an error and returns @racket[#false] otherwise.

 @(examples
   #:eval (make-evaluator) #:once
   (source-can-expand? (string-source "#lang racket/base\n(define x 42)"))
   (source-can-expand? (string-source "#lang racket/base\n(if)")))}


@defproc[(source-text-of [code source?] [stx syntax?]) immutable-string?]{
 Returns the source text within @racket[code] that produced @racket[stx], based on the source location
 information attached to @racket[stx]. The returned text is verbatim, preserving the exact whitespace
 and formatting used in @racket[code]. Raises a contract violation if @racket[stx] does not have
 source location information.

 @(examples
   #:eval (make-evaluator) #:once
   (define code (string-source "#lang racket/base\n(+  1   2)"))
   (define program (source-read-syntax code))
   (define plus-form (cadr (syntax->list (list-ref (syntax->list program) 3))))
   (source-text-of code plus-form)
   (eval:error (source-text-of code (datum->syntax #false 'no-location))))}


@defproc[(source-comment-locations [code source?]) immutable-range-set?]{
 Returns a range set containing the positions of all comments in @racket[code]. This is implemented
 by looking up the lexer of the @hash-lang[] that @racket[code] is written in, using the
 @racketmodname[syntax-color/module-lexer] API. @bold{Warning: the positions are zero-based}, unlike
 the one-based positions returned from @racket[syntax-position]. Additionally, positions are in terms
 of @emph{characters} and not @emph{bytes}.

 @(examples
   #:eval (make-evaluator) #:once
   (source-comment-locations (string-source "#lang racket/base\n; a comment\n(+ 1 2)")))}
