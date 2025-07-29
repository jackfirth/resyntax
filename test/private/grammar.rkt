#lang brag






begin
:

statement
*


statement
:

COLON-IDENTIFIER

(
option

|

expression

|

code-block-sequence
)
+


@
expression
:

range-set

|

IDENTIFIER

|

LITERAL-STRING

|

LITERAL-INTEGER


option
:

AT-SIGN-IDENTIFIER

expression






@
code-block-sequence
:

standalone-code-block






















|

starting-code-block

middle-code-block
*

ending-code-block
+


standalone-code-block
:

(
/
SINGLE-DASH

CODE-LINE
)

|

(
/
DASH-LINE

CODE-LINE
*

/
DASH-LINE
)


starting-code-block
:

/
DASH-LINE

CODE-LINE
*

/
EQUALS-LINE


middle-code-block
:

CODE-LINE
*

/
EQUALS-LINE


ending-code-block
:

CODE-LINE
*

/
DASH-LINE






range-set
:

line-range

(
/
COMMA

line-range
)
*


line-range
:

LITERAL-INTEGER

/
DOUBLE-DOT

LITERAL-INTEGER
