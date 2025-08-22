#lang brag


program: statement*
statement: IDENTIFIER /COLON (option | expression | code-block-sequence)+
option: /AT-SIGN IDENTIFIER expression


@expression: code-line
           | standalone-code-block
           | range-set
           | IDENTIFIER
           | LITERAL-STRING
           | LITERAL-INTEGER


code-line: /SINGLE-DASH CODE-LINE
standalone-code-block: /DASH-LINE CODE-LINE* /DASH-LINE


@code-block-sequence: starting-code-block middle-code-block* ending-code-block+
starting-code-block: /DASH-LINE CODE-LINE* /EQUALS-LINE
middle-code-block: CODE-LINE* /EQUALS-LINE
ending-code-block: CODE-LINE* /DASH-LINE


range-set: line-range (/COMMA line-range)*
line-range: LITERAL-INTEGER /DOUBLE-DOT LITERAL-INTEGER
