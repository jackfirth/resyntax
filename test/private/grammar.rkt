#lang brag


program: statement*
statement: IDENTIFIER /COLON (option | expression | code-block-sequence)+
option: /AT-SIGN IDENTIFIER expression


@expression: code-line
           | standalone-code-block
           | prefixed-standalone-code-block
           | range-set
           | IDENTIFIER
           | LITERAL-STRING
           | LITERAL-INTEGER


code-line: /SINGLE-DASH CODE-LINE
standalone-code-block: /DASH-LINE CODE-LINE* /DASH-LINE
prefixed-standalone-code-block: /PIPE-DASH-LINE (/PIPE-SPACE CODE-LINE)* /PIPE-DASH-LINE


@code-block-sequence: first-code-block middle-code-block* last-code-block+
                    | prefixed-first-code-block prefixed-middle-code-block* prefixed-last-code-block+


first-code-block: /DASH-LINE CODE-LINE* /EQUALS-LINE
middle-code-block: CODE-LINE* /EQUALS-LINE
last-code-block: CODE-LINE* /DASH-LINE


prefixed-first-code-block: /PIPE-DASH-LINE (/PIPE-SPACE CODE-LINE)* /PIPE-EQUALS-LINE
prefixed-middle-code-block: (/PIPE-SPACE CODE-LINE)* /PIPE-EQUALS-LINE
prefixed-last-code-block: (/PIPE-SPACE CODE-LINE)* /PIPE-DASH-LINE


range-set: line-range (/COMMA line-range)*
line-range: LITERAL-INTEGER /DOUBLE-DOT LITERAL-INTEGER
