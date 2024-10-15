#lang brag

begin: statement*
statement: COLON-IDENTIFIER (expression | option)+
@expression: IDENTIFIER | LITERAL-STRING | LITERAL-INTEGER | closed-range | range-set | code-block
option: AT-SIGN-IDENTIFIER expression
code-block: CODE-BLOCK
closed-range: LITERAL-INTEGER /DOUBLE-DOT LITERAL-INTEGER
range-set: closed-range (/COMMA closed-range)+
