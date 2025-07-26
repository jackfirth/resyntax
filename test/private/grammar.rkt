#lang brag

begin: statement*
statement: COLON-IDENTIFIER (option | code-block | expression)+
@expression: range-set | IDENTIFIER | LITERAL-STRING | LITERAL-INTEGER
option: AT-SIGN-IDENTIFIER expression
code-block: CODE-BLOCK (EQUALS-SEPARATOR CODE-BLOCK)*
range-set: line-range (/COMMA line-range)*
line-range: LITERAL-INTEGER /DOUBLE-DOT LITERAL-INTEGER
