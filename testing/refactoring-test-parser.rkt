#lang brag

refactoring-test: refactoring-test-import* refactoring-test-case*
refactoring-test-import: /REQUIRE-KEYWORD IDENTIFIER IDENTIFIER
refactoring-test-case: /TEST-KEYWORD STRING-LITERAL /SEPARATOR-LINE (LANG-BLOCK /SEPARATOR-LINE){2}
