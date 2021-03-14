#lang brag

refactoring-test: refactoring-test-import* refactoring-test-header? refactoring-test-case*
refactoring-test-import: /REQUIRE-KEYWORD IDENTIFIER IDENTIFIER
refactoring-test-header: /HEADER-KEYWORD CODE-BLOCK
refactoring-test-case: /TEST-KEYWORD STRING-LITERAL CODE-BLOCK{1,2}
