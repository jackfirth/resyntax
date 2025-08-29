#lang resyntax/test


header: - #lang resyntax/test


test: "unnecessary multi-line code blocks in tests refactorable to single-line code blocks"
|-------------------
| test: "foo"
| ------------------
| (and a (and b c))
| ==================
| (and a b c)
| ------------------
|===================
| test: "foo"
| ------------------
| (and a (and b c))
| ------------------
| ------------------
| (and a b c)
| ------------------
|===================
| test: "foo"
| - (and a (and b c))
| - (and a b c)
|-------------------
