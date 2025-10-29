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


test: "explicit require of default recommendations can be removed"
|-------------------
| require: resyntax/default-recommendations default-recommendations
| no-change-test: "foo"
| - (and a b c)
|===================
| no-change-test: "foo"
| - (and a b c)
|-------------------


no-change-test: "explicit require of other suites not removed"
|-------------------
| require: resyntax/default-recommendations list-shortcuts
| no-change-test: "foo"
| - (and a b c)
|-------------------


no-change-test: "explicit require of default recommendations not removed when other requires present"
|-------------------
| require: resyntax/default-recommendations default-recommendations
| require: resyntax/default-recommendations list-shortcuts
| no-change-test: "foo"
| - (and a b c)
|-------------------
