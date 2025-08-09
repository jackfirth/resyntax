#lang resyntax/test

require: resyntax/default-recommendations list-shortcuts

no-change-test: "explicit require should not get automatic default-recommendations"
- (and (and 1 2) 3)
