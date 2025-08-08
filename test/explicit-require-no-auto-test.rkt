#lang resyntax/test

require: resyntax/default-recommendations list-shortcuts

test: "explicit require should not get automatic default-recommendations"
- (and (and 1 2) 3)
