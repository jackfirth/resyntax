#lang resyntax/testing/refactoring-test


require: resyntax/default-recommendations web-server-recommendations


header:
------------------------------------------------------------------------------------------------------
#lang racket/base
(require web-server/http/request-structs)
------------------------------------------------------------------------------------------------------


test: "headers-assq is replaced with headers-assq*"
- (headers-assq #"Cache-Control" '())
- (headers-assq* #"Cache-Control" '())


test: "higher-order headers-assq is replaced with headers-assq*"
- (map headers-assq (list #"Cache-Control") (list '()))
- (map headers-assq* (list #"Cache-Control") (list '()))
