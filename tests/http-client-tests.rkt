#lang racket

(require rackunit)
(require db)
(require sql)
(require "../config.rkt")
(require "../database-client.rkt")
(require "../http-client.rkt")
(require (prefix-in proto- "../defs/database.rkt"))

(test-case
  "HTTP wrapper over db test"
  (define stop-server (httpdb-serve 8081))
  (stop-server))
