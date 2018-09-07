#lang racket

(require rackunit)
(require db)
(require sql)
(require "../config.rkt")
(require "../database-client.rkt")
(require "../http-server.rkt")
(require (planet murphy/protobuf))
(require (prefix-in proto- "../defs/database.rkt"))
(require net/http-client)

(test-case
  "HTTP wrapper over db test"
  ;; Setup test db and insert dummy data
  (define cxn
    (pgdb-cxn config/database config/username config/password))
  (define col1
    (proto-column* #:id 1
                   #:name "col1"
                   #:datatype 'int))
  (define col2
    (proto-column* #:id 2
                   #:name "col2"
                   #:datatype 'string))
  (define col3
    (proto-column* #:id 3
                   #:name "col3"
                   #:datatype 'bool))
  (define tbl1
    (proto-table* #:name "tbl1"
                  #:columns (list col1 col2 col3)))
  (define db1
    (proto-database* #:tables (list tbl1)
                     #:name config/gen-database
                     #:username config/gen-username
                     #:password config/gen-password
                     #:exposeport 5432))
  (db-admin dropdb cxn (proto-database-name db1))
  (db-admin dropuser cxn (proto-database-username db1))
  (define new-cxn (db-admin createdb cxn db1))
  (define test-row
    (list 1 "hello" #t))
  (db-insert new-cxn "tbl1" test-row)
  ;; Start web server
  (define stop-server (httpdb-serve 8081))
  (define-values (in out) (tcp-connect "localhost" 8081))
  ;; Setup test query
  (define src
    (proto-query-source* #:table "tbl1"))
  (define proj1
    (proto-query-projection* #:column "col1"))
  (define fltr1
    (proto-query-filter* #:column "col1"
                         #:intvalue 1
                         #:datatype 'int))
  (define fltrlist-singleton
    (proto-query-filter-list* #:connective 'or
                              #:filters (list fltr1)))
  (define query
    (proto-query* #:projections (list proj1)
                  #:sources (list src)
                  #:filterlist fltrlist-singleton))
  (define http-cxn
    (http-conn-open "localhost"
                    #:port 8081
                    #:ssl? #f))
  (define serialized-payload (open-output-bytes))
  (serialize query serialized-payload)
  (define-values (status headers http-res-port)
    (http-conn-sendrecv!
      http-cxn "/yoyo"
      #:method "POST"
      #:data (get-output-bytes serialized-payload)
      #:headers (list "Content-Type: application/octet-stream")))
  (printf "STATUS ~s\n" (deserialize (port->string status)))
  (printf "GOT ~s\n" (deserialize (port->bytes http-res-port)))
  (stop-server)
  (disconnect new-cxn)
  (disconnect cxn))
