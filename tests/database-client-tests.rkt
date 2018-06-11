#lang racket

(require rackunit)
(require db)
(require sql)
(require "../config.rkt")
(require "../database-client.rkt")
(require (prefix-in proto- "../defs/database.rkt"))

(test-case
  "Create db test"
  (define cxn
    (pgdb-cxn config/database config/username config/password))
  ;; Setup test db
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
                     #:name "cautiontestdbcaution"
                     #:username "cautiontestusernamecaution"
                     #:password "cautiontestpasswordcaution"
                     #:exposeport 5432))
  (db-admin dropdb cxn (proto-database-name db1))
  (db-admin dropuser cxn (proto-database-username db1))
  (define new-cxn (db-admin createdb cxn db1))
  ;; Test database admin API.
  ;; Get tables (and composite columns) matching those added via API.
  (define found-tbls
    (build-list
      (length (proto-database-tables db1))
      (lambda (i)
        (query-rows new-cxn
          (string-append "select column_name from "
                         "information_schema.columns "
                         "where table_name = '"
                         (proto-table-name (list-ref (proto-database-tables db1) i))
                         "'")))))
  ;; Wrapper representation to mimic DB query result format.
  (define (expected->vectorize tbl)
    (map vector (map proto-column-name (proto-table-columns tbl))))
  ;; Ensure every column exists the test tables.
  (for-each
    (lambda (expected-tbl found-tbl)
      (for-each
        (lambda (expected-col found-col)
          (check-equal? (vector-ref expected-col 0) (vector-ref found-col 0)))
        (expected->vectorize expected-tbl) found-tbl))
    (proto-database-tables db1) found-tbls)
  ;; Test database insert API.
  (define test-row
    ;; TODO(aditya): Raw PSQL types should be casted correctly.
    (list 1 "hello" #t))
  (db-insert new-cxn "tbl1" test-row)
  ;; Build test queries.
  (define src
    (proto-query-source* #:table "tbl1"))
  (define proj1
    (proto-query-projection* #:column "col1"))
  (define proj2
    (proto-query-projection* #:column "col2"))
  (define proj3
    (proto-query-projection* #:column "col3"))
  (define fltr1
    (proto-query-filter* #:column "col1"
                         #:intvalue 1
                         #:datatype 'int))
  (define fltr2
    (proto-query-filter* #:column "col2"
                         #:stringvalue "hello"
                         #:datatype 'string))
  (define fltr3
    (proto-query-filter* #:column "col3"
                         #:boolvalue #t
                         #:datatype 'bool))
  (define q1
    (proto-query* #:projections (list proj1)
                  #:sources (list src)
                  #:filters (list fltr1)))
  (define q2
    (proto-query* #:projections (list proj1)
                  #:sources (list src)
                  #:filters (list fltr2)))
  (define q3
    (proto-query* #:projections (list proj1)
                  #:sources (list src)
                  #:filters (list fltr3)))
  (define q4
    (proto-query* #:projections (list proj2)
                  #:sources (list src)
                  #:filters (list fltr1)))
  (define q5
    (proto-query* #:projections (list proj2)
                  #:sources (list src)
                  #:filters (list fltr2)))
  (define q6
    (proto-query* #:projections (list proj2)
                  #:sources (list src)
                  #:filters (list fltr3)))
  (define q7
    (proto-query* #:projections (list proj3)
                  #:sources (list src)
                  #:filters (list fltr1)))
  (define q8
    (proto-query* #:projections (list proj3)
                  #:sources (list src)
                  #:filters (list fltr2)))
  (define q9
    (proto-query* #:projections (list proj3)
                  #:sources (list src)
                  #:filters (list fltr3)))
  ;; Verify test queries.
  (check-equal? (db-query new-cxn q1) (list (vector 1)))
  (check-equal? (db-query new-cxn q2) (list (vector 1)))
  (check-equal? (db-query new-cxn q3) (list (vector 1)))
  (check-equal? (db-query new-cxn q4) (list (vector "hello")))
  (check-equal? (db-query new-cxn q5) (list (vector "hello")))
  (check-equal? (db-query new-cxn q6) (list (vector "hello")))
  (check-equal? (db-query new-cxn q7) (list (vector #t)))
  (check-equal? (db-query new-cxn q8) (list (vector #t)))
  (check-equal? (db-query new-cxn q9) (list (vector #t)))
  (disconnect new-cxn)
  ;; Delete test resources: database and database user.
  (db-admin dropdb cxn (proto-database-name db1))
  (db-admin dropuser cxn (proto-database-username db1))
  (disconnect cxn))
