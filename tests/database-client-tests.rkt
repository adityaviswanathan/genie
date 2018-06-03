#lang racket

(require rackunit)
(require db)
(require sql)
(require "../config.rkt")
(require "../database-client.rkt")
(require "../defs/database.rkt")

(test-case
  "Create db test"
  (define cxn
    (pgdb-cxn config/database config/username config/password))
  ;; Setup test db
  (define col1
    (column* #:id 1
             #:name "col1"
             #:datatype 'int))
  (define col2
    (column* #:id 2
             #:name "col2"
             #:datatype 'string))
  (define col3
    (column* #:id 3
             #:name "col3"
             #:datatype 'bool))
  (define tbl1
    (table* #:name "tbl1"
            #:columns (list col1 col2 col3)))
  (define db1
    (database* #:tables (list tbl1)
               #:name "cautiontestdbcaution"
               #:username "cautiontestusernamecaution"
               #:password "cautiontestpasswordcaution"
               #:exposeport 5432))
  (db-exec cxn dropdb (database-name db1))
  (db-exec cxn dropuser (database-username db1))
  (define new-cxn (db-exec cxn createdb db1))
  ;; Do some tests
  (define found-cols
    (query-rows new-cxn
      (string-append "select column_name from "
                     "information_schema.columns "
                     "where table_name = '"
                     (table-name (list-ref (database-tables db1) 0))
                     "'")))
  ;; Wrapper representation to mimic DB query result format.
  (define (expected->vectorize tbl)
    (map vector (map column-name (table-columns tbl))))
  ;; Ensure every column exists the test tables.
  (for-each
    (lambda (expected-tbl found-tbl)
      (for-each
        (lambda (expected-col found-col)
          (check-equal? (vector-ref expected-col 0) (vector-ref found-col 0)))
        (expected->vectorize expected-tbl) found-tbl))
    (database-tables db1) (list found-cols))
  (disconnect new-cxn)
  ;; Delete test resources: database and database user.
  (db-exec cxn dropdb (database-name db1))
  (db-exec cxn dropuser (database-username db1))
  (disconnect cxn))
