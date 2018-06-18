#lang racket

(require rackunit)
(require db)
(require sql)
(require "../config.rkt")
(require "../database-client.rkt")
(require (prefix-in proto- "../defs/database.rkt"))

(test-case
  "Create/insert/query db test"
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
  (define fltrlist-innest
    (proto-query-filter-list* #:connective 'or
                              #:filters (list fltr1 fltr2)))
  (define fltrlist1
    (proto-query-filter-list* #:connective 'and
                              #:filters (list fltr1 fltr2 fltr3)
                              #:lists (list fltrlist-innest)))
  (define fltrlist2
    (proto-query-filter-list* #:connective 'and
                              #:filters (list fltr1 fltr3)))
  (define fltrlist3
    (proto-query-filter-list* #:connective 'or
                              #:filters (list fltr2 fltr3)))
  ;; Corresponds to singleton, non-connective filter on value 1.
  (define fltrlist-singleton
    (proto-query-filter-list* #:connective 'or
                              #:filters (list fltr1)))
  ;; Corresponds to filter:
  ;;       OR
  ;;       |
  ;; ---------------
  ;; |     |       |
  ;; 1  "hello"  TRUE
  (define fltrlist-simple-or
    (proto-query-filter-list* #:connective 'or
                              #:filters (list fltr1 fltr2 fltr3)))
  ;; Corresponds to filter:
  ;;      AND
  ;;       |
  ;; ---------------
  ;; |     |       |
  ;; 1  "hello"  TRUE
  (define fltrlist-simple-and
    (proto-query-filter-list* #:connective 'and
                              #:filters (list fltr1 fltr2 fltr3)))
  ;; Corresponds to filter:
  ;;                           OR
  ;;                           |
  ;;  ---------------------------------------------------
  ;;  |     |      |      |               |             |
  ;;  1  "hello"  TRUE   AND             AND            OR
  ;;                      |               |             |
  ;;             -------------------    -----        --------
  ;;             |     |      |    |    |   |        |      |
  ;;             1  "hello"  TRUE  OR   1  TRUE   "hello"  TRUE
  ;;                               |
  ;;                            -------
  ;;                            |     |
  ;;                            1  "hello"
  (define fltrlist-deepnested
    (proto-query-filter-list* #:connective 'or
                              #:filters (list fltr1 fltr2 fltr3)
                              #:lists (list fltrlist1 fltrlist2 fltrlist3)))
  ;; Query projecting one column via a single filter.
  (define q1
    (proto-query* #:projections (list proj1)
                  #:sources (list src)
                  #:filterlist fltrlist-singleton))
  ;; Query projecting one column via an OR filterlist over three filters.
  (define q2
    (proto-query* #:projections (list proj1)
                  #:sources (list src)
                  #:filterlist fltrlist-simple-or))
  ;; Query projecting one column via an AND filterlist over three filters.
  (define q3
    (proto-query* #:projections (list proj1)
                  #:sources (list src)
                  #:filterlist fltrlist-simple-and))
  ;; Query projecting three columns via an AND filterlist over three filters.
  (define q4
    (proto-query* #:projections (list proj1 proj2 proj3)
                  #:sources (list src)
                  #:filterlist fltrlist-simple-and))
  ;; Query projecting three columns via an deeply nested filterlist.
  (define q5
    (proto-query* #:projections (list proj1 proj2 proj3)
                  #:sources (list src)
                  #:filterlist fltrlist-deepnested))
  ;; Verify test queries via query API.
  ;; TODO(aditya): Test queries across various sources.
  (check-equal? (db-query new-cxn q1) (list (vector 1)))
  (check-equal? (db-query new-cxn q2) (list (vector 1)))
  (check-equal? (db-query new-cxn q3) (list (vector 1)))
  (check-equal? (db-query new-cxn q4) (list (vector 1 "hello" #t)))
  (check-equal? (db-query new-cxn q5) (list (vector 1 "hello" #t)))
  (disconnect new-cxn)
  ;; Delete test resources: database and database user.
  (db-admin dropdb cxn (proto-database-name db1))
  (db-admin dropuser cxn (proto-database-username db1))
  (disconnect cxn))
