#lang racket

(require rackunit)
(require "../defs/database.rkt")

(test-case
  "Database proto test"
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
               #:name "db1"
               #:username "un"
               #:password "pw"
               #:exposeport 5432))
  (check-eq? (database-name db1) "db1")
  (check-eq? (table-name (car (database-tables db1))) "tbl1")
  (check-eq? (table-name tbl1) "tbl1")
  (check-eq? (car (table-columns tbl1)) col1)
  (check-eq? (column-name (car (table-columns tbl1))) "col1")
  (check-eq? (cadr (table-columns tbl1)) col2)
  (check-eq? (column-name (cadr (table-columns tbl1))) "col2")
  (check-eq? (caddr (table-columns tbl1)) col3)
  (check-eq? (column-name (caddr (table-columns tbl1))) "col3")
  (check-eq? (column-name (caddr (table-columns tbl1))) "col3")
  (check-eq?
    (column:datatype->integer (column-datatype (car (table-columns tbl1))))
    (column:datatype->integer 'int)))
