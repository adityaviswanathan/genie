#lang racket

(require (planet murphy/protobuf))
(require "../defs/database.rkt")

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
(serialize tbl1)
