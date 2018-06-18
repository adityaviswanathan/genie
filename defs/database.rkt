#lang racket/base
;; Generated using protoc-gen-racket v1.1
(require (planet murphy/protobuf:1/syntax))

(define-message-type datatype ())
(define-enum-type datatype:e ((int 0) (float 1) (string 2) (bool 3)))
(define-message-type query-projection ((optional primitive:string column 1)))
(define-message-type query-source ((optional primitive:string table 1)))
(define-message-type
 query-filter
 ((optional primitive:string column 1)
  (optional enum:datatype:e datatype 2)
  (optional primitive:int64 intvalue 3)
  (optional primitive:float floatvalue 4)
  (optional primitive:string stringvalue 5)
  (optional primitive:bool boolvalue 6)))
(define-message-type
 query-filter-list
 ((optional enum:query-filter-list:e connective 1)
  (repeated struct:query-filter filters 2)
  (repeated struct:query-filter-list lists 3)))
(define-enum-type query-filter-list:e ((and 0) (or 1)))
(define-message-type
 query
 ((repeated struct:query-projection projections 1)
  (repeated struct:query-source sources 2)
  (repeated struct:query-filter filters 3)
  (optional struct:query-filter-list filterlist 4)))
(define-message-type
 entry
 ((optional struct:column column 1)
  (optional primitive:int64 intvalue 2)
  (optional primitive:float floatvalue 3)
  (optional primitive:string stringvalue 4)
  (optional primitive:bool boolvalue 5)))
(define-message-type
 column
 ((optional primitive:int64 id 1)
  (optional primitive:string name 2)
  (optional enum:datatype:e datatype 3)))
(define-message-type
 table
 ((optional primitive:string name 1) (repeated struct:column columns 2)))
(define-message-type
 database
 ((repeated struct:table tables 1)
  (optional primitive:string name 2)
  (optional primitive:string username 3)
  (optional primitive:string password 4)
  (optional primitive:int64 exposeport 5)))

(provide (all-defined-out))
