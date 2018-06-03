#lang racket/base
;; Generated using protoc-gen-racket v1.1
(require (planet murphy/protobuf:1/syntax))

(define-message-type
 column
 ((optional primitive:int64 id 1)
  (optional primitive:string name 2)
  (optional enum:column:datatype datatype 3)))
(define-enum-type column:datatype ((int 0) (float 1) (string 2) (bool 3)))
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
