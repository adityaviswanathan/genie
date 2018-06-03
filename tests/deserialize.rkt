#lang racket

(require (planet murphy/protobuf))
(require "../defs/database.rkt")

(column-name (car (table-columns (deserialize (table*)))))
