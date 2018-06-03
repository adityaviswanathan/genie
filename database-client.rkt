#lang racket/base

(require db)
(require sql)
(require racket/list)
(require racket/string)
(require "config.rkt")
(require "defs/database.rkt")

(define (pgdb-cxn db username password)
  (postgresql-connect #:user username
                      #:database db
                      #:password password))

(define (type->queryfrag type)
  (case (column:datatype->integer type)
    [(0) "INTEGER"]
    [(1) "FLOAT"]
    [(2) "TEXT"]
    [(3) "BOOL"]))

(define (column->queryfrag col)
  (string-append (column-name col)
                 " "
                 (type->queryfrag (column-datatype col))))

;; Generates SQL frag for the column list of @tbl.
;; db: Database proto object.
;; cxn: Active connection to database.
;; tbl: Table for which SQL frag of columns will be generated.
(define (columns->queryfrag db tbl)
  (string-append* (cdr (append* (map
    (lambda (col) (list ", " col))
      (map column->queryfrag (table-columns tbl)))))))

;; Generates SQL command to generate @tbl from @db @cxn.
;; db: Database proto object to be generated.
;; cxn: Active connection to database.
;; tbl: Table for which SQL command will be generated.
(define (table->querycmd db cxn tbl)
  (query-exec cxn (format "create table ~a (~a)"
                          (table-name tbl)
                          (columns->queryfrag db tbl))))

(define (createdb->cxn db old-cxn)
  (query-exec old-cxn (format "create database ~a" (database-name db)))
  (db-exec old-cxn createuser (database-username db)
                              (database-password db)
                              (database-name db))
  (define new-cxn
    (pgdb-cxn (database-name db)
              (database-username db)
              (database-password db)))
  (for-each table->querycmd (list db) (list new-cxn) (database-tables db))
  new-cxn)

;; Using format here is acceptable because all base app protos are defined at
;; compile time and do not take user input. These are structures used to build
;; the app itself, and this code is not used at run time.
;; Top-level API to Database. Using this syntax, we can statically create
;; databases, users, and tables.
;; e.g. "(db-exec cxn createdb db)"
;; db: Database proto object used for resource generation.
;; cxn: Active connection to database.
(define-syntax db-exec
  (syntax-rules (createdb createuser dropdb dropuser)
    ;; Create database @db and return active cxn for newly created database.
    [(db-exec cxn createdb db)
     (createdb->cxn db cxn)]
    ;; Create @dbuser with @dbpw for database @dbname.
    [(db-exec cxn createuser dbuser dbpw dbname)
     (begin
       (query-exec cxn (format "create user ~a with password '~a'" dbuser dbpw))
       (query-exec cxn
         (format "grant all privileges on database ~a to ~a" dbname dbuser)))]
    ;; Drop database @dbname.
    [(db-exec cxn dropdb dbname)
     (query-exec cxn (format "drop database if exists ~a" dbname))]
    ;; Drop @dbuser.
    [(db-exec cxn dropuser dbuser)
     (query-exec cxn (format "drop user if exists ~a" dbuser))]))

(provide pgdb-cxn db-exec)
