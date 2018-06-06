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
;; cxn: Active connection to database.
;; tbl: Table for which SQL frag of columns will be generated.
(define (columns->queryfrag tbl)
  (string-append* (cdr (append* (map
    (lambda (col) (list ", " col))
      (map column->queryfrag (table-columns tbl)))))))

;; Executes SQL command to generate @tbl from @db and @cxn.
;; db: Database proto to be generated.
;; cxn: Active connection to database.
;; tbl: Table for which SQL command will be generated.
(define (table->querycmd db cxn tbl)
  (query-exec cxn (format "create table ~a (~a)"
                          (table-name tbl)
                          (columns->queryfrag tbl))))

;; Executes SQL command to generate @dbuser with permissions to @dbname via
;; @cxn.
;; dbuser: Username for new database user that will be created.
;; dbpw: Password for new database user that will be created.
;; dbpw: Name of database new user will be granted permissions to.
;; cxn: Active connection to database.
(define (createuser dbuser dbpw dbname cxn)
  (begin
       (query-exec cxn (format "create user ~a with password '~a'" dbuser dbpw))
       (query-exec cxn
         (format "grant all privileges on database ~a to ~a" dbname dbuser))))

;; Executes SQL command to generates a new database from @db proto and @old-cxn
;; and generates a new connection to the newly created database.
;; db: Database proto to be generated.
;; old-cxn: Old database connection. Replaced with returned new connection.
(define (createdb->cxn db old-cxn)
  (query-exec old-cxn (format "create database ~a" (database-name db)))
  (createuser (database-username db)
              (database-password db)
              (database-name db) old-cxn)
  (define new-cxn
    (pgdb-cxn (database-name db)
              (database-username db)
              (database-password db)))
  (for-each table->querycmd (list db) (list new-cxn) (database-tables db))
  new-cxn)

;; Top-level orchestrator API to Database. Using this syntax, we can statically
;; create databases, users, and tables e.g. "(db-orc cxn createdb db)".
;; db: Database proto object used for resource generation.
;; cxn: Active connection to database.
;; NOTE: Using format here is acceptable because all base app protos are defined
;; at compile time and do not take user input. These are structures used to
;; build the app itself, and this code is not used at run time.
(define-syntax db-orc
  (syntax-rules (createdb createuser dropdb dropuser)
    ;; Create database @db and return active cxn for newly created database.
    [(db-orc cxn createdb db)
     (createdb->cxn db cxn)]
    ;; Drop database @dbname.
    [(db-orc cxn dropdb dbname)
     (query-exec cxn (format "drop database if exists ~a" dbname))]
    ;; Drop @dbuser.
    [(db-orc cxn dropuser dbuser)
     (query-exec cxn (format "drop user if exists ~a" dbuser))]))

;; TODO(aditya): Expose "SELECT $COL FROM $TABLE", etc getter queries.
;; (define-syntax db-query
  ;; (syntax-rules ()
  ;;   []))

;; Generates SQL frag for the value list of @tbl.
;; row: Row for which SQL frag of column instance will be generated.
(define (values->queryfrag row)
  ;; TODO(aditya): Switch case check that types of row elements are as expected.
  (string-append* (cdr (append* (map
    (lambda (col) (list ", " col)) row)))))

(define (insertdb tablename row cxn)
  (write (format "Running PSQL query: insert into ~a values (~a)"
                 tablename
                 (values->queryfrag row)))
  (query-exec cxn (format "insert into ~a values (~a)"
                           tablename
                           (values->queryfrag row))))

;; TODO(aditya): Expose "INSERT INTO $TABLE VALUES ( ...", etc setter queries.
(define-syntax db-insert
  (syntax-rules (addrow)
    [(db-insert cxn addrow tablename row)
     (insertdb tablename row cxn)]))

(provide pgdb-cxn db-orc db-insert)
