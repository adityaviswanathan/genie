#lang racket/base

(require db)
(require sql)
(require racket/list)
(require racket/string)
(require "config.rkt")
(require (prefix-in proto- "defs/database.rkt"))

(define (pgdb-cxn db username password)
  (postgresql-connect #:user username
                      #:database db
                      #:password password))

(define (type->queryfrag type)
  (case (proto-datatype:e->integer type)
    [(0) "INTEGER"]
    [(1) "FLOAT"]
    [(2) "TEXT"]
    [(3) "BOOL"]))

(define (column->queryfrag col)
  (string-append (proto-column-name col)
                 " "
                 (type->queryfrag (proto-column-datatype col))))

;; Generates SQL frag for the column list of @tbl.
;; cxn: Active connection to database.
;; tbl: Table for which SQL frag of columns will be generated.
(define (columns->queryfrag tbl)
  (string-append* (cdr (append* (map
    (lambda (col) (list ", " col))
      (map column->queryfrag (proto-table-columns tbl)))))))

;; Executes SQL command to generate @tbl from @db and @cxn.
;; db: Database proto to be generated.
;; cxn: Active connection to database.
;; tbl: Table for which SQL command will be generated.
(define (table->querycmd db cxn tbl)
  (query-exec cxn (format "create table ~a (~a)"
                          (proto-table-name tbl)
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
  (query-exec old-cxn (format "create database ~a" (proto-database-name db)))
  (createuser (proto-database-username db)
              (proto-database-password db)
              (proto-database-name db) old-cxn)
  (define new-cxn
    (pgdb-cxn (proto-database-name db)
              (proto-database-username db)
              (proto-database-password db)))
  (for-each table->querycmd (list db) (list new-cxn) (proto-database-tables db))
  new-cxn)

;; Top-level administrator API to Database. Using this syntax, we can statically
;; create databases, users, and tables e.g. "(db-admin createdb cxn db)".
;; db: Database proto object used for resource generation.
;; cxn: Active connection to database.
;; NOTE: Using format here is acceptable because all base app protos are defined
;; at compile time and do not take user input. This syntax generates
;; well-formed SQL programs.
(define-syntax db-admin
  (syntax-rules (createdb createuser dropdb dropuser)
    ;; Create database @db and return active cxn for newly created database.
    [(db-admin createdb cxn db)
     (createdb->cxn db cxn)]
    ;; Drop database @dbname.
    [(db-admin dropdb cxn dbname)
     (query-exec cxn (format "drop database if exists ~a" dbname))]
    ;; Drop @dbuser.
    [(db-admin dropuser cxn dbuser)
     (query-exec cxn (format "drop user if exists ~a" dbuser))]))

;; Executes SQL command to run @query and returns rows of data.
;; cxn: Active connection to database.
;; query: Query proto object used to execute the query against database.
(define (query->rows cxn query)
  (define (projections->queryfrag projs)
    (string-append* (cdr (append* (map
      (lambda (i) (list ", " i)) (map proto-query-projection-column projs))))))
  (define (sources->queryfrag sources)
    (string-append* (cdr (append* (map
      (lambda (i) (list ", " i)) (map proto-query-source-table sources))))))
  (define (list->prefix lst end)
    (append
      (for/list ([i (in-range end)])
                (list-ref lst i))))
  (define (filterlist->length fltrlist idx)
    (define prefix (list->prefix (proto-query-filter-list-lists fltrlist) idx))
    (define childlengths
      (for/list ([i (in-range (length prefix))])
                (filterlist->length (list-ref prefix i) (length (proto-query-filter-list-lists (list-ref prefix i))))))
    (apply +
      (append (map length (map proto-query-filter-list-filters prefix))
              childlengths)))
  (define (filterlist-contents->queryfrag fltrlist curridx)
    (append
      (for/list ([i (in-range (length (proto-query-filter-list-filters fltrlist)))])
                (string-append (proto-query-filter-column (list-ref (proto-query-filter-list-filters fltrlist) i))
                                " = $" ;; TODO(aditya): Generalize filter operator support.
                                (number->string (+ curridx (+ i 1)))))
      (for/list ([i (in-range (length (proto-query-filter-list-lists fltrlist)))])
                (string-append "("
                               (filterlist->queryfrag
                                 (list-ref (proto-query-filter-list-lists fltrlist) i)
                                 (+ curridx (+ (filterlist->length fltrlist i)
                                               (length (proto-query-filter-list-filters fltrlist)))))
                               ")"))))
  (define (filterlist->queryfrag fltrlist curridx)
    (string-append*
      (cdr (append*
        (map (lambda (connective col) (list connective col))
          (for/list ([i (in-range (+ (length (proto-query-filter-list-filters fltrlist))
                                     (length (proto-query-filter-list-lists fltrlist))))])
            (case (proto-query-filter-list:e->integer (proto-query-filter-list-connective fltrlist))
              [(0) " and "]
              [(1) " or "]))
          (filterlist-contents->queryfrag fltrlist curridx))))))
  (define sql-query
    (format "select ~a from ~a where ~a"
            (projections->queryfrag (proto-query-projections query))
            (sources->queryfrag (proto-query-sources query))
            (filterlist->queryfrag (proto-query-filterlist query) 0)))
  (define (filterlist->filters fltrlist)
    (define basefilters
      (for/list ([i (in-range (length (proto-query-filter-list-filters fltrlist)))])
        (list (case (proto-datatype:e->integer (proto-query-filter-datatype (list-ref (proto-query-filter-list-filters fltrlist) i)))
                [(0) (proto-query-filter-intvalue (list-ref (proto-query-filter-list-filters fltrlist) i))]
                [(1) (proto-query-filter-floatvalue (list-ref (proto-query-filter-list-filters fltrlist) i))]
                [(2) (proto-query-filter-stringvalue (list-ref (proto-query-filter-list-filters fltrlist) i))]
                [(3) (proto-query-filter-boolvalue (list-ref (proto-query-filter-list-filters fltrlist) i))]))))
    (define deepfilters
      (map filterlist->filters (proto-query-filter-list-lists fltrlist)))
    (flatten (append basefilters deepfilters)))
  (apply query-rows
    (append (list cxn sql-query)
            (filterlist->filters (proto-query-filterlist query)))))

;; Query API to Database. Using this syntax, we can query the database using a
;; Query proto and conntection e.g. "(db-query cxn query)".
;; cxn: Active connection to database.
;; query: Query proto object used to execute the query against database.
;; NOTE: Using format here is acceptable because the SQL builders are macros
;; that correctly pass user values as arguments to query-rows, not as literals.
(define-syntax db-query
  (syntax-rules ()
    [(db-query cxn query)
     (query->rows cxn query)]))

;; Generates SQL frag for the value list of @tbl.
;; row: Row for which SQL frag of column instance will be generated.
(define (colindexes->queryfrag row)
  (string-append* (cdr (append*
    (map (lambda (col) (list ", " col))
      (for/list ([i (in-range (length row))])
                (string-append "$"
                               (number->string (+ i 1)))))))))

;; Executes SQL command to insert @row into @tablename.
;; cxn: Active connection to database.
;; tablename: Table into which @row will be inserted.
;; row: Row of data that will be inserted.
(define (insertdb cxn tablename row)
  (define sql-stmt
    (format "insert into ~a values (~a)"
            tablename
            (colindexes->queryfrag row)))
  (apply query-exec (append (list cxn sql-stmt) row)))

;; Insert API to Database. Using this syntax, we can insert rows into the database
;; using a Query proto and connection e.g. "(db-insert cxn "table" row)".
;; cxn: Active connection to database.
;; tablename: Table into which @row will be inserted.
;; row: Row of data that will be inserted.
;; NOTE: Using format here is acceptable because the SQL builders are macros
;; that correctly pass user values as arguments to query-exec, not as literals.
(define-syntax db-insert
  (syntax-rules ()
    [(db-insert cxn tablename row)
     (insertdb cxn tablename row)]))

(provide pgdb-cxn db-admin db-insert db-query)
