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

;; Executes SQL command to generate a new database from @db proto and @old-cxn
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
  ;; Maps @unwrapfunc to each element in @lst and comma delimits the
  ;; resulting list into a string of the form "elemA, elemB, elemC".
  ;; lst: List to comma delimit.
  ;; unwrapfunc: Func to apply to each element in @lst before inserting into
  ;;             into comma delimited string list.
  (define (comma-delimit lst unwrapfunc)
    (string-append* (cdr (append* (map
      (lambda (i) (list ", " i)) (map unwrapfunc lst))))))
  ;; Slices @lst until but not including index @end.
  (define (list->prefix lst end)
    (append
      (for/list ([i (in-range end)])
                (list-ref lst i))))
  ;; Computes the recursive length of a FilterList proto.
  ;; fltrlist: FilterList proto whose length will be computed.
  ;; idx: Index of @fltrlist, used to control recursive length calculation.
  (define (filterlist->index fltrlist idx)
    ;; Slice of @fltrlist up till @idx.
    (define prefix (list->prefix (proto-query-filter-list-lists fltrlist) idx))
    (define curr 
      (for/list ([i (in-range (length prefix))])
        (filterlist->index (list-ref prefix i)
                           (length (proto-query-filter-list-lists (list-ref prefix i))))))
    (apply +
      (append (map length (map proto-query-filter-list-filters prefix)) curr)))
  ;; Generates a list of the contents of a FilterList proto. The filters are in
  ;; the output list are placeholder strings of the form "<COL> = $<IDX>".
  ;; fltrlist: FilterList proto for which the contents will be generated.
  ;; curridx: Start index of this list for recursive traversal of FilterList.
  (define (filterlist->filters fltrlist curridx)
    (define openparen "(")
    (define closeparen ")")
    (define equals " = $") ;; TODO(aditya): Generalize filter operator support.
    (append
      (for/list ([i (in-range (length (proto-query-filter-list-filters fltrlist)))])
        (define fltr (list-ref (proto-query-filter-list-filters fltrlist) i))
        (string-append (proto-query-filter-column fltr)
                        equals
                        (number->string (+ curridx (+ i 1)))))
      (for/list ([i (in-range (length (proto-query-filter-list-lists fltrlist)))])
        (define fltr (list-ref (proto-query-filter-list-lists fltrlist) i))
        (define baseindex (length (proto-query-filter-list-filters fltrlist)))
        (define deepindex (filterlist->index fltrlist i))
        (string-append openparen 
                       (filterlist->queryfrag fltr (+ curridx (+ baseindex deepindex)))
                       closeparen))))
  ;; Emits the SQL equivalent of an ordered traversal of the composite filters
  ;; in a FilterList proto and returns a SQL fragment of the form:
  ;; "<COL> = $<IDX> <CONNECTIVE> ..." e.g. "name = $1 and type = $2".
  ;; This is used in the query-rows call later. Note that filter values are not
  ;; inserted into SQL here but are instead bound via the query-rows call and
  ;; the production of the filter value list via filterlist->values.
  ;; fltrlist: FilterList proto to be traversed.
  ;; curridx: Index of the current FilterList, required for recursive traversal.
  (define (filterlist->queryfrag fltrlist curridx)
    ;; The sum of this FilterList's filters and nested FilterLists is the
    ;; net contribution of this FilterList to the overall count of filters in
    ;; the query and tells us how many connectives are required for this frag.
    (define currlength (+ (length (proto-query-filter-list-filters fltrlist))
                          (length (proto-query-filter-list-lists fltrlist))))
    ;; Build the SQL frag by constructing the current FilterProto artifacts
    ;; and recursing on all nested FilterProtos.
    (string-append*
      (cdr (append*
        (map (lambda (connective col) (list connective col))
          (for/list ([i (in-range currlength)])
            (case (proto-query-filter-list:e->integer
                    (proto-query-filter-list-connective fltrlist))
              [(0) " and "]
              [(1) " or "]))
          (filterlist->filters fltrlist curridx))))))
  ;; SQL statement skeleton for DB query. Currently supports placeholders for
  ;; projections, sources, and filters in a "select ... from ... where ... "
  ;; statement.
  (define sql-query
    (format "select ~a from ~a where ~a"
            (comma-delimit (proto-query-projections query)
                           proto-query-projection-column)
            (comma-delimit (proto-query-sources query)
                           proto-query-source-table)
            (filterlist->queryfrag (proto-query-filterlist query) 0)))
  ;; Flattens a list of FilterList protos into an ordered list of their
  ;; corresponding values.
  ;; fltrlist: FilterList proto from which to produce a list of values.
  (define (filterlist->values fltrlist)
    ;; Because FilterList proto can nest other FilterLists, we first extract
    ;; the top level filter values and then append to the values extracted from
    ;; each nested FilterList.
    ;; Collect top-level filter values.
    (define basefilters
      (for/list ([i (in-range (length (proto-query-filter-list-filters fltrlist)))])
        (define fltr (list-ref (proto-query-filter-list-filters fltrlist) i))
        (list (case (proto-datatype:e->integer
                      (proto-query-filter-datatype fltr))
                [(0) (proto-query-filter-intvalue fltr)]
                [(1) (proto-query-filter-floatvalue fltr)]
                [(2) (proto-query-filter-stringvalue fltr)]
                [(3) (proto-query-filter-boolvalue fltr)]))))
    ;; Recurse on each nested FilterList.
    (define deepfilters
      (map filterlist->values (proto-query-filter-list-lists fltrlist)))
    ;; Collect all filter values on all levels and flatten into single list.
    (flatten (append basefilters deepfilters)))
  (apply query-rows
    (append (list cxn sql-query)
            (filterlist->values (proto-query-filterlist query)))))

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
