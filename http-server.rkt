#lang racket

(require racket/trace)
(require "config.rkt")
(require "database-client.rkt")
(require (planet murphy/protobuf))
(require (prefix-in proto- "defs/database.rkt"))
(require xml net/url)

;; Primary HTTP server that is managed via a custodian container, runs in
;; background thread.
;; port-no: IP port on which serve resource.
(define (serve port-no)
  (define main-cust (make-custodian))
  (parameterize ([current-custodian main-cust])
    (define listener (tcp-listen port-no 5 #t))
    (define (loop)
      (accept-and-handle listener)
      (loop))
    (thread loop))
  (lambda ()
    (custodian-shutdown-all main-cust)))

;; Resource endpoint dispatcher managed via custodian container. Each
;; connnection is handled by a thread, enabling the server to handle multiple
;; requests simultaneously.
;; listener: Server's TCP connection listener.
(define (accept-and-handle listener)
  (define cust (make-custodian))
  (custodian-limit-memory cust (* 50 1024 1024)) ;; 50MB memory limit/cxn.
  (parameterize ([current-custodian cust])
    (define-values (in out) (tcp-accept listener))
    (thread (lambda ()
              ;; (handle in out)
              (handle-proto in out)
              (close-input-port in)
              (close-output-port out))))
  ;; Watcher thread, kills connections that don't send requests within 10 sec.
  (thread (lambda ()
            (sleep 10)
            (custodian-shutdown-all cust))))

;; Handles proto communication over HTTP.
;; in: Input request port.
;; out: Output response port.
(define (handle-proto in out)
  (define route
    ;; Match the route stub of the POST request:
    ;; E.g. "/endpoint" in "POST /endpoint HTTP/1.1 ..."
    (regexp-match #rx"^POST (.+) HTTP/[0-9]+\\.[0-9]+"
                  (read-line in)))
  (define datasz 
    ;; Match Content-Length field of the POST request:
    (regexp-match #rx"Content-Length: ([0-9]+)"
                  in))
  (when (and route datasz)
    (printf "[--------- Received query at url: ~s ---------]\n" (list-ref route 1))
    (define numbytes
      (string->number (bytes->string/utf-8 (list-ref datasz 1))))
    (printf "[--------- Query payload size ~s ---------]\n" numbytes)
    ;; Discard the rest of the header (up to and including blank line), yielding only data:
    (define header (regexp-match #rx"[^\r\n]\r\n\r\n" in))
    (when header
      ;; Read input payload into bytestring and deserialize.
      (define (readme payload bytesleft bytesread)
        (define byteread (open-output-bytes))
        (if (> bytesleft 0)
          (let ([c (read-byte payload)])
            (write-byte c byteread)
            (readme payload (- bytesleft 1) (bytes-append bytesread (get-output-bytes byteread))))
          bytesread))
      (define empty-bytes (open-output-bytes))
      (define serialized-payload-in (open-input-bytes (readme in numbytes (get-output-bytes empty-bytes))))
      (define deserialized-payload (deserialize (proto-query*) serialized-payload-in))
      ;; Connect to db using config and run query.
      (define db-cxn
        (pgdb-cxn config/gen-database config/gen-username config/gen-password))
      (define res (db-query db-cxn deserialized-payload))
      (printf "[--------- Query result: ~s ---------]\n" res)
      ;; TODO(aditya): Figure out correct way to send response to client.
      (display res out))))
    
;; Top-level handler for dispatching based on request @in. Response is written
;; to @out.
;; in: Input request port.
;; out: Output response port.
(define (handle in out)
  (define req
    ;; Match the first line to extract the request:
    (regexp-match #rx"^GET (.+) HTTP/[0-9]+\\.[0-9]+"
                  (read-line in)))
  (when req
    ;; Discard the rest of the header (up to blank line):
    (regexp-match #rx"(\r\n|^)\r\n" in)
    ;; Dispatch:
    (let ([xexpr (dispatch (list-ref req 1))])
      ;; Send reply:
      (display "HTTP/1.0 200 Okay\r\n" out)
      (display "Server: k\r\nContent-Type: text/html\r\n\r\n" out)
      (display (xexpr->string xexpr) out))))

;; Generates an xexpr if @str-path maps to a valid dispatch endpoint.
;; str-path: String URL path from request, decoded to dispatch endpoint.
(define (dispatch str-path)
  ;; Parse the request as a URL:
  (define url (string->url str-path))
  ;; Extract the path part:
  (define path (map path/param-path (url-path url)))
  ;; Find a handler based on the path's first element:
  (define h (hash-ref dispatch-table (car path) #f))
  ;; Define an error page.
  (define errpage
    `(html (head (title "Error"))
      (body
       (font ((color "red"))
             "Unknown page: "
             ,str-path))))
  (if h (h (url-query url)) errpage))

(define dispatch-table (make-hash))

;; A simple dispatcher:
(hash-set! dispatch-table "hello"
           (lambda (query)
             `(html (body "Hello, World!"))))

(define-syntax httpdb-serve
  (syntax-rules ()
    [(httpdb-serve port)
     (serve port)]))

(provide httpdb-serve)
