# Genie: Microservice generator in Racket

### Object wrapper
- Statically define resources as protos to enable subsequent db generation
- Expose methods to add objects (insert into db)


```
(define db
  (database* #:tables ...
              #:name ...
              #:username ...
              #:password ...
              #:exposeport ...))
```

### DB(Object) wrapper
- Manage a connection
- Execute a query against a known object struct schema

### API(DB(Object)) wrapper
- Expose HTTP server to do CRUD on object
- Can own pool of DB connections
