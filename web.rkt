#lang racket

(require web-server/servlet
         web-server/servlet-env)

(struct elem (classname contents))

(define frontend
  (list (elem "Someclass" "some stuff here 1")
        (elem "Someclass" "some stuff here 2")))

(define (my-app req)
  (response/xexpr
    `(html (head (title "Hello world!"))
           (body (p "Hey out there!")
                 ,(render-divs frontend)))))

; render-class: divs -> xexpr
; Generates wrapper for list of divs with classes.
(define (render-divs divs)
  `(div ((class "render-divs-wrapper"))
        ,@(map render-elem divs)))

; render-elem: name contents -> xexpr
; Generates wrapper div with @classname filled with @contents.
(define (render-elem elem)
  `(div ((class ,(elem-classname elem)))
        (p ,(elem-contents elem))))

(serve/servlet my-app
               #:servlet-path "/hello")
