#lang racket

(define-syntax my-if
  (syntax-rules (then else)
    [(my-if e1 then e2 else e3)
     (if (e1) e2 e3)]))

(define-syntax dbl
  (syntax-rules ()
    [(dbl x) (* 2 x)]))


(define-syntax for
  (syntax-rules (to do)
    [(for lo to hi do body)
     (let ([l lo]
           (h hi)
           )
       (letrec ([loop (lambda (it)
                        (if (> it h)
                            #t
                            ; here is call body
                            (begin (body it) (loop ( + it 1 )))))])
         (loop l)))]))