#lang racket

; local variables

(define (silly-double x)
  (let (
        [x ( + x 3)]
        [y ( + x 2)]
        )
    (+ x y -5)))


(define (change-variables j k )
  (
   let
      (
       ; in Racket these lines will change the variables
       [j k]
       [k j]
       )
    (list j k)))



; let* = ML let

(define (change-variables2 j k )
  (
   let*
      ; The expressions are evaluated in the environment produced from
      ; the previous bindings
      (
       [j k]
       [k j]
       )
    (list j k)))

; letrec 
(define (silly-trible x )
  (letrec
      ; hover cursor on the letter
      ; you will see the bindings
      ; remember , Racket produce an error if use a variable
      ; before it is defined in letrec
      ( [y (+ x 2)]
        [f (lambda (z) (+ z y w x))]
        [w ( + x 7 )]
        )
    (f -9)
    ))
; letrec 
(define (silly-trible2 x )
  (define
      ; hover cursor on the letter
      ; you will see the bindings
      ; remember , Racket produce an error if use a variable
      ; before it is defined in letrec
      ( [y (lambda (x) (+ x 2))]
       
        )
    (f -9)
    ))
