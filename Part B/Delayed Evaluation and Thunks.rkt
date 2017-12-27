#lang racket


(define (if-bad e1 e2 e3)

  (if e1 e2 e3)

  )

(define (factorial x)
  (if (= x 0)
      1
      ( * x (factorial (- x 1)))))


(define (factorial-fail x)
  (if-bad (= x 0 )
          1
          (* x (factorial-fail (- x 1)))))


(define (if-good e1 e2 e3)
  (if e1 (e2) (e3)))
(define (factorial-ok x)
  (if-good (= x 0 )
          (lambda () 1)
          ; this is call thunk
          ; and will holds the lexicol params
          (lambda () (* x (factorial-ok (- x 1))))))

; Avoiding expensive computations
; if they are not needed

