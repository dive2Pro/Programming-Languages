#lang racket
(define x 1)
(set! x 12)
(define y (begin
            ( + 5 3)
            ( - 3 2))
  )


(define pr (cons 1 ( cons #t "hio")))

(define lst (cons 1 ( cons #t ( cons "hi" (cons "asd" null)))))

; mcons will create a cons that holds mutable functions

(define mpr (mcons 1 (mcons #t "____")))

;(mcons 1 (mcons #t "___"))

(mcar mpr)
; 1
(mcdr mpr)

; (mcons #t "___")

(set-mcar! mpr 2)

;(mcons 2 (mcons #t "__"))

; (length mpr)
; This will cause Runtime error
; A proper list cannot be made of mcons cells