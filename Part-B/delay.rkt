#lang racket
(define (my-mult x y-thunk)
  (cond [( = x 0) 0]
        [( = x 1) (y-thunk)]
        [#t ( + (y-thunk) (my-mult (- x 1) y-thunk))]
        ))

(define (my-delay th)
  (mcons #f th))

(define (delay-computatoin x y)
  (letrec (
        [start (lambda (j k)
               (cond
                 [(= k 0) (+ x y)]
                 [#t (start j (- k 1))])
                 )
         ]
        )
    (start x 150000000)))

(define (my-force p )
  (if (mcar p )
      (mcdr p)
      (begin
        (set-mcar! p #t)
        (set-mcdr! p ((mcdr p)))
        (mcdr p)
        )))

;(define (my-mult2 x y-thunk)
 ; )

(my-mult 1 (lambda () (delay-computatoin 3 4)))

(my-mult 12 (let
               ([p (my-delay
                     (lambda ()
                       (delay-computatoin 3 4)))]
                )
             (lambda () (my-force p))))

             
