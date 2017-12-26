#lang racket


(define (my-map f xs )
  (if (null? xs)
      null
      (cons (f (car xs))
            (my-map f (cdr xs))
            )
      )
  )
(define foo (my-map (lambda (x)
                      ( + x 1)
                      )
                    (cons 3 (cons 4 (cons 5 null)))
                    )
  )

; Parentheses matter
; Parentheses always has meaning
; ((e)) mean call e and call e's called result

; cond
; a new Better style than if

(define (sum1 xs)
  (if (null? xs)
      0
      (if (number? (car xs))
          (+ (car xs) (sum1 (cdr xs)))
          (if (list? (car xs))
               (+ (sum1 (car xs)) (sum1 (cdr xs)))
             
               (sum1 (cdr xs))
             
          )
       )
   )
)

(define (sum2 xs)
  (cond [(null? xs) 0]
        [(number? (car xs)) (+ (car xs) (sum1 (cdr xs)))]
        [(list? (car xs)) (+ (sum2 (car xs)) (sum1 (cdr xs)))]
        [#t (+ (sum1 (cdr xs)))]
        ))

; #f is unique
(define (count-falses xs)
  (cond [(null? xs) 0]
        [(car xs) (count-falses (cdr xs))]
        [#t ( + 1 (count-falses (cdr xs)))]
        )
  )