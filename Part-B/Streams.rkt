#lang racket

; Streams as a thunk
; think of it it a stream event

; pair (value thunk)

; How to use a stream
(define (number-until stream tester)
  (letrec ([f (lambda (stream ans)
             (let* ([pr (stream)])
               (if (tester (car pr))
                   ans
                   (f (cdr pr) (+ ans 1))))
             )])
    (f stream 1)))

; how to defining streams
; '(next-answer . next-thunk)

(define (f x) (cons x (lambda () (f ( + x 1)))))

(define power-of-two
         (letrec (
                  [f (lambda (x)
                        (cons x (lambda () (f (* x 2)))))]
                  )
           (lambda () (f 1 )))
         )
                       
; Memoization ( avoid repeat computation)
; Memoization

; fibonacci

(define (fibonacci1 x)
  (letrec ([f (lambda (y)
                (if (or (= y 1) (= y 2))
                    1
                    (+ (f (- y 1))
                       (f (- y 2))
                       )
                    ))])
    (f x)
))

(define (fibonacci2 x)
 ; memo table
 ; (let ([f
 ; memo 中是以 (ans value) 保存
 ; 在 f 中
  ; 从 memo 中根据 ans 拿 value
  ; 如果 有 value  返回之
  ; 如果 没有 计算之
  ;     k = ( a + b , b ans +1 )
  
  (letrec ([ f (lambda (j k ans)
               (if (= ans x )
                   ( + j k)
                   (f (+ j k) j (+ ans 1 ))))])
    ; This is the diffrent way to do fibonacci
  (if (or (= x 1) (= x 2))
      1
      ; call f
      (f 1 1 3))))

; This is a demo how we use the recuiceve function to do fibonacci
(define (fibonacci3 x)
  (letrec ([memo null]
           [f (lambda (x)
                (let ([ans (assoc x memo)])
                (if ans
                    (cdr ans)
                    (let ([new-ans (if (or (= x 1) (= x 2))
                                      1
                                      (+ (f (- x 1))
                                         (f (- x 2))))])
                      (begin
                        (set! memo (cons (cons x new-ans) memo))
                        new-ans)))))])
    (f x)))

