
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

; Write a function sequence that takes 3 arguments low, hight , and stride ,
; all assumed to be numbers.
; Further assume stride is positive. sequence produces a list of numbers from low to high
; (including low and possibly hight ) separated by stride and in sorted order.
; Sample solution: 4 lines;

(define (sequence low high stride )
  (cond [(> low high) null]
        [#t (cons low (sequence (+ low stride ) high stride))]))

; Write a function string-append-map that takes a list of strings xs and a string suffix
; and returns a list of strings.
; Each element of the output should be the corresponding element of the input aapended
; with suffix (with no extra space betwwen the element and suffix).
; You mus use Racket-library functions map and string-append .
; Sample solution: 2 lines;

(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

(string-append-map (list "q" "w" "c") "-")


; Wite a function list-nth-mode that takes a list xs and a number n.
; if the number is negative, terminate the computation with (error "list-nth-mod :
;                       negative number")
; else if the list is empty, terminate the computation with (error "list-nth-mode:
;                       empth list")
; else return the i^th element of the list where we count from zero
; and i is the remainder produced when dividing n by the list's length
; Library functions length, remainder,car, and list-tail are all useful- see the
; Racket documentation.
; Sample solution : 6 lines

(define (list-nth-mode xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mode: empty list")]
        [#t (let ([i (remainder n (length xs))])
              (car (list-tail xs i)))]
        ))
(list-nth-mode (list "w" "e" "r" "t" ) 9)

; Write a function stream-for-n-steps that takes a stream s and a number n
; It returns a list holding the first n values produced by s in order.
; Assume n is non-negative.
; Sample solution: 5 lines;
(define (stream-for-n-steps s n )
  (cond [(= n 0) null]
        [#t (let ([i (s)])
              (cons (car i) (stream-for-n-steps (cdr i) ( - n 1))))]))

(define ones (lambda () (cons 1 ones)))

(stream-for-n-steps ones 4)

; Wtite a stream funny-number-stream that is like the stream of nature numbers
; expect numbers divible by 5 are negated
;Remember a stream is a thunk that when called produces a pair.
; Here the car of the pair will be anumber and the cdr will be another steram

(define funny-number-stream  (let ([i 0])
                               (letrec ([f (lambda ()
                                             (begin
                                               (set! i (+ i 1)))
                                             (if ( = (remainder i 5) 0)
                                                 (cons (* -1 i) f)           
                                               (cons i f)
                                               ))])
                                 f)))
                              
(stream-for-n-steps funny-number-stream 15)

; 6. Wtite a stream dan-then-dog , where the elements of the stream alternate between
; the strings "dan.jpg" and "dog.jpg"
; More specifically, dan-then-dog should be a thunk that when called produces a pair of
; "dan.jpg" and a thunk that when called produces a pair of "dog.jpg"
; and a thunk that when called ...
; simple solutions: 4 lines
(define dan-then-dog (lambda () (
                                 letrec ([ f (lambda (xs)
                                               (cons (car xs)
                                                     (lambda () (f (list (second xs) (car xs))))))])
                                  (f (list "dan.jpg" "dog.jpg")))))
(dan-then-dog)

(stream-for-n-steps dan-then-dog 2)
(stream-for-n-steps dan-then-dog 3)
;7 Write a function stream-add-zero that takes a stream s and returns another stream
;If s would produce v for its i^th element, then (stream-add-zero s ) would produce the
;pair ( 0 . v) for its i ^ th element;
; Simple solutions: 4 lines;
(define (stream-add-zero s) (
                             letrec ([f (lambda (r)
                                       (let ([re (r)])
                                         
                                          (cons (cons 0 (car re))
                                          (lambda () (f (cdr re)))
                                          )
                                       ))])
                                  (f s)

                             ))

(stream-add-zero dan-then-dog)

(stream-for-n-steps (lambda () (stream-add-zero dan-then-dog)) 2)

; 8 , Write a function cycle-lists that takes two lists xs and ys and return a stream
; The lists may or may not be the same length, but assume they are both non-empty
; The elements produced by the stream are pairs where the first part is from xs
; and the second part is from ys.
; The stream cycles forever through the lists.

(define (cycle-lists xs ys )
   (letrec ([f (lambda (i)
                 (cons (cons (list-nth-mode xs i) (list-nth-mode ys i))
                       (lambda () (f ( + i 1)))
                       ))
               ]
            )
     (f 0)))

(define cycle-lists-stream (lambda () (cycle-lists (list 1 2 3) (list "a" "b"))))

(stream-for-n-steps cycle-lists-stream 5)

; Write a function vector-assoc that takes a value v and a vector vec.
; It should behave like Racket's assoc library function except
; (1) it process a vector (Racket's name for an array) instead of a list.
; (2) it allows vector elements not to be pairs in which case it skips them
; (3) it always takes exactly two arguments.
; Process the vector elements in order starting from 0
; Return #f if no vector element is a pair with a car field equal to v
; else return the first pair with an equal car field
; Simple solution is 9 lines

(define (vector-assoc v vec)
  (letrec ([l (vector-length vec)]
           [f (lambda (i)
                (if (= l i)
                    #f
                    (let ([p (vector-ref vec i)])
                      (if (and (pair? p) (equal? (car p) v))
                          p
                          ; continue
                          (f (+ i 1))
                          ))))])
    (f 0)))
(vector-assoc 3 (vector 2 3 4 3 ))

(vector-assoc 3 (vector (cons 2 3) (cons 3  4 ) (cons 3 4)))

(vector-assoc 3 (vector (cons 2 3) (cons 4  3 ) (cons 5 11)))

; Write a function cached-assoc that takes a list xs and a number n and returns a function
; that takes one arugment v and returns the same thing that (assoc v xs) would return.
; However , you should use an n-element cache of recent results to possibly make this
; function faster than just calling assoc (if xs is long and a few elements are returned
; often ). The cache must be a Racket vector of length n that is created by the call to
; cached-assoc ( use Racket library function vector oir make-vector) and used-and-possibly
; -mutated each time the function returned by cached-assoc is called.
; Assume n is positive
; The cache starts empty (all elements #f) When the function returned by cached-assoc
; is called, it first checks the cache for the answer.
; If it is not there, it uses assoc and sx to get the answer and if the result is not #f
; it adds the pair to the cache before returning
; The cache slots are used in a round-robin fasion: the first time a pair is added to the
; cache it is put in position 0, the next pair is put in position 1 , etc .
; up to position n - 1 and then back to position 0 , then position 1;

(define ( cached-assoc xs n)
  (let ([cached (make-vector n (cons #f #f))]
        [cached-position -1])
    (lambda (v) (
                 let ([found (vector-assoc v cached)])
                  (if found
                      found
                      ; not found
                      ; finding in the xs
                      (let ([found-xs (assoc v xs)])
                        (if found-xs
                            (begin
                              (print cached)
                              (set! cached-position (remainder (+ cached-position 1) n))
                              (vector-set! cached cached-position (cons v found-xs))
                              (print cached-position)
                              found-xs)
                            (begin (print cached) #f))))))))
(define find-cached-v (cached-assoc (list (cons 1 2) (cons 3 4) (cons 4 5) (cons 5 6)) 2))
;(find-cached-v 3)
;(find-cached-v 4)
;(find-cached-v 5)
;(find-cached-v 4)
;(find-cached-v 1)
;(find-cached-v 3)

;Define a macro that is used like (while-less e1 do e2 )
; Where e1 and e2 are expressions and while-less and do are syntax.
; The marco should do the following:
; It evaluates e1 exactly once
; It evaluates e2 at least once
; It keeps evaluating e2 until and only until the result is not a number less than the
;   result of the evaluation of e1
; Assuming evaluation terminates, the result is #t
; Assume e1 and e2 produce numbers; your marco can do anything or fail mysteriously otherwise
; 

(define-syntax while-less
  (syntax-rules (do)
    ([ while-less e1 do e2]
     (letrec ([v e1]
           [f (lambda (r)
                (if (> r v)
                    #t
                    (let ([r2 e2])
                      (f ( + r2 1)))
                    )
                )])
       (f e2)))))
(define a 2)
(while-less 7 do (begin (set! a (+ a 1)) (print "x") a))
(while-less 7 do (begin (set! a (+ a 1)) (print "x") a))