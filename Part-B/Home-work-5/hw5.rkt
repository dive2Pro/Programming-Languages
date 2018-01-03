;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs but /is/ a MUPL value;
;; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1

;; CHANGE (put your solutions here)
;; 1.1
(define (racketlist->mupllist ls)
  (if (null? ls)
      (aunit)
      (apair (car ls) (racketlist->mupllist (cdr ls)))
      )
  )
(define _mr (racketlist->mupllist (list (int 3) (int 4))) )
;; 1.2
(define (mupllist->racketlist ls)
  (if (apair? ls)
      (let ([v1 (apair-e1 ls)]
            [v2 (apair-e2 ls)])
        (list* v1 (mupllist->racketlist v2))
        )
      null
      )
  )
 (mupllist->racketlist _mr)
;; Problem 2

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str)
         (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

(define (extendenv env ext)
  ;(if  (cons? ext)
       (if (null? ext)
           env
           ;; (define x (list (list 2 3) (list "q" 22)))
           ;; (append x (list (list "q" 123) (list "w" 222))
           (if (list? ext)
               (append env ext)
               (begin
            ; (print (append env (cons ext null)))
            ; (print "-------------")
             
                 (append env (list ext))))
           )
      ; (error "extendenv applied to non-pair"))
  )
;; Do NOT change the two cases given to you.  
;; Do add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond
    ;; 如是这样, 如果在空 env 下去 var 定义一个 variable 就会 error
    ;; 
    [(var? e) 
         (envlookup env (var-string e))]
    [(add? e) 
     (let ([v1 (eval-under-env (add-e1 e) env)]
           [v2 (eval-under-env (add-e2 e) env)])
       (if (and (int? v1)
                (int? v2))
          
             (int (+ (int-num v1) 
                     (int-num v2)))
           
             
             (error "MUPL addition applied to non-number")))]

    ;; CHANGE add more cases here
    [(int? e)
     (let ([v (int-num e)])
       (if (number? v)
           e
           (car (eval-under-env v env))
           ))
     ]
    
    [(ifgreater? e)
     (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
           [v2 (eval-under-env (ifgreater-e2 e) env)])
       (if (and (int? v1)
                (int? v2))
           (if (> (int-num v1) (int-num v2))
               (eval-under-env (ifgreater-e3 e) env)
               (eval-under-env (ifgreater-e4 e) env)
               )
           (error "MUPL ifgreater first two arguments are applied to non-number")))]
    ;; if s is a Racket string and e1 and e2 are MUPR expressions
    ;; then (mlet s e1 e2) is a MUPL expression
    ;; a let expression where the value resulting e1 is bound to s in the evaluation of e2
    [(mlet? e)
     (let ([v1 (mlet-var e) ]
           ;; e1 resulting is under env
           [v2 (eval-under-env (mlet-e e) env)])
       ;; body should under the env that extends by v1 : v2
       (if (string? v1)
          ; (begin
           ;  (print v1)
            ; (print (cons v1 v2))
             (eval-under-env (mlet-body e) (extendenv env (cons v1 v2)))
           (error "MUPL mlet first argument applied to non-string")))]
    ;; struct fun  (nameopt formal body)
    ;; If nameopt and formal are Racket strings and body is a mupl expression,
    ;; then (fun s1 s2 e) is a mupl expression (a function).
    
    ;; In body, nameopt is bound to the function itself (for recursion)
    ;; -> 这句话就是说 将 (const nameopt body) extended to env
    ;; and formal is bound to the (one) argument.
    ;; Also, (fun #f s2 e) is allowed for anonymous nonrecursive functions.
    
    ;; Functions are lexically scoped:
    ;; A function evaluates to a closure holding the function and the current environment.
    [(fun? e)
     (let ([s1 (fun-nameopt e)]
           [s2 (fun-formal e)]
           [body (fun-body e)])
       (if (or (string? s1) (equal? s1 #f))
           (if (string? s2)
               (closure (if s1
                            (extendenv env (cons s1 body))
                            env)
                        body)
               (error "MUPL fun's second argument applied to non-string"))
           (error "MUPL fun's first argument applied neither string or #f")))]
           
    
    ;; struct call (funexp actual) 
    ;; if e1 and e2 are MUPL expressions, then (call e1 e2) is a MUPL expression (a function call)

    [(call? e)
     (let ([c (call-funexp e)]
           [p (eval-under-env (call-actual e) env)])
       
       (if (closure? c)
            ;; it evaluates the closure's function's body in the closure's environment
            ;; extended to map the function's name to the closure (unless the name field is #f)
            ;; the function's argument-name (i.e., the parameter name)
            ;; to the result of the second subexpression
            ;;  (struct closure (env fun) #:transparent)
            
           (let* ([c-env (closure-env c)]
                 [c-body (closure-fun c)]
                 [c-body-param (fun-formal c-body)])
             
             (begin
               ;(print c-env)
               ;(print c-body)
               ;(print c-body-param)
               ;  (print (eval-under-env c-body
               ;   (extendenv env (extendenv c-env (cons c-body-param p)))))
               (eval-under-env
                ;; closure produces a function that result from call fun
                (eval-under-env c-body
                                      
                                (extendenv env (extendenv c-env (cons c-body-param p))))
                env)))
          ;; if the first is not a closure -> error
           (error "MUPL call first argument applied to non-closure")))]
    ;; A call evaluates its first and second subexpressions to values.
    
    ;;  closure (env fun)
    [(closure? e)
     (let ([ce (closure-env e)]
           [fn (closure-fun e)])
       (eval-under-env fn (extendenv env ce)))]
  
   


        
    ;; (struct apair (e1 e2)     #:transparent) ;; make a new pair
    [(apair? e)
     (let
         ([v1 (eval-under-env (apair-e1 e) env)]
          [v2 (eval-under-env (apair-e2 e) env)])
       (cons v1 v2))]
        
         
    ;; struct fst  (e)
    [(fst? e)
     (let ([v (eval-under-env (fst-e e) env)])
       (if (pair? v)
           (car v)
           (error "MUPL fst argument e resulting to non-pair")))]
    [(snd? e)
     (let ([v (eval-under-env (snd-e e) env)])
       (if (pair? v)
           (cdr v)
           (error "MUPL snd argument e resulting to non-pair")))]
    [(isaunit? e)
     (let ([v (eval-under-env (isaunit-e e) env)])
       (if (aunit? v)
           (int 1)
           (int 0)))]
    [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
;(eval-exp (fun "zz" "x" (aunit)))

;; Problem 3

(define (ifaunit e1 e2 e3) "CHANGE")

(define (mlet* lstlst e2) "CHANGE")

(define (ifeq e1 e2 e3 e4) "CHANGE")

;; Problem 4

(define mupl-map "CHANGE")

(define mupl-mapAddN 
  (mlet "map" mupl-map
        "CHANGE (notice map is now in MUPL scope)"))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e) "CHANGE")

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
