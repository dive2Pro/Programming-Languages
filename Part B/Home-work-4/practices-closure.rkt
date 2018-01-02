#lang racket
(struct const (int) #:transparent #:mutable)
(struct add (e1 e2) #:transparent )
(struct multiple (e1 e2) #:transparent )

(define (exp-eval e) (
                      cond [
                             (const? e) e]
                            [( add? e) (let(
                                            [v1 (const-int (exp-eval (add-e1 e)))]
                                            [v2 (const-int (exp-eval (add-e2 e)))]
                                            )
                                         (const ( + v1 v2))
)]
                            [#t (error "eval wrong thins")]



                      )
  )
(const-int (exp-eval (add (const 2) (const 3))))

(set-const-int! (exp-eval (add (const 2) (const 3))) 1)

(const-int (exp-eval (add (const 2) (const 3))))

