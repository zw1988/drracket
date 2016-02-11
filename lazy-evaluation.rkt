#lang racket

; A zero-argument function used to delay evaluation is called a thunk
(define (make-thunk expr) (mcons #f expr))
(define (eval thunk)
  (if (mcar thunk) (mcdr thunk)
      (begin (set-mcar! thunk #t) (set-mcdr! thunk ((mcdr thunk))) (mcdr thunk))))

(define (sum n)
  (if (= n 0) 0
      (+ n (sum (- n 1))))); 1+2+3+..+n

;thunk only evaluated once
(define (test-lazy-eval n thunk)
  (if (= n 0) 0
     (+ (thunk) (test-lazy-eval (- n 1) thunk))))

;(sum 10000) only evaluated once
(test-lazy-eval 1000000 (let ([thunk (make-thunk (lambda () (sum 10000)))]) (lambda () (eval thunk))))

;make stream
(define (make-stream fn init)
  (define (f x) (cons x (lambda () (f (fn x)))))
  (lambda () (f init)))

(define powers2 (make-stream (lambda (x) (* x 2)) 1))
(define nats (make-stream (lambda (x) (+ x 1)) 1))
(define fibs (make-stream (lambda (x) (cons (cdr x) (+ (car x) (cdr x))))  (cons 1 1)))

;output
;50005000000000
;> (powers2)
;'(1 . #<procedure>)
;> ((cdr (powers2)))
;'(2 . #<procedure>)
;> ((cdr ((cdr (powers2)))))
;'(4 . #<procedure>)
;> (nats) ((cdr (nats))) ((cdr ((cdr (nats))))) 
;'(1 . #<procedure>)
;'(2 . #<procedure>)
;'(3 . #<procedure>)
 