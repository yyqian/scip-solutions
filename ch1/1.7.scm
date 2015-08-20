#lang scheme
(define (sqrt-iter guess x)
  (if (good-enough? guess (improve guess x))
      guess
      (sqrt-iter (improve guess x)
                 x)))
(define (good-enough? old-guess new-guess)
  (< (abs (/ (- old-guess new-guess)
             old-guess))
     0.001))
(define (square x) (* x x))
(define (improve guess x)
  (average guess (/ x guess)))
(define (average x y) (/ (+ x y) 2.0))
(define initial-guess 1.0)
(define (sqrt x)
  (sqrt-iter initial-guess x))

(sqrt 0.9)