#lang scheme
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))
(A 1 10)
(A 0 (A 1 9))
(A 0 (A 0 (A 1 8)))
;(A 0 (A 0 ... (A 1 1))), it would be 2^10
(A 2 4)
(A 1 (A 2 3))
(A 1 (A 1 (A 2 2)))
(A 1 (A 1 (A 1 (A 2 1))))
(A 1 (A 1 (A 1 2)))
;2^(2^(2^2))
(A 3 3)
(A 2 (A 3 2))
(A 2 (A 2 (A 3 1)))
(A 2 (A 2 2))
;2^2(2^(2^2))
(define (f n) (A 0 n)); f(n) = 2*n
(f 10)
(define (g n) (A 1 n)); g(n) = 2^n
(g 10)
(define (h n) (A 2 n)); h(n) = 2^(2^2(...(2^2))), there would be n number of 2
(h 4)
(define (k n) (* 5 n n)); k(n) = 5(n^2)
(k 4)
;this is a recursive function