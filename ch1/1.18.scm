;a    * b     + y -> const
;a    * (b-1) + (y+a)
;(2a) * (b/2) + y
(define (fast-mult a b)
  (define (double x) (* x 2))
  (define (halve x) (/ x 2))
  (define (even? x) (= (remainder x 2) 0))
  (define (mult-iter a b y)
    (cond ((= b 0) y)
          ((even? b) (mult-iter (double a) (halve b) y))
          (else (mult-iter a (- b 1) (+ y a)))))
  (trace-entry mult-iter)
  (mult-iter a b 0))