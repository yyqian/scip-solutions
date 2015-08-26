(define (even? x)
  (= (remainder x 2) 0))
(define (pow x n)
  (define (iter x n a)
    (cond ((<= n 0) a)
          ((even? n) (iter (* x x) (/ n 2) a))
          (else (iter x (- n 1) (* a x)))))
  (iter x n 1))

(define (fixed-point f guess)
  (let ((next-guess (f guess))
        (tolerance 0.000001))
    (if (< (abs (- guess next-guess)) tolerance)
        next-guess
        (fixed-point f next-guess))))

(define (compose f g)
  (lambda (x)
    (f (g x))))
(define (repeated f n)
  (define (iter i g)
    (if (>= i n)
        g
        (iter (+ i 1) (compose f g))))
  (iter 1 f))

(define (average-damp f)
  (lambda (x)
    (/ (+ x (f x)) 2)))

(define (nth-root x n damp-times)
  (fixed-point (factory x n damp-times) 1.0))

(define (factory x n damp-times)
  ((repeated average-damp damp-times)
   (lambda (y) (/ x (pow y (- n 1))))))

(nth-root 100000 5 3)