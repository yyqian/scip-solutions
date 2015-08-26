(define tolerance 0.000001)
(define (fixed-point f guess)
  (let ((next-guess (f guess)))
    (newline)
    (display guess)
    (if (< (abs (- guess next-guess)) tolerance)
        next-guess
        (fixed-point f next-guess))))

(fixed-point (lambda (x) (/ (log 1000) (log x))) 10.0)

(define (f x) (/ (log 1000) (log x)))
(fixed-point (lambda (x) (/ (+ x (f x)) 2)) 10.0)