(define tolerance 0.00001)
(define (fixed-point f guess)
  (let (
        (next-guess (f guess))
       )
    (if (< (abs (- guess next-guess)) tolerance)
        next-guess
        (fixed-point f next-guess))
  )
)

(fixed-point (lambda (x) (+ 1 (/ 1.0 x))) 1.0)