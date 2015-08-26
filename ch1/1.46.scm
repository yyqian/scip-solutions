(define (close-enough? v1 v2)
  (define tolerance 0.00001)
  (< (abs (- v1 v2)) tolerance))

(define (iterative-improve close-enough? improve)
  (lambda (x)
    (let ((next-x (improve x)))
      (if (close-enough? x next-x)
          next-x
          ((iterative-improve close-enough? improve) next-x)))))

(define (sqrt x)
  (define (improve guess)
    (/ (+ guess (/ x guess)) 2))
  ((iterative-improve close-enough? improve) 1.0))

(define (fixed-point f first-guess)
  ((iterative-improve close-enough? f) first-guess))

(define (sqrt1 x)
  (fixed-point (lambda (y) (/ (+ y (/ x y)) 2)) 1.0))

(newline)
(display (sqrt 2))
(newline)
(display (sqrt1 2))