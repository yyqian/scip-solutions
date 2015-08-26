(define (fixed-point f guess)
  (let ((next-guess (f guess))
        (tolerance 0.000001))
    (if (< (abs (- guess next-guess)) tolerance)
        next-guess
        (fixed-point f next-guess))))

(define (newtons-method g guess)
  (fixed-point (newtons-transform g) guess))

(define (newtons-transform g)
  (lambda (x)
    (- x (/ (g x) ((D g) x)))))

(define (D g)
  (let ((dx 0.00001))
    (lambda (x)
      (/ (- (g (+ x dx)) (g x)) dx))))

(define (cubic a b c)
  (lambda (x) (+ (* x x x)
                 (* a x x)
                 (* b x)
                 c)))

(newtons-method (cubic 1 2 3) 1)