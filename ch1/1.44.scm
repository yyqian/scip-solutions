(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (repeated-iter f n)
  (define (iter i g)
    (if (>= i n)
        g
        (iter (+ i 1) (compose f g))))
  (iter 1 f))

(define (smooth f)
  (let ((dx 0.00001))
    (lambda (x)
      (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3))))
(define (smooth-n f n)
  ((repeated-iter smooth n) f))

(newline)
(display ((smooth square) 5))
(newline)
(display ((smooth-n square 10) 5))