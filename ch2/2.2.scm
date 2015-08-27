(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (make-point x y)
  (cons x y))
(define (x-point p)
  (car p))
(define (y-point p)
  (cdr p))

(define (make-segment start-p end-p)
  (cons start-p end-p))
(define (start-segment segment)
  (car segment))
(define (end-segment segment)
  (cdr segment))

(define (average a b)
  (/ (+ a b) 2))

(define (midpoint-segment segment)
  (make-point
    (average (x-point (start-segment segment)) (x-point (end-segment segment)))
    (average (y-point (start-segment segment)) (y-point (end-segment segment)))))

(print-point (midpoint-segment (make-segment (make-point 1 2) (make-point 3 4))))