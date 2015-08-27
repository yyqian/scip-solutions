(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

;(add-1 zero)
;((lambda (f) (lambda (x) (f ((zero f) x)))))
;((lambda (f) (lambda (x) (f ((lambda (y) y) x)))))
;((lambda (f) (lambda (x) (f x))))
(define one (lambda (f) (lambda (x) (f x))))

;(add-1 one)
;((lambda (f) (lambda (x) (f ((one f) x)))))
;((lambda (f) (lambda (x) (f ((lambda (y) (f y)) x)))))
;((lambda (f) (lambda (x) (f (f x)))))
(define two (lambda (f) (lambda (x) (f (f x)))))

(define (add a b)
  (lambda (f) (lambda (x) ((a f) ((b f) x)))))

(newline)
(display ((one square) 2))
(newline)
(display ((two square) 2))
(newline)
(display (((add one two) square) 2))
