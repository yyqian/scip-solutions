;helper function
(define (sum-of-squares x y)
  (+ (* x x)
     (* y y)))
(define (bigger x y)
  (if (> x y)
      x
      y))
(define (smaller x y)
  (if (< x y)
      x
      y))
;solution 0
(define (f0 x y z)
  (if (< x y)
      (if (< x z)
          (sum-of-squares y z)
          (sum-of-squares x y))
      (if (< y z)
          (sum-of-squares x z)
          (sum-of-squares x y))))
;solution 1
(define (f1 x y z)
  (cond ((and (<= x y) (<= x z)) (sum-of-squares y z))
        ((and (<= x y) (>= x z)) (sum-of-squares x y))
        ((and (>= x y) (<= y z)) (sum-of-squares x z))
        ((and (>= x y) (>= y z)) (sum-of-squares x y))))
;solution 2
(define (f2 x y z)
  (if (<= x y)
      (sum-of-squares y (bigger x z))
      (sum-of-squares x (bigger y z))))
;solution 3
(define (f3 x y z)
  (sum-of-squares (bigger x y)
                  (bigger (smaller x y) z)))
;solution 4 (recursive)
(define (f4 x y z)
  (if (and (<= x y) (<= x z))
      (sum-of-squares y z)
      (f4 z x y)))
;testing
(define (test f) (f 87 23 32))
(test f0)
(test f1)
(test f2)
(test f3)
(test f4)