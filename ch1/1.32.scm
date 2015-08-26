(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumulate combiner
                                     null-value
                                     term
                                     (next a)
                                     next
                                     b))))

(define (sum term a next b)
  (accumulate + 0 term a next b))
(define (product term a next b)
  (accumulate * 1 term a next b))

(define (next i) (+ i 1))
(define (term i) i)
(newline)
(display (product term 1 next 5))
(newline)
(display (sum term 1 next 10))