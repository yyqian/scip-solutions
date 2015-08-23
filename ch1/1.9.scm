(define (inc x) (+ x 1))
(define (dec x) (- x 1))
;the first part
(define (add a b)
  (if (= a 0)
      b
      (inc (add (dec a) b))))
(add 4 5)
;expand, recursive
(inc (add 3 5))
(inc (inc (add 2 5)))
(inc (inc (inc (add 1 5))))
(inc (inc (inc (inc (add 0 5)))))
(inc (inc (inc (inc 5))))
(inc (inc (inc 6)))
(inc (inc 7))
(inc 8)
;the second part
(define (add1 a b)
  (if (= a 0)
      b
      (add1 (dec a) (inc b))))
(add1 4 5)
;expand, iterative
(add1 3 6)
(add1 2 7)
(add1 1 8)
(add1 0 9)
9