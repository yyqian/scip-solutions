(define (p r c)
  (if (or (= c 1) (= c r))
      1
      (+ (p (- r 1) (- c 1))
         (p (- r 1) c))))
(p 5 3)