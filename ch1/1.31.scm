
(define (next i)
  (+ i 1))
(define (term i)
  (if (= (remainder i 2) 0)
      (/ (+ i 2.0)
         (+ i 1.0))
      (/ (+ i 1.0)
         (+ i 2.0))))

(define (product-iter term a next b)
  (define (iter cur pro)
    (if (> cur b)
        pro
        (iter (next cur) (* pro (term cur)))))
  (iter a 1))

(define (product-rec term a next b)
  (if (> a b)
      1
      (* (term a) (product-rec term (next a) next b))))

(define (factorial-term i) i)

(newline)
(display (product-iter term 1 next 10000))
(newline)
(display (product-rec term 1 next 10000))
(newline)
(display (product-iter factorial-term 1 next 5))
