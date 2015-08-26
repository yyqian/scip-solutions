(define (filtered-accumulate combiner null-value term a next b valid?)
  (if (> a b)
      null-value
      (combiner (if (valid? a)
                    (term a)
                    null-value)
                (filtered-accumulate combiner
                                     null-value
                                     term
                                     (next a)
                                     next
                                     b
                                     valid?))))

(define (smallest-divisor n)
  (define (next-divisor n)
    (if (= n 2)
        3
        (+ n 2)))
  (define (find-divisor n test-divisor)
    (cond ((> (* test-divisor test-divisor) n) n)
          ((= (remainder n test-divisor) 0) test-divisor)
          (else (find-divisor n (next-divisor test-divisor)))))
  (find-divisor n 2))
(define (prime? n)
  (if (= n 1)
      #f
      (= (smallest-divisor n) n)))

(newline)
(display
(filtered-accumulate +
                     0
                     (lambda (x) x)
                     1
                     (lambda (x) (+ x 1))
                     10
                     prime?))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (product-of-relative-primes n)
  (define (filter x)
    (= 1 (gcd n x)))
  (filtered-accumulate *
                       1
                       (lambda (x) x)
                       1
                       (lambda (x) (+ x 1))
                       n
                       filter))
(newline)
(display (product-of-relative-primes 10))