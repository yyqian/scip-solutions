(define (tan-cf-rec n d k)
  (define (rec i)
    (if (= i k)
        (/ (n i) (d i))
        (/ (n i) (+ (d i) (rec (+ i 1))))))
  (rec 1))

(define (tan-cf-iter n d k)
  (define (iter i result)
    (if (= i 0)
        result
        (iter (- i 1) (/ (n i) (+ (d i) result)))))
  (iter k 0))

(define (tan x)
  (define (d i) (- (* 2 i) 1))
  (define (n i)
    (if (= i 1)
        x
        (- (* x x))))
  (newline)
  (display (tan-cf-rec n d 100))
  (newline)
  (display (tan-cf-iter n d 100)))

(tan 1.0)