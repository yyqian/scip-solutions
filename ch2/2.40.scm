(define nil '())
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
(define (enumerate-interval i j)
  (if (> i j)
      nil
      (cons i (enumerate-interval (+ i 1) j))))

(define (unique-pairs n)
  (accumulate append nil (map (lambda (x) (map (lambda (y) (list x y)) (enumerate-interval 1 (- x 1))))
       (enumerate-interval 1 n))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime? x)
 (define (test divisor)
   (cond ((> (* divisor divisor) x) true)
         ((= 0 (remainder x divisor)) false)
         (else (test (+ divisor 1)))))
 (test 2))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum (filter prime-sum? (unique-pairs n))))

(prime-sum-pairs 6)