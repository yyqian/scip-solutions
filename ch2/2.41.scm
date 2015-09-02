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

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (unique-pairs n)
  (flatmap (lambda (x) (map (lambda (y) (list x y)) (enumerate-interval 1 (- x 1))))
       (enumerate-interval 1 n)))

(define (unique-triples n)
  (flatmap (lambda (i) (map (lambda (pair) (cons i pair)) (unique-pairs (- i 1))))
           (enumerate-interval 1 n)))

(define (magic-3 n)
  (let ((sum-eq-n? (lambda (pair) (= n (+ (car pair) (cadr pair) (caddr pair))))))
    (filter sum-eq-n? (unique-triples n))))

(magic-3 14)
