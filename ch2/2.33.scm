(define nil '())
(define (accumlate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumlate op initial (cdr sequence)))))

(define (map p sequence)
  (accumlate (lambda (x y) (cons (p x) y))
             nil
             sequence))

(define (append seq1 seq2)
  (accumlate cons seq2 seq1))

(define (length sequence)
  (accumlate (lambda (x y) (+ 1 y)) 0 sequence))