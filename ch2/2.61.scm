(define (adjoin-set x set)
  (cond ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))

(adjoin-set 7 (list 4 6 8 10))