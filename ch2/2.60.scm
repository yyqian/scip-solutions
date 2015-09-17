; union-set不需要去重的话就是简单的append，
; intersection-set按照之前的算法会有重复的元素，我这儿做法是把这个结果再去重

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (cons x set))

(define (union-set set1 set2)
  (append set1 set2))

(define (intersection-set-dupe set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2) (cons (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(define (remove-dupe set)
  (cond ((null? set) '())
        ((element-of-set? (car set) (cdr set)) (remove-dupe (cdr set)))
        (else (cons (car set) (remove-dupe (cdr set))))))

(define (intersection-set set1 set2)
  (remove-dupe (intersection-set-dupe set1 set2)))

(intersection-set (list 1 2 2 3 4) (list 2 2 5 4))