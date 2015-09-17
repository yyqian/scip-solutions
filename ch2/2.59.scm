; 利用adjoin-set在join的过程中会进行判断元素是否已存在，只需要递归地将set1所有的元素adjoin到set2中

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (union-set set1 set2)
  (if (null? set1)
      set2
      (adjoin-set (car set1) (union-set (cdr set1) set2))))

(union-set (list 'a 'b 'c 'd) '(c d e f))