(define nil '())
(define (enumerate-interval i j)
  (if (> i j)
      nil
      (cons i (enumerate-interval (+ i 1) j))))
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))


(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
          (lambda (positions) (safe? k positions))
          (flatmap
            (lambda (rest-of-queens)
              (map (lambda (new-row)
                           (adjoin-position new-row k rest-of-queens))
                   (enumerate-interval 1 board-size)))
            (queen-cols (- k 1))))))
  (queen-cols board-size))

(define empty-board nil)
(define (adjoin-position new-row k rest-of-queens)
  (cons new-row rest-of-queens))

(define (get-row location)
  (car location))
(define (get-col location)
  (cdr location))

(define (safe? k positions)
  (let ((row-to-compare (car positions))
        (compare (lambda (loc0 loc1)
                    (cond ((= (get-row loc0) (get-row loc1)) #f)
                          ((= (- (get-row loc0) (get-col loc0)) (- (get-row loc1) (get-col loc1))) #f)
                          ((= (+ (get-row loc0) (get-col loc0)) (+ (get-row loc1) (get-col loc1))) #f)
                          (else #t)))))
    (define (iter-comp result col rest-positions)
      (if (null? rest-positions)
          result
          (iter-comp (and result (compare (cons (car rest-positions) col) (cons row-to-compare k)))
                     (- col 1)
                     (cdr rest-positions))))
    (iter-comp #t (- k 1) (cdr positions))))

;show results
(define (reverse items)
  (if (null? items)
      nil
      (append (reverse (cdr items)) (list (car items)))))
(define (reverse-each-row m)
  (map reverse m))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))
(define (trans m)
  (accumulate-n cons nil m))

(define (rotate-right m)
  (reverse-each-row (trans m)))

(define (make-matrix board size)
  (rotate-right (map (lambda (x)
                        (map (lambda (y) (if (= x y) 1 0))
                             (enumerate-interval 1 size)))
                     board)))
;plot的矩阵
(define (plot-board board size)
  (map (lambda (row)
          (begin (newline)
                 (map (lambda (x) (if (= x 0) (display "o") (display "x"))) row)))
       (make-matrix board size)))

(define (count items)
  (define (iter result rest-items)
    (if (null? rest-items)
        result
        (iter (+ 1 result) (cdr rest-items))))
  (iter 0 items))

(define (show-queens board-size)
  (let ((result (queens board-size)))
    (begin (map (lambda (board) (begin (newline) (plot-board board board-size))) result)
           (newline)
           (display "count: ")
           (display (count result)))))

(show-queens 8)