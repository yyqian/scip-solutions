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


(define (queens0 board-size)
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
  (trace-entry queen-cols)
  (queen-cols board-size))

(define (queens1 board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
          (lambda (positions) (safe? k positions))
          (flatmap
            (lambda (new-row)
              (map (lambda (rest-of-queens)
                           (adjoin-position new-row k rest-of-queens))
                   (queen-cols (- k 1))))
            (enumerate-interval 1 board-size)))))
  (trace-entry queen-cols)
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

(queens1 3)
;You can check the number of times queen-cols is called. After the enumerate-interval is called, the queen-cols will be called board-size times.
;If we set the board-szie as 4, we need to call (queen-cols 3) 4 times, (queen-cols 2) 4*4 times, (queen-cols 1) 4*4*4 times, (queen-cols 0) 4*4*4*4 times
;while the previous method only need to call (queen-cols 3), (queen-cols 2), (queen-cols 1), (queen-cols 0) one time.
;The queen-cols will be called (1 + n + n^2 + ... + n^n) and (1 + n) times in the two methods respectively.