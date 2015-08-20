#lang scheme
(define (sqrt-iter0 guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter0 (improve guess x)
                 x)))
(define (good-enough? guess x)
  (< (abs (- (square guess)
             x))
     0.0001))
(define (square x) (* x x))
(define (improve guess x)
  (average guess (/ x guess)))
(define (average x y) (/ (+ x y) 2.0))
(define (sqrt0 x)
  (sqrt-iter0 initial-guess x))
(define initial-guess 1.0)
;test sqrt0
(sqrt0 5)

;define the new-if
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))
;test the new-if
(new-if (= 2 3) 0 5)
(new-if (= 1 1) 0 5)
;define the new sqrt1
(define (sqrt-iter1 guess x)
  (new-if (good-enough? guess x)
      guess
      (sqrt-iter1 (improve guess x)
                 x)))
(define (sqrt1 x)
  (sqrt-iter1 initial-guess x))
;the program will terminate here
;(sqrt1 5)
;这儿出错的根本原因是if和new-if存在一定区别
;Lisp采用的是应用序求值，new-if是一个普通函数，因此在调用new-if时，then-clause和else-clause都会先进行求值
;而if不是普通的函数，是特殊的，if只有在predicate作出判断之后再进行求值

;可以用下面的语句进行试验
(if #t
    (display "good")
    (display "bad"))
;display good
(new-if #t
        (display "new-good")
        (display "new-bad"))
;display new-goodnew-bad