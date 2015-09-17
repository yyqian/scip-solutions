; 这个答案是错误的，虽然可以处理多个参数的情况
; 但这里忽略了加法和乘法的优先法则，是按从左到右的顺序演算的

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        (else
          (error "unknow expression type -- DERIV" exp))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (variable? x)
  (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum m1 . m2)
  (if (= 1 (length m2))
      (let ((m2 (car m2)))
        (cond ((=number? m1 0) m2)
              ((=number? m2 0) m1)
              ((and (number? m1) (number? m2)) (+ m1 m2))
              (else (list m1 '+ m2))))
      (cond ((=number? m1 0) m2)
            (else (cons m1 (cons '+ m2))))))

(define (make-product m1 . m2)
  (if (= 1 (length m2))
      (let ((m2 (car m2)))
        (cond ((or (=number? m1 0) (=number? m2 0)) 0)
              ((=number? m1 1) m2)
              ((=number? m2 1) m1)
              ((and (number? m1) (number? m2)) (* m1 m2))
              (else (list m1 '* m2))))
      (cond ((=number? m1 0) 0)
            ((=number? m1 1) m2)
            (else (cons m1 (cons '* m2))))))

(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+))); patched

(define (addend s)
  (car s)); patched

(define (augend s)
  (if (= 3 (length s))
      (caddr s)
      (cddr s)))

(define (product? x)
  (and (pair? x) (eq? (cadr x) '*))); patched

(define (multiplier p)
  (car p)); patched

(define (multiplicand p)
  (if (= 3 (length p))
      (caddr p)
      (cddr p)))

(deriv '(x + 3 * (x + y + 2)) 'x)