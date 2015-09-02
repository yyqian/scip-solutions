(define (accumlate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumlate op initial (cdr sequence)))))

(define (count-leaves t)
  (accumlate +
             0
             (map (lambda (node)
                    (if (pair? node)
                        (count-leaves node)
                        1))
                  t)))

(count-leaves (list 1 2 3 (list 4 5)))