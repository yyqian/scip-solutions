(define (square-tree tree)
  (map (lambda (sub-tree)
        (if (pair? sub-tree)
            (square-tree sub-tree)
            (square sub-tree)))
       tree))

(square-tree (list 1 (list 2 4) 3))