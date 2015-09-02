(define (tree-map proc tree)
  (map (lambda (sub-tree)
        (if (pair? sub-tree)
            (tree-map proc sub-tree)
            (proc sub-tree)))
       tree))

(tree-map square (list 1 (list 2 4) 3))