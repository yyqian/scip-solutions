(define (union-tree tree1 tree2)
  (union-set (tree->list-2 tree1)
             (tree->list-2 tree2)))

(define (intersection-tree tree1 tree2)
  (intersection-set (tree->list-2 tree1)
             (tree->list-2 tree2)))