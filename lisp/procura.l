;;Deve conter a implementação de:
    ;;1. Algoritmo de Procura de Largura Primeiro (BFS)
    ;;2. Algoritmo de Procura do Profundidade Primeiro (DFS)
    ;;3. Algoritmo de Procura do Melhor Primeiro (A*)
    ;;4. Os algoritmos SMA*, IDA* e/ou RBFS (caso optem por implementar o bónus)

(defun create-node (board score parent &optional (d 0) (h 0))
  (list board score parent d h)
)

(defun node-board (node)
  (first node)
)

(defun node-score (node)
  (second node)
)

(defun node-parent (node)
  (third node)
)

(defun node-depth (node)
  (fourth node)
)

(defun node-heuristic (node)
  (fifth node)
)

(defun test-node (&optional (problem-number 0))
  (let ((in (get-problem problem-number)))
    (create-node (caddr in) (cadr in) nil)
  )
)

(defun calculate-score (current-score previous-board new-board)
  (let* (
      (pos (current-position new-board))
      (value (cell previous-board (car pos) (cadr pos)))
    )
    (- current-score value)
  )
)

(defun list-to-nodes (parent children)
  (mapcar 
    (lambda (board) 
      (create-node board (calculate-score (node-score parent) (node-board parent) board) parent)
    )
    children
  )
)

(defun generate-first-children (node)
  (list-to-nodes node (all-horse-start-positions (node-board node)))
)

(defun generate-children (node)
  (cond
    ((current-position (node-board node)) (list-to-nodes node (all-possible-movements (node-board node))))
    (t (generate-first-children (node)))
  )
)
