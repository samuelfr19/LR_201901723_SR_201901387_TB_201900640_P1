;;Deve conter a implementação de:
    ;;1. Algoritmo de Procura de Largura Primeiro (BFS)
    ;;2. Algoritmo de Procura do Profundidade Primeiro (DFS)
    ;;3. Algoritmo de Procura do Melhor Primeiro (A*)
    ;;4. Os algoritmos SMA*, IDA* e/ou RBFS (caso optem por implementar o bónus)

(defun is-node (node)
  (cond
    ((= 5 (length node)) 
      (cond 
        ((and 
          (numberp (node-score node)) 
          (numberp (node-depth node))
          (numberp (node-heuristic node))
          (= 10 (length (node-board node)))
        )
        t
        )
        (t nil)
      )
    )
    (t nil)
  )
)

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

(defun print-node (node)
  (progn
    (format t "Score: ~a | Depth: ~a | Heuristic: ~a~%~%" (node-score node) (node-depth node) (node-heuristic node))
    (print-board (node-board node))
    (if (node-parent node) (print-node (node-parent node)) nil)
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
      (create-node board (calculate-score (node-score parent) (node-board parent) board) parent (1+ (node-depth parent)))
    )
    children
  )
)

(defun generate-first-children (node)
  (list-to-nodes node (all-horse-start-positions (node-board node)))
)

(defun generate-children (node)
  (cond
    ((current-position (node-board node)) (list-to-nodes node (all-possible-movements (node-board node)))) ;problema
    (t (generate-first-children node))
  )
)

; ------------------------------------------------- Algorythms

(defun bfs (node &optional (queue nil) (visited nil))
  (cond
    ((>= 0 (node-score node)) (print-node node))
    ((null node) nil)
    ((and (null queue) (null visited)) (bfs node (list node) '()))
    ((car queue) 
      (bfs (car queue) (append (cdr queue) (generate-children node)) (cons node visited))
    )
    (t visited)
  )
)

(defun dfs (node &optional (queue nil) (visited nil))
  (cond
    ((>= 0 (node-score node)) (print-node node))
    ((null node) nil)
    ((and (null queue) (null visited)) (bfs node (list node) '()))
    ((car queue) 
      (bfs (car queue) (append (generate-children node) (cdr queue)) (cons node visited))
    )
    (t visited)
  )
)