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
; was getting stack overflow on recursive functions so added iterative aproaches to mitigate them
(defun fs-recursive (node b &optional (queue nil) (visited nil))
  (cond
    ((>= 0 (node-score node)) (progn (print-node node) visited))
    ((null node) visited)
    ((and (null queue) (null visited)) (fs node b (list node) '()))
    ((car queue)
      (if b
        (fs (car queue) b (append (cdr queue) (generate-children node)) (cons node visited))
        (fs (car queue) b (append (generate-children node) (cdr queue)) (cons node visited))
      )
    )
    (t visited)
  )
)

(defun fs (node b)
  (let (
      (queue (list node))
      (visited '())
    )
    (loop while queue do
      (let ((current-node (car queue)))
        (if (>= 0 (node-score current-node))
          (prog2
            (print-node current-node)
            (break "^ This is a solution node, read from bottom up.^~%~%Type 'continue' to find another solution or 'abort' to stop looking.")
          )
        (progn
          (setq visited (cons current-node visited))
          (if b
            (setq queue (append (cdr queue) (generate-children current-node)))
            (setq queue (append (generate-children current-node) (cdr queue)))
          )
        )
        )
      )
    )
  )
)

(defun bfs (node)
  (fs node T)
)

(defun dfs (node)
  (fs node NIL)
)

(defun a-star (node ))
