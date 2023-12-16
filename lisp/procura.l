;;Deve conter a implementação de:
    ;;1. Algoritmo de Procura de Largura Primeiro (BFS)
    ;;2. Algoritmo de Procura do Profundidade Primeiro (DFS)
    ;;3. Algoritmo de Procura do Melhor Primeiro (A*)
    ;;4. Os algoritmos SMA*, IDA* e/ou RBFS (caso optem por implementar o bónus)

(defun is-node (node)
  (cond
    ((= 6 (length node)) 
      (cond 
        ((and 
          (numberp (node-score node)) 
          (numberp (node-final-score node)) 
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

(defun create-node (board score final-score parent &optional (d 0) (h 0))
  (list board score final-score parent d h)
)

(defun node-board (node)
  (nth 0 node)
)

(defun node-score (node)
  (nth 1 node)
)

(defun node-final-score (node)
  (nth 2 node)
)

(defun node-parent (node)
  (nth 3 node)
)

(defun node-depth (node)
  (nth 4 node)
)

(defun node-heuristic (node)
  (nth 5 node)
)

(defun node-from-problem (&optional (problem-number 0))
  (let ((in (get-problem problem-number)))
    (create-node (caddr in) 0 (cadr in) nil)
  )
)

(defun print-node (node &optional (stream-to-write-to t))
  (progn
    (if (node-parent node) (print-node (node-parent node) stream-to-write-to) nil)
    (format stream-to-write-to "~%~%Score: ~a/~a | Depth: ~a | Heuristic: ~a~%~%" (node-score node) (node-final-score node) (node-depth node) (node-heuristic node))
    (print-board (node-board node) stream-to-write-to)
  )
)

(defun solution-node (node size-of-queue size-of-visited)
  (list (node-to-movements node) size-of-queue size-of-visited node)
)

(defun node-to-movements (node)
  (cond
    ((not node) '())
    ((node-parent node) 
      (let* (
          (current (current-position (node-board node)))
          (letter (nth (cadr current) '(A B C D E F G H I J)))
          (movement (list letter (1+ (car current))))
        )
        (append (node-to-movements (node-parent node)) (list movement))
      )
    )
    (t nil)
  )
)

(defun calculate-score (current-score previous-board new-board)
  (let* (
      (pos (current-position new-board))
      (value (cell previous-board (car pos) (cadr pos)))
    )
    (+ current-score value)
  )
)

(defun list-to-nodes (parent children)
  (mapcar 
    (lambda (board) 
      (create-node 
        board 
        (calculate-score (node-score parent) (node-board parent) board) 
        (node-final-score parent) 
        parent 
        (1+ (node-depth parent))
      )
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

(defun is-solution (node)
  (<= (node-final-score node) (node-score node))
)

; ------------------------------------------------- Calculations

(defun apply-heuristic (node heuristic-value)
  (create-node  ; this wasn't my favorite implementation but the only one i could find at this point :c
    (node-board node)
    (node-score node)
    (node-final-score node)
    (node-parent node)
    (node-depth node)
    heuristic-value
  )
)

(defun calculate-heuristic-default (node)
  (cond
    ((= 0 (total-available-houses (node-board node))) node)
    ((= 0 (/ (float (total-points (node-board node))) (total-available-houses (node-board node)))) node)
    (t 
      (apply-heuristic
        node
        (/
          (- (node-final-score node) (node-score node))
          (/ 
            (float (total-points (node-board node))) 
            (total-available-houses (node-board node))
          )
        )
      )
    )
  )
)

;todo nossa heuristica

; ------------------------------------------------- Algorythms
; was getting stack overflow on recursive functions so added iterative aproach to mitigate them
(defun fs-recursive (node b &optional (queue nil) (visited nil))
  (cond
    ((is-solution node) (prog2 (print-node node) visited))
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

(defun fs (node b &optional (maximum-depth most-positive-fixnum))
  (let (
      (queue (list node))
      (visited '())
    )
    (loop while queue do
      (let* ((current-node (car queue)))
        (if (is-solution current-node)
          (return (solution-node current-node (length queue) (length visited)))
          (progn
            (setq visited (cons current-node visited))
            (if b
              (setq queue (append (cdr queue) (generate-children current-node)))
              (if (> (1+ (node-depth current-node)) maximum-depth)
                (return (solution-node nil (length queue) (length visited)))
                (setq queue (append (generate-children current-node) (cdr queue)))
              )
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

(defun dfs (node &optional (max-depth most-positive-fixnum))
  (fs node NIL max-depth)
)

;------------------------ a-star
(defun order-queue (queue)
  (cond
    ((null queue) nil)
    (t 
      (append 
        (order-queue (remove-if (lambda (node) (> (node-heuristic node) (node-heuristic (car queue)))) (cdr queue)))
        (list (car queue))
        (order-queue (remove-if (lambda (node) (<= (node-heuristic node) (node-heuristic (car queue)))) (cdr queue)))
      )
    )
  )
)

(defun a-star (node &optional (heuristic-function 'calculate-heuristic-default))
  (let ((queue (list node))
      (visited '()))
    (loop while queue do
      (let ((current (pop queue)))
        (if (is-solution current) 
          (return (solution-node current (length queue) (length visited)))
          (progn
            (do* ((children (generate-children current) (cdr children))) ((null children)) 
              (let* (
                  (child (funcall heuristic-function (car children)))
                  (found-in-queue (find child queue :key #'node-board))
                  (found-in-visited (find child visited :key #'node-board))
                )
                (cond
                  (found-in-queue 
                    (if (> (node-heuristic found-in-queue) (node-heuristic child)) 
                      (setq queue (order-queue (cons child (remove found-in-queue queue))))
                    )
                  )
                  (found-in-visited 
                    (if (> (node-heuristic found-in-visited) (node-heuristic child)) 
                      (setq visited (remove found-in-visited visited) queue (order-queue (cons child queue)))
                    )
                  )
                  (t (setq queue (order-queue (cons child queue))))
                )
              )
            )
          )
        )
        (push current visited)
      )
    )
  )
)

;; ------------------------------------------ GONCALO