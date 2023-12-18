;;Deve conter a implementação de:
    ;;1. Algoritmo de Procura de Largura Primeiro (BFS)
    ;;2. Algoritmo de Procura do Profundidade Primeiro (DFS)
    ;;3. Algoritmo de Procura do Melhor Primeiro (A*)
    ;;4. Os algoritmos SMA*, IDA* e/ou RBFS (caso optem por implementar o bónus)

; ------------------------------------------------- Node 
(defun is-node (node)
  (cond
    ((= 7 (length node)) 
      (cond 
        ((and 
          (numberp (node-score node)) 
          (numberp (node-final-score node)) 
          (numberp (node-depth node))
          (numberp (node-heuristic node))
          (numberp (node-f node))
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

(defun create-node (board score final-score parent &optional (d 0) (h 0) (f 0))
"Cria um nó"
  (list board score final-score parent d h f)
)

(defun node-board (node)
"Devolve o tabuleiro do nó"
  (nth 0 node)
)

(defun node-score (node)
"Devolve a pontuação atual do nó"
  (nth 1 node)
)

(defun node-final-score (node)
"Devolve a pontuação objetivo do nó"
  (nth 2 node)
)

(defun node-parent (node)
"Devolve o nó pai do nó"
  (nth 3 node)
)

(defun node-depth (node)
"Devolve a profundidade do nó"
  (nth 4 node)
)

(defun node-heuristic (node)
"Devolve o valor heurístico do nó"
  (nth 5 node)
)

(defun node-f (node)
"Devolve o valor da função de avaliação"
  (nth 6 node)
)

(defun final-node (node)
"Devolve o nó final"
  (car(last node))
)

(defun solution-lenght (node)
"Devolve o a profundidade da solução encontrada"
 (length (car node))
)

(defun generated-nodes (solution)
  (+ (second solution) (third solution))
)

(defun first-node (node)
  (cond
    ((null (node-parent node)) node)
    (t (first-node (node-parent node)))
  )
)

; ------------------------------------------------- Solution Node 

(defun create-solution-node (node size-of-queue size-of-visited)
  (list (node-to-movements node) size-of-queue size-of-visited node)
)

(defun solution-node-path (solution-node)
  (nth 0 solution-node)
)

(defun solution-node-size-queue (solution-node)
  (nth 1 solution-node)
)

(defun solution-node-size-visited (solution-node)
  (nth 2 solution-node)
)

(defun solution-node-node (solution-node)
  (nth 3 solution-node)
)

; ------------------------------------------------- Aux Functions

(defun node-from-problem (&optional (problem-number 0))
  (let ((in (get-problem problem-number)))
      (create-node (caddr in) 0 (cadr in) nil)
  )
)

(defun print-node (node &optional (stream-to-write-to t))
  (progn
    (if (node-parent node) (print-node (node-parent node) stream-to-write-to) nil)
    (format stream-to-write-to "~%Score: ~a/~a | Depth: ~a | Heuristic: ~a | F: ~a~%~%" (node-score node) (node-final-score node) (node-depth node) (node-heuristic node) (node-f node))
    (print-board (node-board node) stream-to-write-to)
  )
)

(defun print-node-individual (node &optional (stream-to-write-to t))
  (progn
    (format stream-to-write-to "~%Score: ~a/~a | Depth: ~a | Heuristic: ~a | F: ~a~%~%" (node-score node) (node-final-score node) (node-depth node) (node-heuristic node) (node-f node))
    (print-board (node-board node) stream-to-write-to)
  )
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
    ((current-position (node-board node)) (list-to-nodes node (all-possible-movements (node-board node))))
    (t (generate-first-children node))
  )
)

; ------------------------------------------------- Calculations

(defun is-solution (node)
  (<= (node-final-score node) (node-score node))
)

(defun calculate-score (current-score previous-board new-board)
  (let* (
      (pos (current-position new-board))
      (value (cell previous-board (car pos) (cadr pos)))
    )
    (+ current-score value)
  )
)

(defun apply-heuristic (node heuristic-value)
  (create-node  ; this wasn't my favorite implementation but the only one i could find at this point :c
    (node-board node)
    (node-score node)
    (node-final-score node)
    (node-parent node)
    (node-depth node)
    heuristic-value
    (+ (node-depth node) heuristic-value)
  )
)

(defun calculate-heuristic-default (node)
  (cond
    ((= 0 (total-available-squares (node-board node))) 0)
    ((= 0 (/ (float (total-points (node-board node))) (total-available-squares (node-board node)))) 0)
    (t 
      (/
        (- (node-final-score node) (node-score node))
        (/ 
          (float (total-points (node-board node))) 
          (total-available-squares (node-board node))
        )
      )
    )
  )
)

(defun calculate-heuristic-proprietary (node) 
  (if (node-parent node)
    (let (
        (score-square (- (node-score node) (node-score (node-parent node))))
        (number-moves (movements-from-square (node-board node)))
      )
      (if (eq number-moves 0) 
        (- most-positive-fixnum score-square )
        (/ number-moves score-square)
      )
    )
    0
  )
)

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
          (return (create-solution-node current-node (length queue) (length visited)))
          (progn
            (setq visited (cons current-node visited))
            (if b
              (setq queue (append (cdr queue) (generate-children current-node)))
              (if (> (1+ (node-depth current-node)) maximum-depth)
                (return (create-solution-node nil (length queue) (length visited)))
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
        (order-queue (remove-if (lambda (node) (> (node-f node) (node-f (car queue)))) (cdr queue)))
        (list (car queue))
        (order-queue (remove-if (lambda (node) (<= (node-f node) (node-f (car queue)))) (cdr queue)))
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
          (return (create-solution-node current (length queue) (length visited)))
          (progn
            (do* ((children (generate-children current) (cdr children))) ((null children)) 
              (let* (
                  (child (apply-heuristic (car children) (funcall heuristic-function (car children))))
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

;; ------------------------------------------ Penetrance

(defun penetrance (solution)
 "Funcao para calcular e definir a penetrancia da solucao final"
    (coerce (/ (solution-lenght solution) (generated-nodes solution)) 'float)
) 

;; ------------------------------------------ Branching Fator

(defun average-branching-fator (solution &optional (depth (solution-lenght solution)) (generatedNodes (generated-nodes solution))
  (tolerance 0.1) (min 0) (max (generated-nodes solution)))
  (let ((average (/ ( + min max) 2)))
       (cond 
        ((< (- max min) tolerance) (/ (+ max min) 2))
        ((< (auxiliar-branching average depth generatednodes) 0) 
                (average-branching-fator solution depth generatednodes tolerance average max))
          (t (average-branching-fator solution depth generatednodes tolerance min average))
       )
  )
)

(defun auxiliar-branching (average depth generatedNodes)
 (cond
   ((= 1 depth) (- average generatednodes))
   (T (+  (expt average depth) 
          (auxiliar-branching average (- depth 1) generatednodes)))
  )
)
