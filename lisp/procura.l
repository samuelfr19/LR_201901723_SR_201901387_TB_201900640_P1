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

(defun first-node (node)
"Devolve primeiro nó pai"
  (cond
    ((null (node-parent node)) node)
    (t (first-node (node-parent node)))
  )
)

; ------------------------------------------------- Solution Node 

(defun create-solution-node (node size-of-queue size-of-visited)
"Cria um nó solução"
  (list (node-to-movements node) size-of-queue size-of-visited node)
)

(defun solution-node-path (solution-node)
"Devolve a lista de jogadas até ao nó solução"
  (nth 0 solution-node)
)

(defun solution-node-size-queue (solution-node)
"Devolve o numero de nós na lista de abertos"
  (nth 1 solution-node)
)

(defun solution-node-size-visited (solution-node)
"Devolve o numero de nós na lista de fechados"
  (nth 2 solution-node)
)

(defun solution-node-node (solution-node)
"Devolve o nó solução"
  (nth 3 solution-node)
)

(defun generated-nodes (solution)
"Devolve o total de nós gerados tanto na lista de abertos como na de fechados"
  (+ (solution-node-size-queue solution) (solution-node-size-visited solution))
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
    (format stream-to-write-to "~%~%Score: ~a/~a | Depth: ~a " (node-score node) (node-final-score node) (node-depth node) )
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
"Devolve true ou nill dependendo se é um nó solução"
  (<= (node-final-score node) (node-score node))
)

(defun calculate-score (current-score previous-board new-board)
"Atualiza a pontuação atual"
  (let* (
      (pos (current-position new-board))
      (value (cell previous-board (car pos) (cadr pos)))
    )
    (+ current-score value)
  )
)

(defun apply-heuristic (node heuristic-value)
"Devolve uma cópia de uma nó com o valor heristico e com a função de avaliação"
  (create-node 
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
"Calcula a heuristca de um nó com base na fornecida"
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
"Calcula o valor heuristico de nó usando a heuristica personalizada"
  (let ((sum-squares-left (apply #'+ (remove nil (apply #'append (node-board node))))))
    (cond
      ((eq sum-squares-left 0) most-positive-fixnum)
      (t (/))
    )
  )
)

; ------------------------------------------------- Algorythms
; was getting stack overflow on recursive functions so added iterative aproach to mitigate them
(defun fs-recursive (node b &optional (queue nil) (visited nil))
"Base do algoritmo BFS e DFS implementado recursivamente"
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
"Base do algoritmo BFS e DFS implementado com recurso a iteração"
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
                nil
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
"Aplica o bfs a um nó"
  (fs node T)
)

(defun dfs (node &optional (max-depth most-positive-fixnum))
"Aplica o dfs a um nó"
  (fs node NIL max-depth)
)

;------------------------ a-star
(defun order-queue (queue)
"Função auxiliar do a* que ordena a lista de abertos"
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
"Aplica o a* a um nó"
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
                    (if (> (node-f found-in-queue) (node-f child)) 
                      (setq queue (order-queue (cons child (remove found-in-queue queue))))
                    )
                  )
                  (found-in-visited 
                    (if (> (node-f found-in-visited) (node-f child)) 
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

(defun penetrance (solution-node)
"Funcao para calcular e definir a penetrancia da solucao final"
    (coerce (/ (node-depth (solution-node-node solution-node)) (generated-nodes solution-node)) 'float)
) 

;; ------------------------------------------ Branching Fator

(defun average-branching-fator (solution &optional (depth (node-depth (solution-node-node solution))) (generatedNodes (generated-nodes solution))
"Função para calcular e definir o fator de ramificação média da solução final"
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
