;======================================  NODE STRUCTURE

(defun create-node (score-one score-two turn-player board &optional (parent nil))
  (list score-one score-two turn-player board parent)
)

(defun node-score-one (node)
  (first node)
)

(defun node-score-two (node)
  (second node)
)

(defun node-turn-player (node)
  (third node)
)

(defun node-board (node)
  (fourth node)
)

(defun node-parent (node)
  (fifth node)
)

(defun node-next-turn (node)
  (cond
    ((not (numberp (node-turn-player node))) nil)
    ((= *player-one* (node-turn-player node)) *player-two*)
    ((= *player-two* (node-turn-player node)) *player-one*)
    (t nil)
  )
)

(defun node-print (node &optional (stream-to-write-to t))
  (progn
    (if (node-parent node) (node-print (node-parent node) stream-to-write-to))
    (format stream-to-write-to "~%SCORE: ~5a | ~5a|TURN: ~5a~%~%" (node-score-one node) (node-score-two node) (node-turn-player node))
    (print-board (node-board node) stream-to-write-to)
  )
)

;;@LUIS OLHA AQUI PARA MUDAR 
(defun node-change-board(node new-board)
    (create-node (node-score-one node) (node-score-two node) (node-turn-player node) new-board (node-parent node))
)

;======================================  SOLUTION-NODE STRUCTURE
; o número de nós analisados,
; o número de cortes efetuados (de cada tipo), 
; o tempo gasto em cada jogada e o tabuleiro atual
(defun create-solution-node (optimal-move alpha-cuts beta-cuts time-elapsed)
  (list optimal-move alpha-cuts beta-cuts time-elapsed)
)

(defun solution-node-optimal-move (node)
  (first node)
)

(defun solution-node-alpha-cuts (node)
  (second node)
)

(defun solution-node-beta-cuts (node)
  (third node)
)

(defun solution-node-time-elapsed (node)
  (fourth node)
)

;======================================  ALGORITHM

(defun apply-expanding-function (expanding-function node)
  (let ((children (funcall expanding-function (node-board node) (node-turn-player node))))
    (if children
      (remove nil (mapcar (lambda (movement-result)
          (let* (
              (current-board (movement-result-board movement-result))
              (val (car (last (find-double-digit-numbers current-board))))
              (new-board (remove-double-number current-board (cell current-board (car val) (cadr val))))
            )
            (if new-board
              (cond
                ((= (movement-result-player movement-result) *player-one*)
                  (create-node
                    (+ (node-score-one node) (movement-result-score movement-result))
                    (node-score-two node)
                    (node-next-turn node)
                    new-board
                    node
                  )
                )
                ((= (movement-result-player movement-result) *player-two*)
                  (create-node
                    (node-score-one node)
                    (+ (node-score-two node) (movement-result-score movement-result))
                    (node-next-turn node)
                    new-board
                    node
                  )
                )
                (t nil)
              )
              (cond
                ((= (movement-result-player movement-result) *player-one*)
                  (create-node
                    (+ (node-score-one node) (movement-result-score movement-result))
                    (node-score-two node)
                    (node-next-turn node)
                    current-board
                    node
                  )
                )
                ((= (movement-result-player movement-result) *player-two*)
                  (create-node
                    (node-score-one node)
                    (+ (node-score-two node) (movement-result-score movement-result))
                    (node-next-turn node)
                    current-board
                    node
                  )
                )
                (t nil)
              )

            )
          )
        )
        children
      ))
      nil ; its a solution node
    )
  )
)

(defun evaluate-node (node cpu-player) ; podemos adicionar multiplicar por movimentos disponiveis
  (cond
    ((= cpu-player *player-one*) (- (node-score-one node) (node-score-two node)))
    ((= cpu-player *player-two*) (- (node-score-two node) (node-score-one node)))
    (t most-negative-fixnum)
  )
)

(defun minimax (initial-node max-depth max-time expanding-function)
  (cond
    ((or (= max-depth 0)))
  )
)
(minimax (create-node 0 0 *player-two* (random-board)) 5 1000 'all-possible-movements)
