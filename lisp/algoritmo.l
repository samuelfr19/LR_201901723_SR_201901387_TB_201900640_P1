;======================================  NODE STRUCTURE
(defstruct node 
  score-one
  score-two 
  turn-player
  depth
  board  
  evaluation 
  parent
)

(defun create-node (score-one score-two turn-player board &optional (depth 0) (evaluation most-negative-fixnum) (parent nil))
  (make-node
    :score-one score-one
    :score-two score-two
    :turn-player turn-player
    :depth depth
    :evaluation evaluation
    :board board 
    :parent parent
  )
)

(defun node-change-board (node new-board)
  (setf (node-board node) new-board)
)

(defun node-print (node &optional (stream-to-write-to t))
  (if (node-parent node) (node-print (node-parent node) stream-to-write-to))
  (format stream-to-write-to "~%SCORE: ~5a | ~5a|TURN: ~5a~%~%" (node-score-one node) (node-score-two node) (node-turn-player node))
  (print-board (node-board node) stream-to-write-to)
)

(defun node-print-individual (node &optional (stream-to-write-to t))
  (format stream-to-write-to "~%SCORE: ~5a | ~5a|TURN: ~5a~%~%" (node-score-one node) (node-score-two node) (node-turn-player node))
  (print-board (node-board node) stream-to-write-to)
)

(defun node-fix-movement (node)
  (cond 
    ((null (node-parent (node-parent node))) node)
    ((null (node-parent node)) (format t "pilinha"))
    (t (node-fix-movement (node-parent node)))
  )
)

;======================================  SOLUTION-NODE STRUCTURE
(defstruct solution-node
  optimal-move 
  alfa-cuts 
  beta-cuts 
  time-elapsed
  analized-nodes
)

(defun create-solution-node (optimal-move alfa-cuts beta-cuts time-elapsed &optional (analized-nodes 0))
  (make-solution-node 
    :optimal-move optimal-move 
    :alfa-cuts alfa-cuts 
    :beta-cuts beta-cuts 
    :time-elapsed time-elapsed
    :analized-nodes analized-nodes)
)

(defun solution-node-to-move (solution-node)
  (let* (
      (node (node-to-first-move-node (solution-node-optimal-move solution-node)))
      (player (oposite-player (node-player node)))
    )
    (find-value (node-board node) player)
  )
)

(defun solution-node-print  (solution-node &optional (type t))
"Dá print das estatisticas de cada jogada tanto para o file como para o terminal"
  (format type "~%analized nodes: ~5a |alfa: ~5a |beta: ~5a | time-elapsed: ~5a~%~%"
    (solution-node-analized-nodes solution-node)
    (solution-node-alfa-cuts solution-node)
    (solution-node-beta-cuts  solution-node)
    (solution-node-time-elapsed solution-node)
  )
  (node-print-individual (solution-node-optimal-move solution-node) type)
)

;======================================  ALGORITHM

(defun apply-expanding-function (expanding-function node)
  (let* (
      (children (funcall expanding-function (node-board node) (node-turn-player node)))
    )
    (when children
      (remove nil 
        (mapcar (lambda (movement-result)
          (let* (
              (current-board (movement-result-board movement-result))
              (val (car (last (find-double-digit-numbers current-board))))
              (new-board 
                (if val 
                  (remove-double-number current-board (cell current-board (car val) (cadr val)))
                  current-board
                )
              )
            )
            (when new-board
              (let* (
                  (score-one (if (= (node-turn-player node) *player-one*) (movement-result-score movement-result) 0))
                  (score-two (if (= (node-turn-player node) *player-two*) (movement-result-score movement-result) 0))
                  (current-node (create-node 
                    (+ (node-score-one node) score-one)
                    (+ (node-score-two node) score-two)
                    (oposite-player (node-turn-player node))
                    new-board
                    (1+ (node-depth node))
                    nil
                    node
                  ))
                )
                current-node
              )
            )
          )
        )children)
      )
    )
  )
)

(defun evaluate-node (node cpu-player)
  (setf (node-evaluation node ) 
    (let ((maximizing (= cpu-player (node-turn-player node))))
      (if (null (node-board node))
        (if maximizing
          most-negative-fixnum
          most-positive-fixnum
        )
        (if maximizing 
          (- (node-score-one node) (node-score-two node))
          (- (node-score-two node) (node-score-one node))
        )
      )
    )
  )
  node
)

(defun order-queue-of-nodes (list-of-nodes symb max-player)
  (let* (
      (sorted-evaluation 
        (sort list-of-nodes
          #'(lambda (x y)
            (progn
              (evaluate-node x max-player) (evaluate-node y max-player)
              (funcall symb (node-evaluation x) (node-evaluation y))
            )
          )
        )
      )
    )
    sorted-evaluation
  )
)

(defun pick-best-node (node1 node2 symb maximizing-player)
  (when 
    (and 
      (/= most-negative-fixnum (node-evaluation node1)) 
      (/= most-positive-fixnum (node-evaluation node1))
    ) 
    (evaluate-node node1 maximizing-player)
  )
  (when 
    (and 
      (/= most-negative-fixnum (node-evaluation node2)) 
      (/= most-positive-fixnum (node-evaluation node2))
    ) 
    (evaluate-node node2 maximizing-player)
  )
  (if (funcall symb (node-evaluation node1) (node-evaluation node2)) 
    node1 node2
  ) 
)


(defun alfabeta (node expanding-function max-depth max-time &optional (maximizing-player (node-turn-player node)))
  (let (
      (start-time (current-time-milliseconds))
      (alfa-cuts 0)
      (beta-cuts 0)
      (result-final nil)
      (analized-nodes 0)
      (memo-table (make-hash-table :test 'equalp)) ;memoization table
    )
    (labels
      ((alfabeta-helper (current-node current-depth &optional (alfa most-negative-fixnum) (beta most-positive-fixnum))
        (let* (
            (key (list current-node current-depth)) ; for memoization
            (cached-value (gethash key memo-table)) ; for memoization
          )
          (if cached-value ; for memoization
            cached-value ; for memoization
            (let ((r ; for memoization
                (progn (setq analized-nodes (+ 1 analized-nodes))
                  (cond
                    ((or (>= current-depth max-depth) (time-limit-exceeded-p start-time max-time))
                      (evaluate-node current-node maximizing-player)
                    )
                    (t
                      (let* (
                          (maximizing (= (node-turn-player current-node) maximizing-player))
                          (symb (if maximizing #'>= #'<=))
                          (children (apply-expanding-function expanding-function current-node))
                          (ordered-children (order-queue-of-nodes children symb maximizing-player))
                          (initial-value (if maximizing most-negative-fixnum most-positive-fixnum))
                          (result (create-node 0 0 0 nil current-depth initial-value nil))
                        )
                        (if ordered-children
                          (loop 
                            for child in ordered-children 
                            do 
                              (let* (
                                  (ab-node (alfabeta-helper child (1+ current-depth) alfa beta))
                                  (v (pick-best-node ab-node result symb maximizing-player))
                                  (v-eval (node-evaluation v))
                                )
                                (setq result v)
                                (if maximizing
                                  (progn
                                    (setq alfa (if (funcall symb alfa v-eval) alfa v-eval))
                                    (when (>= alfa beta) (progn (setf alfa-cuts (+ 1 alfa-cuts)) (return-from alfabeta-helper v)))
                                  )
                                  (progn
                                    (setq beta (if (funcall symb beta v-eval) beta v-eval))
                                    (when (>= alfa beta) (progn (setf beta-cuts (+ 1 beta-cuts)) (return-from alfabeta-helper v)))
                                  )
                                )
                              )
                            finally (return result)
                          )
                          (evaluate-node current-node maximizing-player)
                        )
                      )
                    )
                  )
                )
              ))
              (setf (gethash key memo-table) r) r ;memoization
            )
          )
        )
      )) 
      (setq result-final 
        (create-solution-node 
            (alfabeta-helper node 0 most-negative-fixnum most-positive-fixnum)
          alfa-cuts beta-cuts (time-elapsed start-time) analized-nodes
        )
      ))
    result-final
  )
)  



;======================================  TIMER

(defun time-limit-exceeded-p (start-time max-time)
  (let ((leeway (/ max-time 10)))
    (>= (+ leeway (time-elapsed start-time)) max-time)
  )
)

(defun current-time-milliseconds ()
  (* 1000 (get-universal-time))
)

(defun time-elapsed (start-time)
  (- (current-time-milliseconds) start-time)
)