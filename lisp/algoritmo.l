(defun test-node ()
  (create-node 0 0 *player-one* (random-board))
)

(defun test-node1 ()
  (create-node 17 84 *player-one* 
    '(
      (92  42  31  96  7   -1  80  3   52  72 )
      (97  76  30  nil 87  89  93  19  0   78 )
      (46  88  61  79  60  57  32  50  83  47 )
      (9   25  36  49  21  74  85  55  54  4  )
      (68  82  34  73  13  16  22  14  27  64 )
      (59  29  39  28  94  43  51  95  6   10 )
      (20  33  99  81  5   69  67  2   58  37 )
      (15  98  8   53  18  90  63  26  62  35 )
      (nil 86  40  66  65  23  77  44  1   11 )
      (70  75  41  91  -2  12  45  24  56  38 )
    )
  )
)

;======================================  NODE STRUCTURE
(defstruct node 
  score-one
  score-two 
  turn-player
  board 
  parent
)

(defun create-node (score-one score-two turn-player board &optional (parent nil))
  (make-node
    :score-one score-one
    :score-two score-two
    :turn-player turn-player
    :board board 
    :parent parent
  )
)

(defun node-next-turn (node)
  (cond
    ((not (numberp (node-turn-player node))) nil)
    ((= *player-one* (node-turn-player node)) *player-two*)
    ((= *player-two* (node-turn-player node)) *player-one*)
    (t nil)
  )
)

(defun node-to-first-move-node (node)
  (cond 
    ((null (node-parent node)) nil)
    ((null (node-parent (node-parent node))) node)
    (t (node-to-first-move-node (node-parent node)))
  )
)

(defun node-print (node &optional (stream-to-write-to t))
  (if (node-parent node) (node-print (node-parent node) stream-to-write-to))
  (format stream-to-write-to "~%SCORE: ~5a | ~5a|TURN: ~5a~%~%" (node-score-one node) (node-score-two node) (node-turn-player node))
  (print-board (node-board node) stream-to-write-to)
)

;======================================  SOLUTION-NODE STRUCTURE
; o número de nós analisados,
; o número de cortes efetuados (de cada tipo), 
; o tempo gasto em cada jogada e o tabuleiro atual
(defstruct solution-node
  optimal-move 
  evaluation 
  alpha-cuts 
  beta-cuts 
  time-elapsed
)

(defun create-solution-node (optimal-move evaluation alpha-cuts beta-cuts time-elapsed)
  (make-solution-node 
    :optimal-move optimal-move 
    :evaluation evaluation 
    :alpha-cuts alpha-cuts 
    :beta-cuts beta-cuts 
    :time-elapsed time-elapsed)
)

(defun solution-node-to-move (solution-node) ;;alterar
  (let* ((node (node-to-first-move-node (solution-node-optimal-move solution-node)))
         (player (node-next-turn node)))
    (find-value (node-board node) player)))


;======================================  ALGORITHM
(defun apply-expanding-function (expanding-function node)
  (let* ((player (node-turn-player node))
         (children (funcall expanding-function (node-board node) player)))
    (if children
        (let* ((new-nodes (remove nil 
          (mapcar (lambda (movement-result)
                    (let* ((current-board (movement-result-board movement-result))
                            (val (car (last (find-double-digit-numbers current-board))))
                            (new-board (if val 
                                          (remove-double-number current-board 
                                                                (cell current-board (car val) (cadr val)))
                                          current-board)))
                      (if new-board
                          (let* ((score-one (if (= player *player-one*) (movement-result-score movement-result) 0))
                                  (score-two (if (= player *player-two*) (movement-result-score movement-result) 0)))
                            (create-node (+ (node-score-one node) score-one)
                                          (+ (node-score-two node) score-two)
                                          (node-next-turn node)
                                          new-board
                                          node))
                          nil)))
          children)))
          new-nodes)
        nil))))


(defun evaluate-node (node cpu-player)
  (when (or (= cpu-player *player-one*) (= cpu-player *player-two*))
    (- (if (= cpu-player *player-one*) 
           (node-score-one node) 
           (node-score-two node))
       (if (= cpu-player *player-one*) 
           (node-score-two node) 
           (node-score-one node)))
    most-negative-fixnum))


(defun minimax (node max-depth max-time expanding-function)
  (let* ((children (apply-expanding-function expanding-function node))
         (maximizing-player (node-next-turn node)))
    (cond
      ((or (= max-depth 0) (null children)) 
       (create-solution-node node
                             (evaluate-node node maximizing-player)
                             0 ; alpha cuts
                             0 ; beta cuts
                             0 ; time elapsed
                             ))
      ((= (node-turn-player node) maximizing-player)
       (let ((best-value (create-solution-node nil most-negative-fixnum 0 0 0)))
         (mapcar (lambda (child)
                   (let ((minimax-node (minimax child maximizing-player (1- max-depth) max-time expanding-function)))
                     (setf best-value 
                           (if (> (solution-node-evaluation best-value) (solution-node-evaluation minimax-node)) 
                               best-value
                               minimax-node))))
                 children)
         best-value))
      (t
       (let ((best-value (create-solution-node nil most-positive-fixnum 0 0 0)))
         (mapcar (lambda (child)
                   (let ((minimax-node (minimax child maximizing-player (1- max-depth) max-time expanding-function)))
                     (setf best-value 
                           (if (< (solution-node-evaluation best-value) (solution-node-evaluation minimax-node)) 
                               best-value
                               minimax-node))))
                 children)
         best-value)))))