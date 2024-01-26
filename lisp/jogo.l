;====================================== MOVEMENT-RESULT
; for handling each movemnt, returns (score board)

(defstruct movement-result
  score
  player
  board
)

(defun create-movement-result (score player board)
  (make-movement-result 
    :score score 
    :player player 
    :board board
  )
)

(defun movement-result-print (movement-result &optional (stream-to-write-to T))
  (progn
    (format stream-to-write-to "~%______________________________________________________~%~%")
    (format stream-to-write-to "SCORE: ~15a~t~15aPLAYER: ~a~%~%"
      (movement-result-score movement-result) (code-char 124) (movement-result-player movement-result))
    (print-board (movement-result-board movement-result))
  )
)

;======================================  AUXILIARIES

(defun print-board (board &optional (stream-to-write-to t))
"Dá print do tabuleiro de maneira limpa e legivel"
  (cond
    ((not (car board)) (format stream-to-write-to "______________________________________________________~%"))
    (T 
      (progn
        (if (= 10 (length board)) 
          (progn
            (format stream-to-write-to "~5t~4,2a~4,2a~4,2a~4,2a~4,2a~4,2a~4,2a~4,2a~4,2a~4,2a~%"
                0 1 2 3 4 5 6 7 8 9)
            (format stream-to-write-to "~5,9t________________________________________________~%")
          )
        )
        (format stream-to-write-to "~4a|" (- 9 (1- (length board))))
        (print-row (car board) stream-to-write-to)
        (print-board (cdr board) stream-to-write-to)
      )
    )
  )
)

(defun print-row (row &optional (stream-to-write-to t) (column 9) )
"Dá print da linha de um tabuleiro de maneira limpa e legivel"
  (cond 
    ((> 0 column) (format stream-to-write-to "~%"))
    (t 
      (progn
        (format stream-to-write-to "~5a" (car row))
        (print-row (cdr row) stream-to-write-to (1- column) )
      )
    )
  )
)

(defun shuffle (board)
"Mistura a lista de posições"
  (cond
    ((car board) (let (
          (n (nth (random (length board)) board))
        )
        (append (cons n nil) (shuffle (remove n board)))
      )
    )
    (T '())
  )
)

(defun list-numbers (&optional (n 100))
"Cria uma lista de numeros de 0 até 100"
  (cond 
    ((= n 1) (list 0))
    (T (append (list (1- n)) (list-numbers (1- n))))
  )
)

(defun cell (board row column)
"Devolve o atomo que está numa certa coluna, no numero da linha do tabuleiro"
  (if (or (> row 9) (< row 0)(> column 9)(< column 0)) 
    nil
    (nth column (nth row board))
  )
)

(defun find-value (board &optional (value-to-find *player-one*) (row 0) (column 0) )
"Devolve a posição atual do cavalo, onde está no tabuleiro (T)" 
  (cond
    ((null board) nil)
    ((eq value-to-find (cell board row column)) (list row column))
    ((< 9 row) nil)
    ((< 9 column) (find-value board  value-to-find (1+ row)))
    (t (find-value board value-to-find row (1+ column)))
  )
)

(defun place (row column board &optional (value nil))
"Coloca um certo valor no tabuleiro na posição fornecida (linha, culuna) em origem no topo esquerdo"
  (cond
    ((null (car board)) nil)
    ((eq row 0) (cons (place-list column (car board) value) (cdr board)))
    (T (cons (car board) (place (1- row) column (cdr board) value)))
  )
)

(defun place-list (position row value)
"Coloca um valor na lista (linha) na posição dada"
  (cond
    ((eq position 0) (cons value (cdr row)))
    (T 
      (append 
        (list (car row)) 
        (place-list (1- position) (cdr row) value)
      )
    )
  )
)

(defun remove-double-number (board val)
"Remove o *val* do tabuleiro"
  (cond
    ((not (numberp val)) nil)
    ((= val (reverse-digits val))
      (let ((position-to-remove (find-value board val)))
        (if position-to-remove
          (place (car position-to-remove) (cadr position-to-remove) board)
          nil
        )
      )
    )
    (t nil)
  )
)

(defun remove-reverse-number (board val)
"Remove da tabela o numero contrario ao recebido no parametro e troca por NIL"
  (let ((pos (find-value board (reverse-digits val))))
    (cond
      ((null pos) board)
      (t (place (car pos) (cadr pos) board))
    )
  )
)

(defun find-double-digit-numbers (board &optional (value 1))
"Encontra números no tabuleiro com o mesmo numero"
  (cond
    ((> value 9) '())
    ((find-value board (* value 11)) 
      (append 
        (list (find-value board (* value 11)))
        (find-double-digit-numbers board (1+ value))
      )
    )
    (t  (find-double-digit-numbers board (1+ value)))
  )
)

(defun reverse-digits (n)
"Troca os dois digitos de um numero"
  (cond
    ((not (numberp n)) nil) 
    ((> n 99) nil) 
    ((< n 10) (* n 10))
    (t 
      (let (
          (first-digit (floor n 10))
          (second-digit (mod n 10))
        )
        (+ (* 10 second-digit) first-digit)
      )
    )
  )
)

;==================================================  OPERATORS

(defun list-possible-movements (board player)
  (remove nil (if (find-value board player)
    (mapcar (lambda (n) (operator (1+ n) board player 'possible-movement)) (list-numbers 8))
    (mapcar (lambda (movement-result) (find-value (movement-result-board movement-result) player)) (all-horse-start-positions board player))
  ))
)

(defun no-possible-movements (board)
  (and (null (list-possible-movements *player-one*)) 
       (null (list-possible-movements *player-two*)))
)

(defun possible-movement (board amount-row amount-column player)
  (let* (
      (player-position (find-value board player))
      (new-row (+ (car player-position) amount-row))
      (new-column (+ (cadr player-position) amount-column))
      (square-value (cell board new-row new-column))
    )
    (cond
      ((or (> new-row 9) (< new-row 0)(> new-column 9)(< new-column 0)) nil)
      ((null square-value) nil)
      ((or (= *player-one* square-value) (= *player-two* square-value)) nil)
      (t (list new-row new-column))
    )
  )
)

(defun set-horse (board player)
  (let* (
      (horse-start-positions (all-horse-start-positions board player))
      (initial-value (create-movement-result most-negative-fixnum player nil))
      (movement-result 
        (reduce 
          (lambda (current-max possible-max)
            (if (> (movement-result-score current-max) (movement-result-score possible-max) )
              current-max
              possible-max
            )
          )
          horse-start-positions
          :initial-value initial-value
        )
      )
    )
    movement-result
  )
)

(defun all-horse-start-positions (board player &optional (column 0))
"Coloca o cavalo em toda a primeira linha do tabuleiro para a geração dos primeiros nós"
  (cond 
    ((not (numberp player)) nil)
    ((find-value board player) nil)
    ((= player *player-one*)
      (let ((first-row (car board)))
        (cond
          ((> column 9) '())
          ((nth column first-row) 
            (append 
              (list 
                (create-movement-result (cell board 0 column) player (set-horse-start-position board player column))
              ) 
              (all-horse-start-positions board player (1+ column))
            )
          )
          (t (all-horse-start-positions board player (1+ column)))
        )
      )
    )
    ((= player *player-two*)
      (let ((last-row (car (last board))))
        (cond
          ((> column 9) '())
          ((nth column last-row) 
            (append 
              (list 
                (create-movement-result (cell board 9 column) player (set-horse-start-position board player column))
              ) 
              (all-horse-start-positions board player (1+ column))
            )
          )
          (t (all-horse-start-positions board player (1+ column)))
        )
      )
    )
    (T nil)
  )
)

(defun set-horse-start-position (board player &optional (pos 0))
"Coloca o cavalo numa certa posição do tabuleiro"
  (cond
    ((not (numberp player)) board)
    ((find-value board player) board)
    ((= player *player-one*) 
      (remove-reverse-number (place 0 pos board player) (cell board 0 pos))
    )
    ((= player *player-two*)
      (remove-reverse-number (place 9 pos board player) (cell board 9 pos)) 
    )
    (T board)
  )
)

(defun move (board row column player)
  (cond
    ((or 
      (not (numberp column))
      (not (numberp row))
      (not (numberp player))
    ) nil)
    (
      (or (and (/= player *player-one*) (/= player *player-two*))
        (< row 0) (> row 9)
        (< column 0) (> column 9)
      )
      nil
    )
    (t 
      (let* (
          (possible-movements (all-possible-movements board player))
          (oponent-possible-movements (list-possible-movements board (oposite-player player)))
        )
        (if (some (lambda (e) (equal e (list row column))) oponent-possible-movements)
          nil
          (if possible-movements
            (car (remove nil (mapcar 
              (lambda (movement-result) 
                (if (equal (find-value (movement-result-board movement-result) player) (list row column))
                  movement-result
                  nil
                )
              ) 
            possible-movements)))
            nil
          )
        )
      )
    )
  )
)

(defun all-possible-movements (board player)
"Lista de todas as possiveis movimentações"
  (if (find-value board player)
    (remove nil  
      (mapcar (lambda (n) (operator (1+ n) board player 'move-amount)) (list-numbers 8))
    )
    (all-horse-start-positions board player)
  )
)

(defun operator (num board player expanding-func)
  (case num
    (1 (funcall expanding-func board  2   -1  player))
    (2 (funcall expanding-func board  2   1   player))
    (3 (funcall expanding-func board  1   2   player))
    (4 (funcall expanding-func board  -1  2   player))
    (5 (funcall expanding-func board  -2  1   player))
    (6 (funcall expanding-func board  -2  -1  player))
    (7 (funcall expanding-func board  -1  -2  player))
    (8 (funcall expanding-func board  1   -2  player))
  )
)

(defun move-amount (board amount-row amount-column player)
"Move o cavalo (player) no tabuleiro pela quantidade de linhas e colunas possiveis"
  (if (find-value board player) 
    (let* (
        (horse-position (find-value board player))
        (new-row (+ amount-row (car horse-position)))
        (new-column (+ amount-column (cadr horse-position)))
      )
      (cond
        ((or (> new-column 9)(< new-column 0)(> new-row 9)(< new-row 0)) nil)
        (T
          (let 
            ((value-at (cell board new-row new-column )))
            (cond 
              ((not (numberp value-at)) nil)
              ((or (= value-at *player-one*) (= value-at *player-two*)) nil)
              ((some (lambda (e) (equal e (list new-row new-column))) (list-possible-movements board (oposite-player player)))
                nil
              )
              (T 
                (create-movement-result value-at player
                  (remove-reverse-number
                    (place new-row new-column (place (car horse-position) (cadr horse-position) board) player)
                    value-at
                  )
                )
              )
            )
          )
        ) 
      )
    )
  )
)


;==================================================  GENERATING BOARD

(defun random-board (&optional (board (shuffle (list-numbers))) (n 10))
"Cria um tabuleiro aleatório"
  (cond
    ((null board) nil)
    (t (cons (subseq board 0 n) (random-board (subseq board n) n)))
  )
)

