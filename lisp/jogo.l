;====================================== MOVEMENT-RESULT
; for handling each movemnt, returns (score board)

(defun create-movement-result (score player board)
  (list score player board)
)

(defun movement-result-score (movement-result)
  (car movement-result)
)

(defun movement-result-player (movement-result)
  (cadr movement-result)
)

(defun movement-result-board (movement-result)
  (caddr movement-result)
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
  (nth column (nth row board))
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
      (let ((possible-movements (all-possible-movements board player)))
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

(defun all-possible-movements (board player)
"Lista de todas as possiveis movimentações"
  (if (find-value board player)
    (remove nil (list 
      (move-1 board player) 
      (move-2 board player) 
      (move-3 board player) 
      (move-4 board player) 
      (move-5 board player)
      (move-6 board player) 
      (move-7 board player) 
      (move-8 board player) 
    ))
    (all-horse-start-positions board player)
  )
)

(defun move-1 (board player)
"1st operator"
  (move-ammount board 2 -1 player)
)

(defun move-2 (board player)
"2nd operator"
  (move-ammount board 2 1 player)
)

(defun move-3 (board player)
"3rd operator"
  (move-ammount board 1 2 player)
)

(defun move-4 (board player)
"4th operator"
  (move-ammount board -1 2 player)
)

(defun move-5 (board player)
"5th operator"
  (move-ammount board -2 1 player)
)

(defun move-6 (board player)
"6th operator"
  (move-ammount board -2 -1 player)
)

(defun move-7 (board player)
"7th operator"
  (move-ammount board -1 -2 player)
)

(defun move-8 (board player)
"8th operator"
  (move-ammount board 1 -2 player)
)

(defun move-ammount (board amount-row amount-column player)
"Move o cavalo (T) no tabuleiro pela quantidade de linhas e colunas possiveis"
  (let 
    ((horse-position (find-value board player)))
    (cond
      ((null horse-position)
        nil
      )
      ((or 
        (> (+ (car horse-position) amount-row) 9) 
        (< (+ (car horse-position) amount-row) 0)
        (> (+ (cadr horse-position) amount-column) 9)
        (< (+ (cadr horse-position) amount-column) 0)
        ) nil)
      (T
        (let 
          ((value-at (cell board (+ amount-row (car horse-position)) (+ amount-column (cadr horse-position)) )))
          (cond 
            ((not (numberp value-at)) nil)
            ((or (= value-at *player-one*) (= value-at *player-two*)) nil)
            (T 
              (create-movement-result value-at player
                (remove-reverse-number
                  (place 
                    (+ amount-row (car horse-position))
                    (+ amount-column (cadr horse-position)) 
                    (place (car horse-position) (cadr horse-position) board)
                    player
                  )
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

;==================================================  GENERATING BOARD

(defun random-board (&optional (board (shuffle (list-numbers))) (n 10))
"Cria um tabuleiro aleatório"
  (cond
    ((null board) nil)
    (t (cons (subseq board 0 n) (random-board (subseq board n) n)))
  )
)

