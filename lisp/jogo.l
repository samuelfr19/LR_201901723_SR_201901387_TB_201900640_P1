(defun print-board (board &optional (stream-to-write-to t))
"Dá print do tabuleiro de maneira limpa e legivel"
  (cond
    ((not (car board)) (format stream-to-write-to "--------------------------------------------------~%"))
    (T 
      (progn
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

(defun cell (board row column)
"Devolve o atomo que está numa certa coluna, no numero da linha do tabuleiro"
  (nth column (nth row board))
)

(defun current-position (board &optional (value-to-find -1) (row 0) (column 0) )
"Devolve a posição atual do cavalo, onde está no tabuleiro (T)"
  (cond
    ((null board) nil)
    ((eq value-to-find (cell board row column)) (list row column))
    ((< 9 row) nil)
    ((< 9 column) (current-position board  value-to-find (1+ row)))
    (t (current-position board value-to-find row (1+ column)))
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

; ------------------------------------------------- Operators

(defun apply-number-rules (board val)
"Aplica as regras de corte das casas"
  (remove-reverse-number (remove-double-number board val) val)
)

(defun remove-double-number (board val) 
"Remove o maior valor duplo por defeito"
  (cond
    ((not (numberp val)))
    ((= val (reverse-digits val))
      (let* (
          (double-digit-numbers (find-double-digit-numbers board))
          (position-to-remove (car double-digit-numbers))
        )
        (if position-to-remove
          (place (car position-to-remove) (cadr position-to-remove) board)
          board
        )
      )
    )
    (t board)
  )
)

(defun remove-reverse-number (board val)
"Remove da tabela o numero contrario ao recebido no parametro e troca por NIL"
  (let ((pos (current-position board (reverse-digits val))))
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
    ((current-position board (* value 11)) 
      (append 
        (list (current-position board (* value 11)))
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

(defun all-horse-start-positions (board &optional (column 0))
"Coloca o cavalo em toda a primeira linha do tabuleiro para a geração dos primeiros nós"
  (cond 
    ((current-position board) nil)
    (T (let ((first-row (car board)))
      (cond
        ((> column 9) '())
        ((nth column first-row) 
          (append (list (set-horse-start-position board column)) (all-horse-start-positions board (1+ column)))
        )
        (t (all-horse-start-positions board (1+ column)))
      )
    ))
  )
)

(defun set-horse-start-position (board &optional (pos 0))
"Coloca o cavalo numa certa posição do tabuleiro"
  (cond
    ((current-position board) board)
    (T (place 0 pos board T))
  )
)

(defun all-possible-movements (board player)
"Lista de todas as possiveis movimentações"
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
)

(defun move-1 (board player)
"1st operator"
  (move board 2 -1 player)
)

(defun move-2 (board player)
"2nd operator"
  (move board 2 1 player)
)

(defun move-3 (board player)
"3rd operator"
  (move board 1 2 player)
)

(defun move-4 (board player)
"4th operator"
  (move board -1 2 player)
)

(defun move-5 (board player)
"5th operator"
  (move board -2 1 player)
)

(defun move-6 (board player)
"6th operator"
  (move board -2 -1 player)
)

(defun move-7 (board player)
"7th operator"
  (move board -1 -2 player)
)

(defun move-8 (board player)
"8th operator"
  (move board 1 -2 player)
)

(defun move (board amount-row amount-column player)
"Move o cavalo (T) no tabuleiro pela quantidade de linhas e colunas possiveis" ;todo update
  (let 
    ((horse-position (current-position board player)))
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
            ((or (= value-at -1) (= value-at -2)))
            (T ;todo rethink this luis, u can do better
              (apply-number-rules (place 
                (+ amount-row (car horse-position))
                (+ amount-column (cadr horse-position)) 
                (place (car horse-position) (cadr horse-position) board)
                T
              ) value-at)
            )
          )
        )
      ) 
    )
  )
)

; ------------------------------------------------- Generating a board

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

(defun random-board (&optional (board (shuffle (list-numbers))) (n 10))
"Cria um tabuleiro aleatório"
  (cond
    ((null board) nil)
    (t (cons (subseq board 0 n) (random-board (subseq board n) n)))
  )
)

