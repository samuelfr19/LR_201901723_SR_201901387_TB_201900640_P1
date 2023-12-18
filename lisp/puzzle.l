;; Código relacionado com o problema.

; ---- Com as regras aplicadas não há resolução possivel para o problema E
(setq aplicar-regras t)


; ------------------------------------------------- Movements and operations

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

(defun current-position (board &optional (value-to-find t) (row 0) (column 0) )
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

(defun place-list (position row &optional (value nil))
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
"Remove o valor duplo mais baixo"
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

(defun all-possible-movements (board)
"Lista de todas as possiveis movimentações"
  (remove nil (list 
    (move-1 board) 
    (move-2 board) 
    (move-3 board) 
    (move-4 board) 
    (move-5 board)
    (move-6 board) 
    (move-7 board) 
    (move-8 board) 
  ))
)

(defun move-1 (board)
"1st operator"
  (move board 2 -1)
)

(defun move-2 (board)
"2nd operator"
  (move board 2 1)
)

(defun move-3 (board)
"3rd operator"
  (move board 1 2)
)

(defun move-4 (board)
"4th operator"
  (move board -1 2)
)

(defun move-5 (board)
"5th operator"
  (move board -2 1)
)

(defun move-6 (board)
"6th operator"
  (move board -2 -1)
)

(defun move-7 (board)
"7th operator"
  (move board -1 -2)
)

(defun move-8 (board)
"8th operator"
  (move board 1 -2)
)

(defun move (board amount-row amount-column)
"Move o cavalo (T) no tabuleiro pela quantidade de linhas e colunas possiveis"
  (let 
    ((horse-position (current-position board)))
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
            (T
              (if aplicar-regras
                (apply-number-rules (place 
                  (+ amount-row (car horse-position))
                  (+ amount-column (cadr horse-position)) 
                  (place (car horse-position) (cadr horse-position) board)
                  T
                ) value-at)
                (place 
                  (+ amount-row (car horse-position))
                  (+ amount-column (cadr horse-position)) 
                  (place (car horse-position) (cadr horse-position) board)
                  T
                )
              )
            )
          )
        )
      ) 
    )
  )
)

; ------------------------------------------------- Calculations

(defun total-points (board)
"Pontos totais no tabuleiro"
  (apply #'+ (remove t (remove nil(apply #'append board))))
)

(defun total-available-squares (board)
"Quantidade de casas ainda disponiveis no tabuleiro"
  (length (remove t (remove nil(apply #'append board))))
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
  (t (cons (subseq board 0 n) (random-board (subseq board n) n))
 )
)
)

