;; Código relacionado com o problema.

;; ola
;; ^^^^Contribuicao do batista :)

; ------------------------------------------------- Test Boards
; ---------- IF WE APPLY THE NUMBER RULES THERE IS NO POSSIBLE ANSWER TO PROBLEM E
(setq aplicar-regras nil)

(defun tabuleiro-teste ()
"Tabuleiro de teste sem nenhuma jogada realizada"
  '(
    (94  25  54  89  21  nil 36  14  41  96) 
    (78  47  56  23  5   49  13  12  26  60) 
    (0   27  17  83  34  93  74  52  45  80) 
    (69  9   77  95  55  39  91  73  57  30) 
    (24  15  22  86  1   11  68  79  76  72) 
    (81  48  32  2   64  16  50  37  29  71) 
    (99  51  6   18  53  28  7   63  10  88) 
    (59  42  46  85  90  75  87  43  20  31) 
    (3   61  58  44  65  82  19  4   35  62) 
    (33  70  84  40  66  38  92  67  98  97)
  )
)

(defun tabuleiro-jogado ()
"Tabuleiro de teste igual ao anterior mas tendo sido colocado o cavalo na posi��o: i=0 e j=0"
  '(
    (T   25  54  89  21  8   36  14  41  96) 
    (78  47  56  23  5   NIL 13  12  26  60) 
    (0   27  17  83  34  93  74  52  45  80) 
    (69  9   77  95  55  39  91  73  57  30) 
    (24  15  22  86  1   11  68  79  76  72) 
    (81  48  32  2   64  16  50  37  29  71) 
    (99  51  6   18  53  28  7   63  10  88) 
    (59  42  46  85  90  75  87  43  20  31) 
    (3   61  58  44  65  82  19  4   35  62) 
    (33  70  84  40  66  38  92  67  98  97)
  )
)

(defun test-board()
'(
  (nil  nil  nil  nil  nil  nil  nil  nil  nil  nil )
  (nil  nil  nil  nil  nil  nil  nil  nil  nil  nil )
  (nil  nil  nil  60   nil  50   nil  nil  nil  nil )
  (nil  nil  70   nil  nil  nil  40   nil  nil  nil )
  (nil  nil  nil  nil  T    nil  nil  nil  nil  nil )
  (nil  nil  80   nil  nil  nil  30   nil  nil  nil )
  (nil  nil  nil  10   nil  20   nil  nil  nil  nil )
  (nil  nil  nil  nil  nil  nil  nil  nil  nil  nil )
  (nil  nil  nil  nil  nil  nil  nil  nil  nil  nil )
  (nil  nil  nil  nil  nil  nil  nil  nil  nil  nil )
 )
)

; ------------------------------------------------- Movements and operations

(defun print-board (board)
"Prints the *board* in a clean and proper way"
  (cond
    ((not (car board)) (format t "~%--------------------------------------------------~%"))
    (T 
      (progn
        (print-row (car board))
        (print-board (cdr board))
      )
    )
  )
)

(defun print-row (row &optional (column 9))
"Prints a *row* of the board in a clean and proper way"
  (cond 
    ((> 0 column) (format t "~%"))
    (t 
      (progn
        (format t "~5a" (car row))
        (print-row (cdr row) (1- column))
      )
    )
  )
)

(defun cell (board row column)
  "returns what is in the *column*, of the row number *row* of *board*"
  (nth column (nth row board))
)

(defun current-position (board &optional (value-to-find t) (row 0) (column 0) )
"Returns the current position of the horse - where in the board is T"
  (cond
    ((null board) nil)
    ((eq value-to-find (cell board row column)) (list row column))
    ((< 9 row) nil)
    ((< 9 column) (current-position board  value-to-find (1+ row)))
    (t (current-position board value-to-find row (1+ column)))
  )
)

(defun place (row column board &optional (value nil))
"Places a *value* into the *board* in the position (*row*, *column*) with origin in the top left"
  (cond
    ((null (car board)) nil)
    ((eq row 0) (cons (place-list column (car board) value) (cdr board)))
    (T (cons (car board) (place (1- row) column (cdr board) value)))
  )
)

(defun place-list (position row &optional (value nil))
"Places a *value* into a list *row* in the *position*"
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

(defun total-points (board)
  (apply #'+ (remove t (remove nil(apply #'append board))))
)

(defun total-available-houses (board)
  (length (remove t (remove nil(apply #'append board))))
)

; ------------------------------------------------- Operators

(defun apply-number-rules (board val)
  (remove-reverse-number (remove-double-number board val) val)
)

(defun remove-double-number (board val) 
  (cond
    ((not (numberp val)))
    ((= val (reverse-digits val))
      (let* (
          (double-digit-numbers (find-double-digit-numbers board))
          (position-to-remove (car double-digit-numbers)) ; the smallest value will always be extracted like this
        )
        (place (car position-to-remove) (cadr position-to-remove) board)
      )
    )
    (t board)
  )
)

(defun remove-reverse-number (board val)
  (let ((pos (current-position board (reverse-digits val))))
    (cond
      ((null pos) board)
      (t (place (car pos) (cadr pos) board))
    )
  )
)

(defun find-double-digit-numbers (board &optional (value 1))
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
  (cond
    ((not (numberp n)) nil) ; we shouldnt get a non number here
    ((> n 99) nil) ; numbers cant be greater then 99
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
  (cond
    ((current-position board) board)
    (T (place 0 pos board T))
  )
)

(defun all-possible-movements (board)
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
"Move the horse (T) on the *board* by *amount-row* and *amount-column"
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
; ------------------------------------------------- Generating a board

(defun shuffle (board)
"Shuffles a list"
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
"Creates a list with numbers from 0 to *n* (excluded)"
  (cond 
    ((= n 1) (list 0))
    (T (append (list (1- n)) (list-numbers (1- n))))
  )
)

(defun random-board (&optional (board (shuffle (list-numbers))) (n 10))
"Creates a random board" 
 (cond
  ((null board) nil)
  (t (cons (subseq board 0 n) (random-board (subseq board n) n)))
 )
)