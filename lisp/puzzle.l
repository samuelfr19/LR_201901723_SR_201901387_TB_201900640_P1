;; Código relacionado com o problema.

;; ola
;; ^^^^Contribuicao do batista :)


(defun tabuleiro-teste ()
"Tabuleiro de teste sem nenhuma jogada realizada"
  '(
    (94  25  54  89  21  8   36  14  41  96) 
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

(defun print-board (board)
  (cond
    ((not (car board)) (format t "~%-----------------------------------------------~%"))
    (T 
      (progn
        (print-row (car board))
        (print-board (cdr board))
      )
    )
  )
)

(defun print-row (row &optional (column 9))
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


(defun current-position (board &optional (row 0) (column 0))
"Returns the current position of the horse - where in the board is T"
  (cond
    ((eq (nth column (car board)) T) (list row column))
    ((<= (length (car board)) column) (current-position (cdr board) (1+  row)))
    ((car board) (current-position board row (1+ column)))
    (T nil)
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

(defun place (row column board &optional (value nil))
"Places a *value* into the *board* in the position (*row*, *column*) with origin in the top left"
  (cond
    ((null (car board)) nil)
    ((eq row 0) (cons (place-list column (car board) value) (cdr board)))
    (T (cons (car board) (place (1- row) column (cdr board) value)))
  )
)

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
