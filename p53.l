; (defpackage :p53)
; (in-package :p53)
(setf *player-one* -1)
(setf *player-two* -2)
(defun oposite-player (player) 
  (cond 
    ((not (numberp player)) nil)
    ((= player *player-one*) *player-two*)
  )
)

(defun test-state()
  (list 
    '() 
    (list 0 0)
  )
)

; ======================================== STATE ========================================
(defun state-board (state) (car state))
(defun state-score-1 (state) (caadr state))
(defun state-score-2 (state) (cadadr state))


(defun jogar (state tempo-maximo)

)



; ======================================== GAME ========================================


; ======================================== TIMER ========================================

(defun get-milliseconds ()
  (* 1000 (/ (float (get-internal-run-time)) internal-time-units-per-second))
)

(defun get-elapsed-milliseconds (start-time)
  (- (* 1000.0d0 (get-milliseconds)) (* 1000.0d0 start-time))
)

; ======================================== ALGORITHM ========================================


; ======================================== NODE ========================================

