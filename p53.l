; (defpackage :p53)
; (in-package :p53)

(defun teste-tabuleiro()

)

(defun estado-tabuleiro (estado) (car estado))
(defun estado-pontuacao-um (estado) (cadar estado))
(defun estado-pontuacao-dois (estado) (caddar estado))

(defun jogar (estado tempo-maximo)

)


(defun get-milliseconds ()
  (* 1000 (/ (float (get-internal-run-time)) internal-time-units-per-second))
)

(defun get-elapsed-milliseconds (start-time)
  (- (* 1000.0d0 (get-milliseconds)) (* 1000.0d0 start-time))
)

; ======================================== JOGO ========================================

