
(load "./lisp/algoritmo.l")
(load "./lisp/jogo.l")

;;  processo:
;;  start > user - pc ou pc - pc > quem joga primeiro > quanto tempo por jogada 1000 - 5000 ms
;;  loop: realiza jogada > print numeros duplos se for o caso para escolher qual quer remover (se for o pc escolhe o maior e nao da print)
;;  

(defun start-message ()
"Mostra as opções iniciais"
  (progn
    (run-program "clear")
    (format t "~%╔═════════════════════════════════════╗")
    (format t "~%║           Jogo do Cavalo            ║")
    (format t "~%║                                     ║")
    (format t "~%║   1. Utilizador VS Computador       ║")
    (format t "~%║   2. Computador VS Computador       ║")
    (format t "~%║   0. Sair                           ║")
    (format t "~%║                                     ║")
    (format t "~%╚═════════════════════════════════════╝   ~%~%> ")
  )
)

;(start)