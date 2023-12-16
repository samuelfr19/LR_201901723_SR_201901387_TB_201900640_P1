;; Carrega os outros ficheiros de código, escreve e lê ficheiros, e trata da interação com o utilizador

(load "./lisp/procura.l")
(load "./lisp/puzzle.l")

;;==========================================    INTERFACE MESSAGES FOR INTERFACE    ==========================================

(defun start-message()
"Mostra as opções iniciais"
  (progn
    (run-program "clear")
    (format t "~%~% ------------------------------")
    (format t "~%|           Jogo  do           |")
    (format t "~%|            Cavalo            |")
    (format t "~%|                              |")
    (format t "~%|   1 Visualizar Problemas     |")
    (format t "~%|   2  Escolher problema       |")
    (format t "~%|   0        Sair              |")
    (format t "~%|                              |")
    (format t "~% ------------------------------   ~%~%> ")
 )
)

(defun choose-algorithm-message()
"Mostra as opções de algoritmos de procura para aplicar no problema/tabuleiro"
  (progn
    (run-program "clear")
    (format t "~%~% -------------------------------------------------------------")
    (format t "~%|    Escolha o algoritmo de procura que pretende utilizar     |")
    (format t "~%|                                                             |")
    (format t "~%|                1     Depth first search                     |")
    (format t "~%|                2    Breadth first search                    |")
    (format t "~%|                3            A*                              |")
    (format t "~%|                0          Voltar                            |")
    (format t "~%|                                                             |")
    (format t "~% -------------------------------------------------------------~%~%> ")
  )
)

(defun choose-problem-message()
"Mostra as opções iniciais"
  (progn
    (run-program "clear")
    (format t "~%~% ----------------------------------------")
    (format t "~%|           Escolher Problema            |")
    (format t "~%|                                        |")
    (format t "~%|    Escolha o problema introduzindo     |")
    (format t "~%|         o numero do problema           |")
    (format t "~%|             0   Voltar                 |")
    (format t "~%|                                        |")
    (format t "~% ----------------------------------------   ~%~%> ")
 )
)

(defun choose-depth-message()
"Mostra a caixa para o utilizador escolher a profundidade maxima desejada"
  (progn
    (run-program "clear")
    (format t "~%~% -----------------------------------------")
    (format t "~%|      Escolha a profundidade maxima      |")
    (format t "~%|       desejada para executar o DFS      |")
    (format t "~%|              0    voltar                |")
    (format t "~%|                                         |")
    (format t "~% -----------------------------------------~%~%> ")
 )
)


(defun choose-heuristic-message()
"Mostra a caixa para o utilizador escolher a heuristica desejada"
  (progn
    (run-program "clear")
    (format t "~%~% -----------------------------------------")
    (format t "~%|      Escolha a Heuristica desejada      |")
    (format t "~%|        para executar o algoritmo        |")
    (format t "~%|     1   Heuristica Personalizada        |")
    (format t "~%|     2        Heuristica Base            |")
    (format t "~%|     0             voltar                |")
    (format t "~%|                                         |")
    (format t "~% -----------------------------------------~%~%> ")
 )
)

; -------------------------------------------------- Menu Navigation

(defun start ()
  (progn
    (startMessage)
    (let ((in (read)))
      (cond
        ((eq in 1) t ;(show-problems)
        )
        ((eq in 2) (choose-problem-message))
        (T nil)
      )
    )
  )
)

; -------------------------------------------------- File management
(defun get-problems()
  (with-open-file (stream "problemas.dat" :if-does-not-exist :error)
    (do
      (
        (result nil (cons next result))
        (next (read stream nil 'eof) (read stream nil 'eof))
      )
      ((equal next 'eof) (reverse result))
    )
  )
)

(defun get-problem (n)
  (nth n (get-problems))
)