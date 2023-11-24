;; Carrega os outros ficheiros de código, escreve e lê ficheiros, e trata da interação com o utilizador

(load "./lisp/procura.l")
(load "./lisp/puzzle.l")

;;==========================================    INTERFACE MESSAGES FOR INTERFACE    ==========================================



(defun startMessage()
"Mostra as opções iniciais"
  (progn
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

(defun chooseAlgorithmMessage()
"Mostra as opções de algoritmos de procura para aplicar no problema/tabuleiro"
  (progn
    (format t"~%~% -------------------------------------------------------------")
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

(defun chooseProblemMessage()
"Mostra as opções iniciais"
  (progn
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

(defun chooseDepthMessage()
"Mostra a caixa para o utilizador escolher a profundidade maxima desejada"
  (progn
    (format t "~%~% -----------------------------------------")
    (format t "~%|      Escolha a profundidade maxima      |")
    (format t "~%|       desejada para executar o DFS      |")
    (format t "~%|              0    voltar                |")
    (format t "~%|                                         |")
    (format t "~% -----------------------------------------~%~%> ")
 )
)


(defun chooseHeuristicMessage()
"Mostra a caixa para o utilizador escolher a heuristica desejada"
  (progn
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