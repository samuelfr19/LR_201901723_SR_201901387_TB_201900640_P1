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
    (format t "~%|   1   Escolher problema      |")
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
    (format t "~%~% ----------------------------------------")
    (format t "~%|           Escolher Problema            |")
    (format t "~%|                                        |")
    (format t "~%|     Escolha o problema a resolver      |")
    (format t "~%|         (numero do problema)           |")
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
"Funcao que inicia todo o processo do programa, apresenta as opcoes iniciais e pede ao utilizador para escolher 
  a opcao para avancar para o proximo passo"
  (progn
    (start-message)
    (let ((in (read)))
      (if (or (not (numberp in)) (> in 1) (< in 0)) (start))
      (cond
        ((eq in 1) (choose-problem))
        ((eq in 0) (progn (format t "Obrigado!")(quit)))
      )
    )
  )
)

(defun choose-problem()
 (progn 
  (show-problems)
  (choose-problem-message)
      (let ((opt (read)))
           (cond ((eq opt '0) (start))
                 ((not (numberp opt)) 
                  (progn (format t "Insira uma opcao valida!!") (choose-problem))) 
                 (T
                  (let ((problem-list (get-problems)))
                  (if (= opt 6) (choose-algorithm (random-board)))
                  (if (or (< opt 0) (> opt (- (length problem-list) 1))) (progn (format t "Insira uma opcao valida!!") (choose-problem))
                                 (choose-algorithm (1- opt))
                  )
                  ))
           )
       )
    )
)


(defun choose-algorithm(problem)
"Executa um algoritmo, dependendo da opcao escolhida"
  (progn (choose-algorithm-message)
    (let ((opt (read)))
      (cond ((not (numberp opt)) (progn (format t "Insira uma opcao valida!!") (choose-algorithm)))
        ((or (> opt 3) (< opt 0)) (progn (format t "Insira uma opcao valida!!") (choose-algorithm)))
        ((eq opt 0) (choose-problem))  
        (T       
            (ecase opt
              (1    
                (let* ((maxDepth (choose-depth problem))
                  (solution (list (get-time) (dfs (test-node problem) maxDepth) (get-time) 'DFS maxDepth)))
                  (progn (write-final-results-file solution) solution)  
                )
              )
              (2  
                (let
                  ((solution (list (get-time) (bfs  (test-node problem)) (get-time) 'BFS)))
                  (progn (write-final-results-file solution) solution)
                )
              )
              (3  
                (let* ((heuristic (choose-heuristic problem))
                  (solution (list (get-time) (a-star (test-node problem)) (get-time) 'A* heuristic)))
                  (progn (write-final-results-file solution) solution)  
                )))))))
)


(defun choose-depth(problem)
  (if (not (choose-depth-message))
      (let ((opt (read)))
         (cond ((eq opt '0) (choose-algorithm problem))
               ((or (not (numberp opt)) (< opt 0)) (progn (format t "Insira uma opcao valida!!")) (choose-depth))
               (T opt))))
)

(defun choose-heuristic(problem)
  (if (not (choose-heuristic-message))
      (let ((opt (read)))
         (cond ((eq opt '0) (choose-algorithm problem))
                (if (or (not (numberp opt)) (> opt 2) (< opt 0)) (progn (format t "Insira uma opcao valida!!~%") (chooseHeuristic))
               (T opt))))
)
)


(defun write-final-results-file (solution)
"Escreve, no ficheiro de resultados, a solucao e medidas de desempenho de um determinado problema"
  (let* (
      (startTime (first solution))
      (solutionNode (second solution))
      (endTime (third solution))
      (search (fourth solution))
    ) 
    (with-open-file (str "./lisp/resultados.dat" :direction :output :if-exists :append :if-does-not-exist :create)
      (write-final-results str solutionNode startTime endTime search) 
    )
  )
)

; -------------------------------------------------- Prints




(defun show-problems (&optional (i 1) (problems (get-problems)))
    (cond
     ((null problems))
     (T (let ((problem (car problems)))
        (format T "~%~d - Problema ~d (~d pontos):~%" i (car problem) (cadr problem))
        (print-board (caddr problem)))
        (show-problems (+ i 1) (cdr problems))
     ) 
)
)


(defun write-final-results (stream solutionNode startTime endTime search &optional depth)
  (progn 
    ;;(format stream "~%~tRESULTADOS FINAIS DA RESOLUCAO DO TABULEIRO:~%")
    ;;(format stream "~%~t -- Objetivo caixas fechadas: ~a " (countClosedBoxes(getSolutionNode solutionNode)))
    ;;(format stream "~%~t -- Algoritmo: ~a " search)
    ;;(format stream "~%~t -- Inicio: ~a:~a:~a" (first startTime) (second startTime) (third startTime))
    ;;(format stream "~%~t -- Fim: ~a:~a:~a" (first endTime) (second endTime) (third endTime))
    ;;(print-board  (first solutionNode))
    ;;(format stream "~%~%~%")


    

    (format T "~%~tRESULTADOS FINAIS DA RESOLUCAO DO TABULEIRO:~%")
    (format t "~%~t -- Objetivo de pontos: ~a " (node-final-score solutionnode))
    (format t "~%~t -- Algoritmo: ~a " search)
    (format T "~%~t -- Inicio: ~a:~a:~a" (first startTime) (second startTime) (third startTime))
    (format t "~%~t -- Fim: ~a:~a:~a" (first endTime) (second endTime) (third endTime))
    (format t "~%~t -- Numero de nós gerados: ~a" (+ (second solutionNode)(third solutionNode)))
    (format t "~%~t -- Numero de nós expandidos: ~a" (second solutionNode))
    ;;(format t "~%~t -- Penetrancia: ~F" (penetrance solutionNode))
    ;;(format T "~%~t -- Fator de ramificacao media: ~F" (averageBranchingFator solutionNode))
    (if (eq search 'DFS)
        (format t "~%~t -- Profundidade maxima: ~a" (car depth)))
    (format t "~%~t -- Comprimento da solucao: ~a" (get-solution-lenght solutionnode))
    (format t "~%~%~t -- Estado Inicial~%")
    (print-board (first (first solutionNode)))
    (format t "~%~t -- Estado Final~%")
    (print-board (get-solution-node solutionNode))
    (format t "~%~%")
    (format t "Obrigado por jogar!~%")
    (quit)
  )
)

; -------------------------------------------------- Read from problemas.dat
(defun get-problems()
  (progn
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
)


(defun get-problem (n)
  (nth n (get-problems))
)



(defun get-time()
  "Retorna o tempo atual em forma de lista"
  (multiple-value-bind (s m h) (get-decoded-time)
    (list h m s)
   )
)
 
