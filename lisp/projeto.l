;; Carrega os outros ficheiros de código, escreve e lê ficheiros, e trata da interação com o utilizador

(load "./lisp/procura.l")
(load "./lisp/puzzle.l")

;;==========================================    INTERFACE MESSAGES FOR INTERFACE    ==========================================

(defun start-message ()
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

(defun choose-algorithm-message ()
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

(defun choose-problem-message ()
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

(defun choose-depth-message ()
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


(defun choose-heuristic-message ()
"Mostra a caixa para o utilizador escolher a heuristica desejada"
  (progn
    (run-program "clear")
    (format t "~%~% -----------------------------------------")
    (format t "~%|      Escolha a Heuristica desejada      |")
    (format t "~%|        para executar o algoritmo        |")
    (format t "~%|     1        Heuristica Base            |")
    (format t "~%|     2   Heuristica Personalizada        |")
    (format t "~%|     0             voltar                |")
    (format t "~%|                                         |")
    (format t "~% -----------------------------------------~%~%> ")
 )
)

; -------------------------------------------------- Menu Navigation

(defun first-start ()
  "Funcao que inicia pela primeira vez o programa, permite gerar um novo tabuleiro para cada vez que é iniciado o programa"
  (progn 
    (f-to-file)
    (start)
  )
)

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

(defun choose-problem ()
  (progn 
    (show-problems)
    (choose-problem-message)
    (let ((opt (read)))
      (cond 
        ((eq opt '0) (start))
        (
          (not (numberp opt)) (progn (format t "Insira uma opcao valida!!") (choose-problem))) 
        (T
          (let ((problem-list (get-problems)))
            (if (or (< opt 0) (> opt (length problem-list)))
              (progn 
                (format t "Insira uma opcao valida!!") 
                (choose-problem)
              )
              (choose-algorithm (1- opt))
            )
          )
        )
      )
    )
  )
)

(defun choose-algorithm (problem)
"Executa um algoritmo, dependendo da opcao escolhida"
  (progn (choose-algorithm-message)
    (let ((opt (read)))
      (cond 
        ((not (numberp opt)) (progn (format t "Insira uma opcao valida!!") (choose-algorithm problem)))
        ((or (> opt 3) (< opt 0)) (progn (format t "Insira uma opcao valida!!") (choose-algorithm problem)))
        ((eq opt 0) (choose-problem))
        (T
          (ecase opt
            (1    
              (let* ((maxDepth (choose-depth problem))
                (solution (list (get-internal-real-time) (dfs (node-from-problem problem) maxDepth) (get-internal-real-time) 'DFS maxdepth)))
                (progn (final-results solution) solution)  
              )
            )
            (2  
              (let
                ((solution (list (get-internal-real-time) (bfs  (node-from-problem problem)) (get-internal-real-time) 'BFS)))
                (progn (final-results solution) solution)
              )
            )
            (3  
              (let* (
                  (heuristic (choose-heuristic problem))
                  (solution (list (get-internal-real-time) (a-star (node-from-problem problem) heuristic) (get-internal-real-time) 'A* heuristic) )
                )
                (progn (final-results solution) solution)  
              )
            )
          )
        )
      )
    )
  )
)

(defun choose-depth (problem)
  (if (not (choose-depth-message))
    (let ((opt (read)))
        (cond 
          ((eq opt '0) (choose-algorithm problem))
          ((or (not (numberp opt)) (< opt 0)) (progn (format t "Insira uma opcao valida!!")) (choose-depth problem))
          (T opt)
        )
    )
  )
)

(defun choose-heuristic (problem)
  (if (not (choose-heuristic-message))
    (let ((opt (read)))
      (cond 
        ((eq opt '0) (choose-algorithm problem))
        ((or (not (numberp opt)) (> opt 2) (< opt 0)) 
          (progn 
            (format t "Insira uma opcao valida!!~%") 
            (choose-heuristic problem)
          )
        )
        ((eq opt '1) 'calculate-heuristic-default)
        ((eq opt '2) 'calculate-heuristic-proprietary)
        (T opt)
      )
    )
  )
)

; -------------------------------------------------- File management

(defun  show-problems (&optional (i 1) (problems (get-problems)))
  (cond
    ((null problems))
    (T (let ((problem (car problems)))
      (format T "~%~d - Problema ~d (~d pontos):~%" i (car problem) (cadr problem))
      (print-board (caddr problem)))
      (show-problems (+ i 1) (cdr problems))
    ) 
  )
)

(defun final-results (solution)
  (with-open-file (stream "./resultados.dat" :direction :output :if-exists :append :if-does-not-exist :create)
    (write-final-results solution stream)
    (close stream)
  )
  (write-final-results solution t)
)

(defun write-final-results (solution type)
  (let* (
      (start-time (first solution))
      (solution-node (second solution))
      (end-time (third solution))
      (search (fourth solution))
      (elapsed-time (* (/ (float (- end-time start-time)) internal-time-units-per-second) 1000))
      (seconds (truncate (/ elapsed-time 1000)))
      (miliseconds (mod (round elapsed-time) 1000))
    ) 
    (progn 
      (format type "~t RESULTADOS FINAIS DA RESOLUCAO DO TABULEIRO:~%")
      (format type "~%~t -- Algoritmo: ~a " search)
      (if (equal search 'A*)
        (format type "~%~t -- Heuristica: ~a " (car (last solution)))
      )
      (if (equal search 'DFS)
        (format type "~%~t -- Profundidade Maxima: ~a " (car (last solution)))
      )
      (format type "~%~t -- Tempo Demorado: ~w.~w" seconds miliseconds)
      (format type "~%~t -- Numero de nós gerados: ~a" (generated-nodes solution-node))
      (format type "~%~t -- Numero de nós expandidos: ~a" (third solution-Node))
      (format type "~%~t -- Penetrancia: ~F" (penetrance solution-Node))
      (format type "~%~t -- Fator de ramificacao media: ~F" (average-branching-fator solution-Node))
      (format type "~%~t -- Comprimento da solucao: ~a" (solution-lenght solution-node))
      (format type "~%~t -- Caminho solução: ~a" (first solution-node))
      (format type "~%~t -- Tabuleiro Final: ~%")
      (format type "--------------------------------------------------~%~%")
      (print-board (final-node solution-node) type)
      (format type "~%~%")
      (if (equal type t)
        (format t "Obrigado por jogar!~%~%Escreva a instrucao (start) para comecar de novo.")
      )
    )
  )
)

; -------------------------------------------------- File management

(defun get-problems ()
  (with-open-file (stream "problemas.dat" :if-does-not-exist :error)
    (do (
        (result nil (cons next result))
        (next (read stream nil 'eof) (read stream nil 'eof))
      )
      ((equal next 'eof)
        (progn
          (finish-output stream)
          (close stream)
          (reverse result)
        )
      )
    )
  )
)

(defun get-problem (n)
  (nth n (get-problems))
)

(defun write-problem (problem stream)
  (progn
    (format stream "~%(~w ~w~%~t(~%" (car problem) (cadr problem))
    (mapcar 
      (lambda (line)  
        (progn
          (format stream "~2t(")
          (mapcar 
            (lambda (cel)
              (format stream "~5a" cel)
            ) 
            line)
          (format stream ")~%")
        )
      )
      (car (last problem)))
    (format stream "~t)~%)~%~%")
  )
)

(defun f-to-file ()
  (let* (
      (problems (get-problems))
      (problem-f (find-if (lambda (problem) (string= "F" (car problem))) problems))
    )
    (cond
      (problem-f
        (let* (
            (updated-problems 
              (mapcar 
                (lambda (problem) 
                  (if (string= (car problem) "F") 
                    (list (car problem) (cadr problem) (random-board))
                    problem
                  )
                ) 
                problems 
              )
            )
          )
          (with-open-file (output-stream "problemas.dat" :direction :output)
            (progn
              (mapcar (lambda (problem) (write-problem problem output-stream)) updated-problems)
              (finish-output output-stream)
              (close output-stream)
            )
          )
          updated-problems
        )
      )
      (t "Didn't find problem f")
    )
  )
)

(defun write-problem (problem stream)
  (progn
    (format stream "~%(~w ~w~%~t(~%" (car problem) (cadr problem))
    (mapcar 
      (lambda (line)  
        (progn
          (format stream "~2t(")
          (mapcar 
            (lambda (cel)
              (format stream "~5a" cel)
            ) 
            line)
          (format stream ")~%")
        )
      )
      (car (last problem)))
    (format stream "~t)~%)~%~%")
  )
)
    
(defun get-time() 
  "Retorna o tempo atual em forma de lista"
  (multiple-value-bind (s m h) (get-decoded-time)
    (list h m s)
  )
)

(first-start)