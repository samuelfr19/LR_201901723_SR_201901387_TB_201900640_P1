(defparameter *player-one* -1)
(defparameter *player-two* -2)
(defparameter *max-depth* 10)

(defun oposite-player (player)
  (cond 
    ((not (numberp player)) nil)
    ((= *player-one* player) *player-two*)
    ((= *player-two* player) *player-one*)
    (t nil)
  )
)

(load "./lisp/jogo.l")
(load "./lisp/algoritmo.l")

(defun initial-status ()
"Funcao para determinar o estado inicial do primeiro nó com o board gerado e os cavalos colocados nas melhores posições"
  (let* (
      (movement-result-player-one (set-horse (random-board) *player-one*))
      (score-player-one (movement-result-score movement-result-player-one))
      (board-after-player-one (movement-result-board movement-result-player-one))
      (movement-result-player-two (set-horse board-after-player-one *player-two*))
      (final-board (movement-result-board movement-result-player-two))
      (score-player-two (movement-result-score movement-result-player-two))
    )
    
    (create-node
      score-player-one
      score-player-two
      *player-one*
      final-board
    )
  )
)

;;==========================================    INTERFACE MESSAGES FOR INTERFACE    ==========================================

(defun start-message ()
"Mostra as opções iniciais"
  (progn
    (run-program "clear")
    (format t "~%╔═════════════════════════════════════╗")
    (format t "~%║           Jogo do Cavalo            ║")
    (format t "~%║                                     ║")
    (format t "~%║   1. Jogador VS Computador          ║")
    (format t "~%║   2. Computador VS Computador       ║")
    (format t "~%║   0. Sair                           ║")
    (format t "~%║                                     ║")
    (format t "~%╚═════════════════════════════════════╝   ~%~%> ")
  )
)


(defun start ()
"Funcao que inicia todo o processo do programa, apresenta as opcoes iniciais e pede ao utilizador para escolher 
  a opcao para avancar para o proximo passo"
  (progn
    (start-message)
    (let ((in (read)))
      (if (or (not (numberp in)) (> in 2) (< in 0)) (start))
      (cond
        ((eq in 1) (first-player))
        ((eq in 2) (time-play))
        ((eq in 0) (progn (format t "Obrigado!")(quit)))
      )
    )
  )
)
  

(defun first-player-message ()
"Mostra as opções de quem joga primeiro"
  (progn
    (run-program "clear")
    (format t "~%╔═════════════════════════════════════╗")
    (format t "~%║           Jogo do Cavalo            ║")
    (format t "~%║                                     ║")
    (format t "~%║         Quem joga primeiro?         ║")
    (format t "~%║                                     ║")
    (format t "~%║   1. Jogador                        ║")
    (format t "~%║   2. Computador                     ║")
    (format t "~%║   0. Voltar                         ║")
    (format t "~%║                                     ║")
    (format t "~%╚═════════════════════════════════════╝   ~%~%> ")
  )
)

(defun first-player()
"Funcao para determinar qual o primeiro a fazer a jogada, entre o jogador e o computador"
  (progn
    (first-player-message)
    (let ((in (read)))
      (if (or (not (numberp in)) (> in 2) (< in 0)) (first-player-message))
      (cond
        ((eq in 1) (time-play 1))
        
        ((eq in 2) (time-play 2))
        
        ((eq in 0) (start))
      )
    )
  )
)

(defun time-play-message ()
"Mostra as opções de quanto tempo ha por jogada"
  (progn
    (run-program "clear")
    (format t "~%╔═════════════════════════════════════╗")
    (format t "~%║           Jogo do Cavalo            ║")
    (format t "~%║                                     ║")
    (format t "~%║   Defina o tempo para cada jogada   ║")
    (format t "~%║         Entre 1000 a 5000 ms        ║")
    (format t "~%║                                     ║")
    (format t "~%║   0. Voltar                         ║")
    (format t "~%║                                     ║")
    (format t "~%╚═════════════════════════════════════╝   ~%~%> ")
  )
)

(defun time-play(&optional (first nil))
"Funcao para escolher quanto tempo o computador deve ter para fazer a sua jogada"
  (progn
    (time-play-message)
    (let ((in (read)))
    (if (and (numberp in) (= 0 in)) (start)
      (if (or (not (numberp in)) (> in 5000) (< in 1000)) (time-play first)
        (start-game in first)
      ))
    )
  )
)

(defun final-results (node)
"Funcao para escrever no final do jogo qual o vencedor e quais os seus pontos"
  (if (> (node-score-one node)(node-score-two node))
      (format t "~%O jogador 1 ganhou o jogo com ~d pontos.~%Enquanto que o jogador 2 ficou com ~d pontos.~%" (node-score-one node) (node-score-two node))
      (format t "~%O jogador 2 ganhou o jogo com ~d pontos.~%Enquanto que o jogador 1 perdeu com ~d pontos.~%" (node-score-two node) (node-score-one node))
  )
  (restart-game)
)

(defun restart-game()
"Funcao para recomeçar jogo"
 (format t "Introduza 0 para começar um novo jogo~%")
  (let ((in (read)))
    (if (= in 0)
      (start)
      (restart-game)
    )
  )
)


;;;;==========================================    GAME STARTER    ==========================================

(defun start-game (time first)
"Funcao para começar e criar o loop de jogo caso seja jogador vs computador ou so mesmo computador"
  (let (
      (current-node (initial-status))
      (user (when first (if (= first 1) *player-one* *player-two*)))
    )
    (node-print current-node)
    (loop
      (setf current-node
        (if (or (null user) (/= (node-turn-player current-node) user) )
          (let ((pc-move (alfabeta current-node 'all-possible-movements *max-depth* time)))
            (print-move pc-move)
            (setf (node-parent (solution-node-optimal-move pc-move)) nil)
            (solution-node-optimal-move pc-move)
          )
          (player-move current-node time)
        )
      ) 
      (when (no-possible-movements (node-board current-node)) (return (final-results current-node))   (return))
    )
  )
)


;;(start-game 5000 nil)
;;(print-move (alfabeta (test-node1) 'all-possible-movements *max-depth* 1000))
;;(print-move (alfabeta (initial-status) 'all-possible-movements *max-depth* 1000))


;;==========================================    PLAYER MOVE    ==========================================


(defun double-n-message (n)
"Mostra as opções de numeros a retirar"
  (progn
    (format t "Escolheu um número duplo, escolha outro para apagar:")
    (format t "~%~d~%~%" n)
  )
)

(defun double-n (node)
"Quando ha a existencia de um numero duplo é chamada esta funcao para pedir ao utilizador outro numero duplo para retirar do tabuleiro"
  (progn
    (node-print-individual node)
    (double-n-message (mapcar (lambda (x) (cell (node-board node) (car x) (cadr x))) (find-double-digit-numbers (node-board node))))
     (let ((in (read)))
      (if (and (= in (reverse-digits in))  (remove-double-number (node-board node) in))
        (progn (format t "Número ~d retirado ~%~%" in) (node-change-board node (remove-double-number (node-board node) in)))
        (progn (format t "Número ~d invalido, escolha outro~%" in) (double-n node))
      )
     )
  )
)

(defun read-position-input (a)
"Ler input do utilizador quando para mudar de posição, tanto para linha como coluna"
  (format t "~a: " a)
  (let ((in (read)))
    (if (and (numberp in) (<= 0 in 9))
      in
      (progn
        (format t "Entrada inválida. Tente novamente.~%")
        (read-position-input a)
      )
    )
  )
)

(defun player-move (node time)
"Input e movimentação do jogador"
  (let ((list (list-possible-movements (node-board node) (node-turn-player node))))
  (if list
    (progn
      (format t "Escolha a proxima posição do cavalo~%")
      (let* ( 
          (l (read-position-input "Linha")) 
          (c (read-position-input "Coluna")) 
        )
        (if (some (lambda (e) (equal e (list l c))) list)      
          (next-node-player node l c time) 
          (progn (format t "Movimento não é possivel, escolha outro!~%") (player-move node time))
        )
      )
    )
    (progn 
      (format t "Turno passado por não haver jogada possivel!") 
      (setf (node-turn-player node) (oposite-player (node-turn-player node)))
      node 
    )
  )
)
)

(defun next-node-player (node l c time)
"Movimentação e gestao do tipo de movimento"
  (let ((result (move (node-board node) l c (node-turn-player node))))
    (if result
      (let* ((score-one (if (= (node-turn-player node) *player-one*) 
                (+ (movement-result-score result) (node-score-one node))
                (node-score-one node)))
              (score-two (if (= (node-turn-player node) *player-two*) 
                (+ (movement-result-score result) (node-score-two node))
                (node-score-two node)))
              )
        (if (and (/= 0 (movement-result-score result))(= (movement-result-score result) (reverse-digits (movement-result-score result))))
          (double-n (create-node score-one score-two (oposite-player (node-turn-player node)) (movement-result-board result) (node-depth node) most-negative-fixnum node))  
          (create-node score-one score-two (oposite-player (node-turn-player node)) (movement-result-board result) (node-depth node) most-negative-fixnum node)
        )
      )
      (progn (format t "Movimento não é possivel, escolha outro!") (player-move node time))
     )
  )
)

;;==========================================    PRINT STATS    ==========================================

(defun print-move (solution-node)
"Funcao de gestao de print das estatisticas"
(progn
    (with-open-file (stream "./log.dat" :direction :output :if-exists :append :if-does-not-exist :create)
      (solution-node-print solution-node stream)
      (close stream)
    )
    (solution-node-print solution-node)
  )
)