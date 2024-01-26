(defparameter *player-one* -1)
(defparameter *player-two* -2)

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


;;  processo:
;;  start > user - pc ou pc - pc > quem joga primeiro > quanto tempo por jogada 1000 - 5000 ms
;;  loop: realiza jogada > print numeros duplos se for o caso para escolher qual quer remover (se for o pc escolhe o maior e nao da print)
;______________________________________________________________________________________________________________________
;   Fazer a verificação se é possivel o jogador se mexer sequer antes de existir uma jogada

; ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀ ⠀  ⢠⣾⣿⣿⣷⣄⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀           
; ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀ ⠀ ⠀⠀⠀⢀⣴⣿⣿⣿⣿⣿⣿⣥⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀          
; ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀ ⠀⠀⠀⠀⣠⣾⣿⣿⣿⣿⣿⣿⣿⣿⣿⡆⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀          
; ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀ ⠀⢀⣼⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣷⣄⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀         
; ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣠⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣦⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀        
; ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢀⣼⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⡆⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀       
; ⠀⠀⠀⠀⠀⠀⠀⠀⠀⣰⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣷⣄⠀⠀⠀⠀⠀⠀⠀⠀⠀      
; ⠀⠀⠀⠀⠀⠀⠀⢠⣾⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣤⠀⠀⠀⠀⠀⠀⠀⠀      
; ⠀⠀⠀⠀⠀⠀⢠⣾⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣷⡄⠀⠀⠀⠀⠀⠀      
; ⠀⠀⠀⠀⠀⣴⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣦⡀⠀⠀⠀⠀     
; ⠀⠀⠀⢠⣾⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣄⠀⠀⠀    
; ⠀⢀⣴⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣷⠄⠀   
; ⣠⣾⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣷⣦   
; ⠻⠿⠿⠿⠿⠿⠿⠿⠿⠿⠿⢿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⡛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛⠛   
; ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀       
; ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
; ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
; ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
; ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
; ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢀⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
; ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
; ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
; ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
; ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
; ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
; ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠐⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
; ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
; ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
; ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
; ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠠⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
; ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⡯⠀⠀⠀⠀⠀⠀⠀

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
  (progn
    (time-play-message)
    (let ((in (read)))
    (if (and (numberp in) (= 0 in)) (start)
      (if (or (not (numberp in)) (> in 5000) (< in 1000)) (time-play first)
        (format t "COMEÇAR JOGO COM ~d E ~d" in first)
      ))
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

(defun player-game (node)
  (let ((list (list-possible-movements (node-board node) (node-turn-player node))))
  (if list
    (progn
      (node-print-singular node)
      (format t "Escolha a proxima posição do cavalo~%")
      (let* ( 
          (l (read-position-input "Linha")) 
          (c (read-position-input "Coluna")) 
        )
        (if (some (lambda (e) (equal e (list l c))) list)      
          (player-game (next-node-player node l c))
          (progn (format t "Não é possivel mover para a posição insira outra!~%") (player-game node))
        )
      )
    )
    (progn 
      (format t "Torno passado por não haver jogada possivel!") 
      ;JOGADA DO ADVERSÁRIO
    )
  )
)
)

(defun next-node-player (node l c)
  (let ((result (move (node-board node) l c (node-turn-player node))))
    (if result
      (let* ((score-one (if (= (node-turn-player node) *player-one*) 
                (+ (movement-result-score result) (node-score-one node))))
              (score-two (if (= (node-turn-player node) *player-two*) 
                (+ (movement-result-score result) (node-score-two node))))
            )
        (if (= (movement-result-score result) (reverse-digits (movement-result-score result)))
          (double-n (create-node score-one score-two *player-one* (movement-result-board result) node))  
          (create-node score-one score-two *player-one* (movement-result-board result) node)
        )
      )
      (progn (format t "Movimento não é possivel, escolha outro!") (player-game node))
     )
  )
)

;(concatenate 'string (write-to-string numero1) (write-to-string numero2))
;(player-game (test-node1))

(defun double-n-message (n)
"Mostra as opções de numeros a retirar"
  (progn
    (format t "Escolheu um número duplo, escolha outro para apagar:")
    (format t "~%~d~%~%" n)
  )
)

(defun double-n (node)
  (progn
    (node-print node)
    (double-n-message (mapcar (lambda (x) (cell (node-board node) (car x) (cadr x))) (find-double-digit-numbers (node-board node))))
     (let ((in (read)))
      (if (and (= in (reverse-digits in))  (remove-double-number (node-board node) in))
        (progn (format t "Número ~d retirado ~%~%" in) (node-change-board node (remove-double-number (node-board node) in)))
        (progn (format t "Número ~d invalido, escolha outro~%" in) (double-n node))
      )
     )
  )
)



