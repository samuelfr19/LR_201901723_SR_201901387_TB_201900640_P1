(defparameter *player-one* -1)
(defparameter *player-two* -2)

(load "./lisp/jogo.l")
(load "./lisp/algoritmo.l")


;;  processo:
;;  start > user - pc ou pc - pc > quem joga primeiro > quanto tempo por jogada 1000 - 5000 ms
;;  loop: realiza jogada > print numeros duplos se for o caso para escolher qual quer remover (se for o pc escolhe o maior e nao da print)
;______________________________________________________________________________________________________________________
;  IMPORTANTE @SHANUEL quando for feito um moviemnto e o valor for (= 0 (mod valor 11)) mostrar os numeros por apagar
;  (mapcar (lambda (x) (cell board (car x) (cadr x))) (find-double-digit-numbers <board>))
;  dar print dos numeros duplos que ainda estao na tabela (11 33 55 99)
;  introduza um destes numeros para remover:
;  opcao 1: nao e' um numero duplo           -     (= <valor> (reverse-digits <valor>)) -> nil
;  opcao 2: nao esta na tabela               -     (remove-double-number <board> <valor>) -> nil
;  opcao 3: tudo valido e numero removido    -     <board-sem-<valor>>


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

(defun test-node1 ()
  (create-node 17 84 *player-one* 
    '(
      (92  42  31  96  7   -1  80  3   52  72 )
      (97  76  30  nil 87  89  93  19  0   78 )
      (46  88  61  79  60  57  32  50  83  47 )
      (9   25  36  49  21  74  85  55  54  4  )
      (68  82  34  73  13  16  22  14  27  64 )
      (59  29  39  28  94  43  51  95  6   10 )
      (20  33  99  81  5   69  67  2   58  37 )
      (15  98  8   53  18  90  63  26  62  35 )
      (nil 86  40  66  65  23  77  44  1   4 )
      (70  75  41  91  -2  12  45  24  56  38 )
    )
  )
)

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
        (progn (format t "Número ~d retirado ~%~%" in) (print-board (remove-double-number (node-board node) in)))
        (progn (format t "Número ~d invalido, escolha outro~%" in) (double-n node))
      )
     )
  )
)


(double-n  (test-node1))

