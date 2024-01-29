To run:
clisp -repl ./lisp/interact.l

Algo que não referi e que pode suscitar algumas dúvidas, que é a situação em que não é possível mover o cavalo. Nesse caso deverão retornar o mesmo estado que receberam, ou seja, o tabuleiro e os pontos irão manter-se idênticos ao que foi recebido na função jogar.


1. O objetivo passa por realizar um campeonato entre os programas de cada grupo. Para participar, será necessário preparar o código fonte da seguinte forma:

  a) ao contrário do que entregarem para o projeto e para facilitar a gestão do código dos vários grupos, peço que coloquem todas as funções que precisam em apenas um ficheiro com a designação p<grupo> em que <grupo>::=<número do grupo presente no ficheiro que contém as avaliações>.

  b) o ficheiro que irá conter todo o código deve iniciar com a instrução para definir um package e cujo nome terá a mesma designação do nome do ficheiro: p<grupo> em que <grupo>::=<número do grupo presente no ficheiro que contém as avaliações>.

  Exemplo para um grupo com o número 55:

  (in-package :p55)

 

2. Necessário que seja definida uma função com o nome jogar com dois parâmetros de entrada:

    2.1. o estado que será representado por uma lista com (<tabuleiro> <pontos de cada jogador>)

        i) O tabuleiro será igual ao utilizado para o projeto, sendo composto por uma lista com 10 listas de 10 elementos, ou seja, uma matriz 10x10.

        O tabuleiro irá respeitar as regras indicadas no enunciado do projeto na fase 2, nomeadamente os valores das células poderão ser os seguintes:

        * De 0 a 99 significa que a casa ainda não foi visitada e são os pontos que podem ser ganhos pelo jogador;
        * NIL significa que a casa já foi visitada;
        * -1 significa que o cavalo branco (Jogador 1) se encontra nessa casa;
        * -2 significa que o cavalo preto (Jogador 2) se encontra nessa casa.

        
        

        ii) Os pontos de cada jogador é representado por uma lista com 2 números (nº de pontos do Jogador 1 e nº de pontos do Jogador 2).

    2.2. tempo máximo para a jogada em milissegundos.

 

        O output da função deverá ser o novo estado, que à semelhança do que foi recebido como parâmetro de entrada, também será representado por uma lista com o tabuleiro como primeiro elemento e uma segunda lista com o número de pontos de cada jogador (atualizada com os pontos da casa visitada). Deverão de ter em atenção a casa visitada de modo a aplicar a regra do número duplo/simétrico.

 

3. De salientar que nesta versão do projeto não devem manter a escrita em ficheiro, ou outras operações de apresentação de resultados estatísticos, visto que o objetivo passa por ter a função jogar a determinar e devolver a melhor jogada no tempo máximo passado como argumento, ou utilizando outra estratégia que considerem mais adequada. 

Os milissegundos definidos para a jogada será desde que o projeto do Campeonato irá chamar a vossa função jogar até que seja retornado o novo estado, pelo que devem ter em atenção que caso estejam a verificar a paragem do algoritmo pelo tempo máximo poderão perder o jogo, visto que no algoritmo ainda terão de ser executadas as restantes chamadas que estão no stack para retornar o valor para o algoritmo. Possíveis soluções que poderão implementar serão:

aplicar alguma folga para que não esperem até esse tempo máximo, para prevenir que não perdem o jogo pelo tempo da jogada exceder o limite.
definir uma profundidade máxima que sabem que o algoritmo irá sempre conseguir determinar a jogada nesse tempo máximo.
Para o campeonato o tempo máximo que será utilizado para fazer a jogada e será passado por argumento para a vossa função e que enviarei sempre será de 5000 milissegundos.

 

4. Todos os participantes neste campeonato recebem um bónus na nota final do projeto 2, com destaque para os dois primeiros lugares:

a) 3 valores para o grupo vencedor.
b) 2 valores para o grupo que acabar na 2ª posição.
c) 1 valor para os restantes grupos que se inscreverem no campeonato e em que o programa esteja corretamente estruturado para participar.

Cada grupo que participe jogará com todos os grupos duas vezes, em que num jogo jogará primeiro e no outro jogo será o adversário a jogar primeiro.

 

5. O ficheiro de código para o campeonato, poderá ser submetido através do Moodle numa zona específica para esse efeito, disponível a partir do dia 27 de Janeiro até ao dia 30 de Janeiro às 23h.

 

De salientar alguns aspetos que podem suscitar algumas dúvidas:

a)    O vosso código irá sempre colocar o cavalo com o valor -1 no tabuleiro, cabe depois ao campeonato ir trocando os valores do tabuleiro antes de enviar o estado para a vossa função jogar. Ou seja, para o vosso código serão sempre o jogador 1 e o campeonato é que irá gerir alternadamente a troca de valores no tabuleiro para que o jogador seguinte tenha novamente o cavalo a -1 e o jogador anterior o cavalo a -2.

b)    O número de pontos que vocês terão de ter em conta para a vossa jogada será sempre o primeiro número, sendo o segundo o número de pontos do adversário.

c)    A submissão para o campeonato terá uma zona à parte da que fizeram para o projeto, sendo que para o campeonato apenas precisam submeter o ficheiro que pedi com a função jogar e todas as funções que permitam que essa função funcione para devolver o novo estado (algoritmo, funções auxiliares, manipulação do tabuleiro, etc).

Nota: Vou anexar a esta mensagem um ficheiro que permitirá servir de exemplo da criação do package e que permitirá compreender melhor o input e output da função jogar para o campeonato.

Exemplo de como executar no listener a função jogar que está no ficheiro fornecido:

(p55::jogar (p55::estado-exemplo) 5000)



Cumprimentos,

Filipe Mariano