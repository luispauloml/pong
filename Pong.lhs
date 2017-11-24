%options ghci -fglasgow-exts

\documentclass[12pt,a4paper,oneside]{article}
\usepackage[a4paper]{geometry}
\usepackage[brazil]{babel}
\usepackage{indentfirst}
\geometry{verbose,tmargin=2.5cm,bmargin=2.5cm,lmargin=3cm,rmargin=3cm}

%---------------------------------------------------------------

\usepackage{hyperref}
\hypersetup{colorlinks=true}

%---------------------------------------------------------------

%include ../lhs2TeX.sty
%include ../lhs2TeX.fmt
%include polycode.fmt

\begin{document}

%---------------------------------------------------------------
%---------------------------------------------------------------

\title{\texttt{Pong}: simples implementa\c c\~ao em Haskell}
\author{Luis Paulo Lima}
\date{\today}
\maketitle
\tableofcontents{}

%---------------------------------------------------------------
%---------------------------------------------------------------

\section{Introdu\c cao}

O prop\'osito deste trabalho \'e entender como funciona programa\c cao liter\'aria em Haskell e praticar algumas funcionalidades da pr\'opria linguagem.
Dentre elas, usei principalmente 
\href{http://hackage.haskell.org/package/lens}{@lens@} e a m\^onada @Maybe@. 
Toda a implementa\c c\~ ao \' e baseada no pacote
\href{http://hackage.haskell.org/package/gloss}{@gloss@}.
Todas as a\c c\~ oes I/O s\~ ao realizadas por ele, sendo que @IO@ s\'o foi usada durante o desenvolvimento para \emph{debugging}, quando necess\'ario.


\begin{code}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens
import Data.Maybe
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
    
\end{code}


Seguindo a ideia da programa\c c\~ao liter\'aria, eu come\c co o desenvolvimento
criando os tipos que ser\~ao usados e as lentes para operar em alguns deles.
Depois, algumas constante globais que ser\~ao usadas ao decorrer da implementa\c c\~ao,
e o estado inicial do jogo e o procedimento de incio da partida.

%---------------------------------------------------------------
%---------------------------------------------------------------

\section{Tipos}

Os tipos b\'asicos s\~ao usados para representar a dire\c c\~ao de movimento dos jogadores e posi\c c\~oes relativas \`a tela.

\begin{code}
data Direcao = Cima | Baixo | Parado deriving (Eq, Show)
data Lado = Topo | Base | Dir | Esq deriving (Eq, Show)

\end{code}
Quanto ao universo do jogo, s\~ao usados registros (\emph{records}) para armazenar o estado da bola e dos jogadores.

\begin{code}
data Jogador = Jogador
             { _posY   :: Float        -- Posicao vertical da barra
             , _dirMov :: Direcao      -- Direcao atual do movimento
             , _pts    :: Int          -- Pontuacao do jogador
             } deriving (Eq, Show)
             
data Bola = Bola
          { _veloc   :: Float          -- Magnitude da velocdiade
          , _angulo  :: Float          -- Angulo do movimento da bola
          , _posXY   :: (Float,Float)  -- Posicao XY da bola
          } deriving (Eq, Show)

\end{code}

Para o jogo como um todo, al\'em de armazenar dois jogadores mais a bola, alguns dados adicionais s\~ao necess\'arios:
\begin{enumerate}
    \item Onde ocorreu o \'ultimo toque: para evitar o \emph{bug} de uma bola muito lenta ficar rebatendo idefinidamente caso estiver muito pr\'oxima de um objeto.
    
    \item De que lado ocorreu o \'ultimo ponto: usado para determinar de que lado a bola come\c ca no ponto seguinte.
    
    \item Contador de tempo: para atrasar o recome\c co quando houver ponto.
    
\end{enumerate}

\begin{code}
data Jogo = Jogo
          { _player1     :: Jogador  -- Jogador 1
          , _player2     :: Jogador  -- Jogador 2
          , _pong        :: Bola     -- Bola do jogo
          , _ultimoToque :: Lado     -- Armazena lado do ultimo toque
          , _ultimoPonto :: Lado     -- Lado onde foi feito o ultimo ponto
          , _atrasoPonto :: Float    -- Contador de tempo para atraso
          } deriving (Eq, Show)        

\end{code}            

%---------------------------------------------------------------

Perecebe-se que cada campo do registro come\c ca por um tra\c co inferior (@_@).
Eles uma exig\^encia para facilitar a cria\c c\~ao das lentes: 

\begin{code}

makeLenses ''Jogador
makeLenses ''Bola
makeLenses ''Jogo

\end{code}

A fun\c c\~ao @makeLenses@ usa metaprograma\c c\~ao para criar as lentes dos registros, a qual j\'a foi habilitada pela extens\~ao @TemplateHaskell@ no cabe\c calho.
Cada campo definido num registro torna-se por se s\'o uma fun\c c\~ao de acesso aos dados armazenados:

\begin{spec} _campo :: a -> b \end{spec}

Quando a lente \'e criada, uma nova fun\c c\~ao \'e criada para ser usada com @over@, @set@ ou @view@, do pacote @lens@:

\begin{spec} campo :: Functor f => (b -> f b) -> a -> f a \end{spec}

Por exemplo, @set@ \'e usada para sobrescrever algum dado num registro.
Se usada para alterar a bola (@_pong@) no estado geral do jogo (@Jogo@), tem-se:


< set pont :: Bola -> Jogo -> Jogo
%\eval{:t set pong}.

%---------------------------------------------------------------
%---------------------------------------------------------------

\section{Constantes globais}

Por se tratar de um jogo gr\'afico, definir algumas constantes globais torna o programa mais leg\'ivel:
em vez de passar v\'arios argumentos para v\'arias fun\c c\~oes que dependem da geometria dos objetos na tela, \'e mais f\'acil apenas invocar o valor global.
H\'a tr\^es constantes b\'asicas, e outras cinco definidas em fun\c c\~ao dessas primeiras.

%---------------------------------------------------------------

\subsection{Constates b\'asicas}

As tr\^es principais s\~ao:
\begin{enumerate}

\item Taxa de quadros por segundo para atualiza\c c\~ao da tela (Hz).
\begin{code}
fps          :: Int
fps          = 60         

\end{code}

\item Tempo de atraso entre os pontos (s).
\begin{code}
delayInit    :: Float
delayInit    = 1

\end{code}

\item Dimens\~oes $(x,y)$ da janela (px).
\begin{code}
tamJanela    :: (Int, Int)
tamJanela    = (800,600)  

\end{code}
\end{enumerate}

%---------------------------------------------------------------

\subsection{Consates derivadas}

As demais s\~ao definidas todas elas em fun\c c\~ao do tamanho da janela,
e entre si mesmas:
\begin{enumerate}

\item Raio da bola (px).

\begin{code}
raioBola     :: Float 
raioBola     = 0.02 * (fromIntegral $ snd tamJanela)

\end{code}

\item Largura do jogador (px).

\begin{code}
ladoJogador  :: Float 
ladoJogador  = raioBola

\end{code}

\item Comprimento do jogador (px).

\begin{code}
compJogador  :: Float 
compJogador  = 10 * ladoJogador

\end{code}

\item Passo do movimento do jogador a cada quadro (px).

\begin{code}
passoJogador :: Float 
passoJogador = 0.2 * compJogador

\end{code}

\item Limites da janela a partir do centro (px):
superior, inferior, esquerda e direita.

\begin{code}
limJanela    :: (Float, Float, Float, Float) 
limJanela    = (,,,)
                -- Fundo direito
                ((fromIntegral   (fst tamJanela) ) / 2)   
                -- Fundo esquerdo
                ((fromIntegral (-(fst tamJanela))) / 2)   
                -- Superior e ponto na esquerda
                ((fromIntegral   (snd tamJanela) ) / 2)   
                -- Inferior e ponto na direita
                ((fromIntegral (-(snd tamJanela))) / 2) 
               
\end{code}
\end{enumerate}

%---------------------------------------------------------------
%---------------------------------------------------------------

\section{Estado inicial e fun\c c\~oes principais}

J\'a tendo os tipos e constantes definidas, \'e poss\'ivel definir o estado incial do jogo: a bola, os jogadores e o universo por completo.

\begin{code}
jogador0 = Jogador { _posY   = 0
                   , _dirMov = Parado
                   , _pts    = 0 }
                   
bola0    = Bola    { _angulo = 45
                   , _posXY  = (0,0)
                   , _veloc  = 7 }
                   
jogo0    = Jogo    { _player1     = jogador0
                   , _player2     = jogador0
                   , _pong        = bola0
                   , _ultimoToque = Topo
                   , _ultimoPonto = Esq
                   , _atrasoPonto = delayInit }

\end{code}

O jogo \'e baseado na fun\c c\~ao @playIO@ do @gloss@. Ela exige a descri\c c\~ao da janela,
cor de fundo,
taxa atualiza\c c\~ao da tela,
uma \'unica constante representando o universo do jogo,
uma fun\c c\~ao para renderizar o jogo,
outra para lidar com eventos do jogador,
e outra para adiatnar o estado do jogo segundo a passagem do tempo.

\begin{code}
jogarIO j = playIO 
    (InWindow "Pong" tamJanela (40,40))  -- Janela do jogo
    black           -- Cor de fundo                      
    fps             -- Taxa FPS
    j               -- Entrada (tipo Jogo)
    renderizar      -- Converto o mundo para Picture
    eventoTecla     -- Lidar com eventos IO
    passoGeral      -- Lida com a passagem do tempo

\end{code}

Na fun\c c\~ao principal @main@, a inicializa\c c\~ao do jogo come\c ca por definir um \^angulo aleat\'orio para o movimento da bola.
Percebe-se que em @bola0@, a posi\c c\~ao inicial \'e $(0,0)$,
que corresponde ao centro da tela.
Diferente do @Pong@ original, ou at\'e mesmo do @Ping Pong@ anterior a ele,
nesta implementa\c c\~ao, a bola sempre partir\'a do centro da tela,
num \^angulo aleat\'orio, come\c cando pela direita.

\begin{code}
main :: IO ()
main = let f x = if odd x then x else (-x)
       in do a <- randomRIO (5, 45) :: IO Int
             s <- newStdGen
             jogarIO $ set (pong . angulo) (fromIntegral . f $ a) 
                     $ jogo0

\end{code}

\subsection{Fun\c c\~oes principais}

Por exig\^encia de @playIO@, @renderizar@, @eventoTecla@ e @passoGeral@ devem ser tais que:

\begin{spec}
j            :: a
renderizar   :: a -> IO Picture
eventoTecla  :: Event -> a -> IO a
passoGeral   :: Float -> a -> IO a
\end{spec}
E j\'a defin\'i-las ajuda no processo de desenvolvimento do programa:
come\c cando pelas fun\c c\~oes de mais alta ordem e partindo para a elementares,
na medida do necess\'ario.

\subsubsection{Desenhar na tela}

A fun\c c\~ao @renderizar@ usa @_player1@ e @_player2@ de @Jogo@ para desenh\'a-los na tela, mais @_pong@ para desenhar a bola.

\begin{code}
renderizar :: Jogo -> IO Picture
renderizar jogo =
  let js  = zip   [Esq,Dir] [_player1 jogo, _player2 jogo]
      b   = _pong jogo
  in  return . pictures $ (renderBola b) : (map renderJogador js)

\end{code}
Acabamos de definir, portanto, que @_player1@ fica do lado esquerdo da tela,
e @_player2@ fica do lado direto.

\subsubsection{Entrada humana}\label{sec:entrada}

@gloss@ define o tipo @Event@ para encapsular todas as poss\'iveis a\c c\~oes do usu\'ario.
No caso deste jogo, fica definido que:
\begin{enumerate}
    \item As teclas @W@ e @S@ movimentam o jogador \`a esquerda da tela (@_player1@).
    \item As teclas $\uparrow$ e $\downarrow$ movimentam o jogador \`a direita da tela (@_player2@).
    \item Qualquer outra entrada (telca, cliques, movimento do mouse) n\~ao realizam nenhuma a\c c\~ao.
\end{enumerate}
Nesse sentido, fica clara a conveni\^encia de se usar @Maybe@ para tratar os eventos de tecla.
Como definimos que @Jogador@ carrega sua dire\c c\~ao do movimento, @_dirMov@,
basta que este valor seja alterado segundo a entrada do jogador.

\begin{code}
eventoTecla :: Event -> Jogo -> IO Jogo
eventoTecla evento jogo = 
  let tecla = lerTecla evento 
  in if isNothing tecla 
     then return jogo
     else let (p,d) = fromJust tecla
          in do return $ set (p . dirMov) d jogo

\end{code}

Daqui tamb\'em j\'a fica claro que a fun\c c\~ao elementar @lerTecla@ dever\'a ter como sa\'ida uma dupla contendo a lente do jogador (@player1@ ou @player2@),
e a traduzir a tecla pressiona num valor do tipo @Direcao@.

\subsubsection{Itera\c c\~ao do universo}
A \'ultima fun\c c\~ao principal \'e respons\'avel por atualizar o jogo
na seguinte sequ\^encia:
\begin{enumerate}
    \item Conta o tempo de atraso de cada ponto at\'e zerar;
    \item Detecta e age se a bola est\'a em posi\c c\~ao de colis\~ao;
    \item Move os jogadores para cima ou para baixo;
    \item Move a bola;
    \item Detecta se h\'a ponto (bola fora).
\end{enumerate}

\begin{code}
passoGeral :: Float -> Jogo -> IO Jogo
passoGeral dt jogo = if _atrasoPonto jogo > 0
                     then return $ over atrasoPonto (+(-dt)) jogo
                     else (detectColisao jogo) 
                          >>= (moverJogadores passoJogador) 
                          >>= moverBola
                          >>= bolaFora

\end{code}

%---------------------------------------------------------------
%---------------------------------------------------------------

\section{Renderizando o jogo}

Para cada jogador, usa-se @renderJogador@, que admite o uma dupla contendo
um lado da tela e o pr\'oprio jogador,
o qual carrega consigo sua posi\c c\~ao vertcial.
O desenho \'e um ret\^angulo branco, cujo tamanho segue as constantes globais.
\begin{code}
renderJogador :: (Lado,Jogador) -> Picture
renderJogador (l,j) = 
    let  p  = _posY j
         b  = color white $ rectangleSolid ladoJogador compJogador
         x  = (/2) . fromIntegral $ snd tamJanela
    in case l of  Dir  -> translate ( x - (ladoJogador/2)) p b
                  Esq  -> translate (-x + (ladoJogador/2)) p b

\end{code}

Um c\'irculo branco representa a bola. @renderBola@ admite apenas @Bola@, que j\'a carrega em si mesma sua posi\c c\~ao $(x,y)$ na tela.

\begin{code}
renderBola :: Bola -> Picture
renderBola b =
    let (x,y) = _posXY b 
    in color white  $ translate x y 
                    $ circleSolid raioBola

\end{code}

%---------------------------------------------------------------
%---------------------------------------------------------------

\section{Entrada das teclas e movimento}

Seguindo a documenta\c c\~ao de @gloss@,
e de acordo com o que definimos na Se\c{c}\~{a}o~\ref{sec:entrada}
pode-se escrever a fun\c c\~ao que ir\'a receber os eventos enviados \`a @eventoTecla@,
que v\^em das fun\c c\~oes internas de @playIO@.

\begin{code}
lerTecla (EventKey (Char c) e _ _)
    | c == 'w' && e == Down = Just (player1, Cima)
    | c == 'w' && e == Up   = Just (player1, Parado)
    | c == 's' && e == Down = Just (player1, Baixo)
    | c == 's' && e == Up   = Just (player1, Parado)
lerTecla (EventKey (SpecialKey s) e _ _)
    | s == KeyUp   && e == Down = Just (player2, Cima)
    | s == KeyUp   && e == Up   = Just (player2, Parado)
    | s == KeyDown && e == Down = Just (player2, Baixo)
    | s == KeyDown && e == Up   = Just (player2, Parado)
lerTecla _ = Nothing

\end{code}

Nota-se que n\~ao h\'a anota\c c\~ao de tipo para @lerTecla@.
Isso acontece porque ela retorna @player1@ e @player2@ e, sendo assim, 
seu tipo deveria ser
\begin{spec}
lerTecla :: Event -> Maybe (Lens' Jogo Jogador,Direcao)
\end{spec}
Por\'em, @Lens'@ dentro de @Maybe@ exige polimorfismo impredicativo,
o que ainda n\~ao \'e suportado pelo GHC,
mesmo se us\'assemos a extens\~ao @RankedNTypes@.
Portanto, conv\'em deixar que o tipo seja automaticamente inferido pelo compilador.
Isso resulta em:

\begin{spec}
lerTecla :: forall {f :: * -> * }
          . Functor f
         => Event 
          -> Maybe ((Jogador -> f Jogador) -> Jogo -> f Jogo, Direcao )
\end{spec}
Por este tipo n\~ao ser muito ``leg\'ivel'', conv\'em n\~ao ser anotado.


\subsection{Movimenta\c c\~ao}

O movimento dos desenhos dos jogadores na tela
\'e realizado usando-se o valor @_dirMov@ presente no registro do @Jogador@,
dentro de @_palyer1@ e @_player2@ do registro @Jogo@.
Ele \'e restringido por @limJanela@ e por @compJogador@.
Apesar de o passo do movimento ser tamb\'em uma constante global,
ele \'e passado para a fun\c c\~ao como um argumento para facilitar \emph{debugging}.

\begin{code}
moverJogadores :: Float -> Jogo -> IO Jogo
moverJogadores h jogo = return $ over player1 (moverJ h) 
                               $ over player2 (moverJ h) jogo
  where moverJ h j = case (_dirMov j) of
          Cima  -> if (_posY j) + compJogador / 2 >= view _3 limJanela
                   then set posY 
                            ((view _3 limJanela) - compJogador/2) j
                   else over posY (+h) j
          Baixo -> if (_posY j) - compJogador / 2 <= view _4 limJanela
                   then set posY 
                            ((view _4 limJanela) + compJogador/2) j
                   else over posY (+(-h)) j
          _     -> j

\end{code}

Quanto \`a bola, os dados necess\'arios para sua movimenta\c c\~ao tamb\'em est\~ao contidos nela mesma.
N\~ao h\'a a\c c\~ao alguma dos jogadores que a fa\c a mudar de dire\c c\~ao ou sentido:
isto deve acontecer apenas em caso de colis\~ao.
Portanto, basta multiplicar @_veloc@ pela varia\c c\~ao do tempo, que vem de @gloss@ e considerar o \^angulo do movimento contido em @_angulo@.

\begin{code}
moverBola :: Jogo -> IO Jogo
moverBola jogo = return $ over (pong . posXY) f jogo
  where v = view (pong . veloc) jogo
        a = view (pong . angulo) jogo
        f (x,y) = ( x + v * (cos (a * pi / 180) )
                  , y + v * (sin (a * pi / 180) ) )

\end{code}

\section{Pontua\c c\~ao}

A condi\c c\~ao para que haja ponto \'e que a bola toque o limite de ponto
definido nas constantes globais, na esquerda ou na direita.
Se a bola sair pelo lado $l$, o procedimento seguido \'e o seguinte:
\begin{enumerate}
    \item Definir que ultimo ponto foi no lado $l$;
    
    \item Definir um \^angulo alet'\'orio para lan\c camento da bola:
    \begin{enumerate}
        \item Se @l == Esq@, entre $-45^\circ{}$ e $+45^\circ{}$;
        \item Se @l == Dir@, entre $135^\circ{}$ e $225^\circ{}$.
    \end{enumerate}

    \item Definir que o \'ultimo toque foi numa posi\c c\~ao ``nula'' (@Topo@);
    \item Posicionar ambos os jogadores na posi\c c\~ao central;
    
    \item Incrementar a pontua\c c\~ao do jogador do lado oposto:
    \begin{enumerate}
        \item Se @l == Esq@, incrimenta em @_player2@;
        \item Se @l == Dir@, incrementa em @_player1@.
    \end{enumerate}

    \item Resetar o contador de atraso para @delayInit@;
    \item Posicionar a bola na origem da tela, $(0,0)$.
\end{enumerate}

\begin{code}
bolaFora :: Jogo -> IO Jogo
bolaFora jogo =
  let (x,y) = view (pong . posXY) jogo
      f d x = if d == Dir
              then if odd x then x          else (-x)
              else if odd x then (180 - x)  else (180 + x)
      pontoReset l a j = set (pong . posXY) (0,0)
                       $ set atrasoPonto delayInit
                       $ over (l . pts) (+1)
                       $ set (player1 . posY) 0
                       $ set (player2 . posY) 0
                       $ set ultimoToque Topo
                       $ (\j' -> if _ultimoPonto j' == Dir 
                                 then set (pong . angulo)
                                      (fromIntegral . (f Dir) $ a) 
                                      j'
                                 else set (pong . angulo) 
                                      (fromIntegral . (f Esq) $ a) 
                                      j' )
                       $ (\j' -> if x > 0 
                                 then set ultimoPonto Dir j'
                                 else set ultimoPonto Esq j' ) j
  in do s <- randomRIO (5, 45) :: IO Int
        if x > view _3 limJanela 
        then return $ pontoReset player1 s jogo 
        else if x < view _4 limJanela 
             then return $ pontoReset player2 s jogo
             else return $ jogo

\end{code}

\begin{code}
detectColisao :: Jogo -> IO Jogo
detectColisao jogo
    | (y - raioBola)               < view (_4) limJanela      
    = atualiza Base jogo
    | (y + raioBola)               > view (_3) limJanela      
    = atualiza Topo jogo
    | (x + raioBola + ladoJogador) > view (_3) limJanela && c 
    = atualiza Dir  jogo
    | (x - raioBola - ladoJogador) < view (_4) limJanela && c 
    = atualiza Esq  jogo
    | otherwise = return $ jogo
      where c     = contatoJB jogo
            (x,y) = view (pong . posXY) jogo
            atualiza d j = if d == _ultimoToque j then return j
                           else return
                                $ set ultimoToque d
                                $ over (pong . angulo) (refletir d) j

-- Contato entre jogador e bola
contatoJB :: Jogo ->  Bool
contatoJB jogo = 
  if ((xB < 0) && (abs (yB - yJ1)) < (compJogador/2)) ||
     ((xB > 0) && (abs (yB - yJ2)) < (compJogador/2))
  then True else False
    where yJ1  = view (player1 . posY) jogo
          yJ2  = view (player2 . posY) jogo
          lEsq = view _4 limJanela
          lDir = view _3 limJanela
          (xB,yB)  = view (pong . posXY) jogo

-- Mudança de ângulo de uma bola devido a colisão
refletir :: Lado -> Float -> Float
refletir l a
    | l == Topo = worker a 270
    | l == Base = worker a 90
    | l == Esq  = worker a 0
    | l == Dir  = worker a 180
      where worker t n = (n + 90) - (t - (n + 90))
\end{code}

\end{document}