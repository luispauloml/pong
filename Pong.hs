{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

import Control.Lens
import Data.Maybe
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

-- Direção do movimento da barra do jogador
data Direcao = Cima | Baixo | Parado deriving (Eq, Show)

-- Lados da tela
data Lado = Topo | Base | Dir | Esq deriving (Eq, Show)

data Jogador = Jogador
             { _posY   :: Float    -- posicao vertical da barra
             , _dirMov :: Direcao  -- direcao atual do movimento
             , _pts    :: Int      -- pontuacao do jogador
             } deriving (Eq, Show)
             
data Bola = Bola
          { _veloc   :: Float         -- Magnitude da velocdiade
          , _angulo  :: Float         -- angulo do movimento da bola
          , _posXY   :: (Float,Float) -- Posição XY da bola
          } deriving (Eq, Show)

data Jogo = Jogo
            { _player1     :: Jogador -- dupla de jogadores
            , _player2     :: Jogador -- ^
            , _pong        :: Bola    -- bola do jogo
            , _ultimoToque :: Lado    -- armazena lado do útimo toque
            , _ultimoPonto :: Lado    -- lado onde foi feito o último ponto
            , _atrasoPonto :: Float   -- contador de tempo para atraso em cada ponto
            } deriving (Eq, Show)
            
makeLenses ''Jogador
makeLenses ''Bola
makeLenses ''Jogo
                                   
-- # INICIO DO JOGO # -------------------------------------------------------
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

main = let f x = if odd x then x else (-x)
       in do a <- randomRIO (5, 45) :: IO Int
             s <- newStdGen
             jogarIO $ set (pong . angulo) (fromIntegral . f $ a) jogo0

-- # GERAL # ---------------------------------------------------------------
fps          = 60 :: Float
delayInit    = 1 :: Float
tamJanela    = (800,600) :: (Int,Int)
raioBola     = 0.02 * (fromIntegral $ snd tamJanela) :: Float
ladoJogador  = raioBola
compJogador  = 10 * ladoJogador
passoJogador = 0.2 * compJogador
limJanela    = ( ((fromIntegral   (fst tamJanela))  / 2)   --fundo direita
               , ((fromIntegral (-(fst tamJanela))) / 2)   --fundo esquerda
               , ((fromIntegral   (snd tamJanela))  / 2)   --superior / ponto na direita
               , ((fromIntegral (-(snd tamJanela))) / 2) ) --inferior / ponto na esquerda

jogarIO j = playIO 
{-janela-}  (InWindow "Pong" tamJanela (40,40))
{-cor-}     black
{-fps-}     (round fps)
{-mundo-}   j
{-render-}  renderizar
{-evento-}  eventoTecla
{-passo-}   passoGeral

-- Renderizar todo o mundo
renderizar :: Jogo -> IO Picture
renderizar jogo = return $ pictures $ (renderBola b) : (map renderJogador js)
  where js = zip [Esq,Dir] [_player1 jogo, _player2 jogo]
        b = _pong    jogo

-- Lidar com eventos de tecla
eventoTecla :: Event -> Jogo -> IO Jogo
eventoTecla evento jogo = 
  let tecla = lerTecla evento 
  in if isNothing tecla 
     then return jogo
     else let (p,d) = fromJust tecla
          in do return $ set (p . dirMov) d jogo
               
-- Passo do jogo
passoGeral :: Float -> Jogo -> IO Jogo
passoGeral dt jogo = if _atrasoPonto jogo > 0
                     then return $ over atrasoPonto (+(-dt)) jogo
                     else (detectColisao jogo) >>= (moverJogadores passoJogador) 
                                               >>= moverBola
                                               >>= bolaFora


-- Mover jogadores
moverJogadores :: Float -> Jogo -> IO Jogo
moverJogadores h jogo = return $ over player1 (moverJ h) $ over player2 (moverJ h) jogo
  where moverJ h j = case (_dirMov j) 
                     of Cima  -> if (_posY j) + compJogador / 2 >= view _3 limJanela
                                 then set posY ((view _3 limJanela) - compJogador/2) j
                                 else over posY (+h) j
                        Baixo -> if (_posY j) - compJogador / 2 <= view _4 limJanela
                                 then set posY ((view _4 limJanela) + compJogador/2) j
                                 else over posY (+(-h)) j
                        _     -> j   

-- Mover o pong
moverBola :: Jogo -> IO Jogo
moverBola jogo = return $ over (pong . posXY) f jogo
  where v = view (pong . veloc) jogo
        a = view (pong . angulo) jogo
        f (x,y) = ( x + v * (cos (a * pi / 180) )
                  , y + v * (sin (a * pi / 180) ) )

-- Detectar fora (ponto)
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
                                          (fromIntegral . (f Dir) $ a) j'
                                 else set (pong . angulo) 
                                          (fromIntegral . (f Esq) $ a) j' )
                       $ (\j' -> if x > 0 
                                 then set ultimoPonto Dir j'
                                 else set ultimoPonto Esq j' ) j
  in do s <- randomRIO (5, 45) :: IO Int
        if x > view _3 limJanela 
        then return $ pontoReset player1 s jogo 
        else if x < view _4 limJanela 
             then return $ pontoReset player2 s jogo
             else return $ jogo

-- # RENDERS # --------------------------------------------------------------
-- Grafico dos jogadores
renderJogador :: (Lado,Jogador) -> Picture
renderJogador (l,j) = 
    let p = _posY j
        b = color white $ rectangleSolid ladoJogador compJogador
        x = (/2) $ fromIntegral $ snd tamJanela
    in case l of Dir -> translate ( x - (ladoJogador/2)) p b
                 Esq -> translate (-x + (ladoJogador/2)) p b

-- Gráfico da bola
renderBola :: Bola -> Picture
renderBola b = let (x,y) = _posXY b 
                in color white $ translate x y 
                               $ circleSolid raioBola

-- # COLISÃO # --------------------------------------------------------------
-- Detectar colisão
detectColisao :: Jogo -> IO Jogo
detectColisao jogo
    | (y - raioBola)               < view (_4) limJanela      = atualiza Base jogo
    | (y + raioBola)               > view (_3) limJanela      = atualiza Topo jogo
    | (x + raioBola + ladoJogador) > view (_3) limJanela && c = atualiza Dir  jogo
    | (x - raioBola - ladoJogador) < view (_4) limJanela && c = atualiza Esq  jogo
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

-- # EVENTO # ---------------------------------------------------------------
--lerTecla :: Event -> Maybe (Lens' Jogo Jogador,Direcao)
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
