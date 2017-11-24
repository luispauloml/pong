{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens
import Data.Maybe
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
    
data Direcao = Cima | Baixo | Parado deriving (Eq, Show)
data Lado = Topo | Base | Dir | Esq deriving (Eq, Show)

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

data Jogo = Jogo
          { _player1     :: Jogador  -- Jogador 1
          , _player2     :: Jogador  -- Jogador 2
          , _pong        :: Bola     -- Bola do jogo
          , _ultimoToque :: Lado     -- Armazena lado do ultimo toque
          , _ultimoPonto :: Lado     -- Lado onde foi feito o ultimo ponto
          , _atrasoPonto :: Float    -- Contador de tempo para atraso
          } deriving (Eq, Show)        

            
makeLenses ''Jogador
makeLenses ''Bola
makeLenses ''Jogo

fps          :: Int
fps          = 60         

delayInit    :: Float
delayInit    = 1

tamJanela    :: (Int, Int)
tamJanela    = (800,600)  

raioBola     :: Float 
raioBola     = 0.02 * (fromIntegral $ snd tamJanela)

ladoJogador  :: Float 
ladoJogador  = raioBola

compJogador  :: Float 
compJogador  = 10 * ladoJogador

passoJogador :: Float 
passoJogador = 0.2 * compJogador

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

jogarIO j = playIO 
    (InWindow "Pong" tamJanela (40,40))  -- Janela do jogo
    black           -- Cor de fundo                      
    fps             -- Taxa FPS
    j               -- Entrada (tipo Jogo)
    renderizar      -- Converto o mundo para Picture
    eventoTecla     -- Lidar com eventos IO
    passoGeral      -- Lida com a passagem do tempo

main :: IO ()
main = let f x = if odd x then x else (-x)
       in do a <- randomRIO (5, 45) :: IO Int
             s <- newStdGen
             jogarIO $ set (pong . angulo) (fromIntegral . f $ a) 
                     $ jogo0

renderizar :: Jogo -> IO Picture
renderizar jogo =
  let js  = zip   [Esq,Dir] [_player1 jogo, _player2 jogo]
      b   = _pong jogo
  in  return . pictures $ (renderBola b) : (map renderJogador js)

eventoTecla :: Event -> Jogo -> IO Jogo
eventoTecla evento jogo = 
  let tecla = lerTecla evento 
  in if isNothing tecla 
     then return jogo
     else let (p,d) = fromJust tecla
          in do return $ set (p . dirMov) d jogo

passoGeral :: Float -> Jogo -> IO Jogo
passoGeral dt jogo = if _atrasoPonto jogo > 0
                     then return $ over atrasoPonto (+(-dt)) jogo
                     else (detectColisao jogo) 
                          >>= (moverJogadores passoJogador) 
                          >>= moverBola
                          >>= bolaFora

renderJogador :: (Lado,Jogador) -> Picture
renderJogador (l,j) = 
    let  p  = _posY j
         b  = color white $ rectangleSolid ladoJogador compJogador
         x  = (/2) . fromIntegral $ snd tamJanela
    in case l of  Dir  -> translate ( x - (ladoJogador/2)) p b
                  Esq  -> translate (-x + (ladoJogador/2)) p b

renderBola :: Bola -> Picture
renderBola b =
    let (x,y) = _posXY b 
    in color white  $ translate x y 
                    $ circleSolid raioBola

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

moverBola :: Jogo -> IO Jogo
moverBola jogo = return $ over (pong . posXY) f jogo
  where v = view (pong . veloc) jogo
        a = view (pong . angulo) jogo
        f (x,y) = ( x + v * (cos (a * pi / 180) )
                  , y + v * (sin (a * pi / 180) ) )

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
