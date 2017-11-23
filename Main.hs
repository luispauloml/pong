module Main where

import Control.Lens
import Data.Maybe
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

import Colisao
import Dinamica
import Global
import Graficos
import Tipos

                        
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

-- # MAIN # -----------------------------------------------------------------
main = let f x = if odd x then x else (-x)
       in do a <- randomRIO (5, 45) :: IO Int
             s <- newStdGen
             jogarIO $ set (pong . angulo) (fromIntegral . f $ a) jogo0

jogarIO j = playIO 
{-janela-}  (InWindow "Pong" tamJanela (40,40))
{-cor-}     black
{-fps-}     (round fps)
{-mundo-}   j
{-render-}  renderizar
{-evento-}  eventoTecla
{-passo-}   passoGeral

-- Passo do jogo
passoGeral :: Float -> Jogo -> IO Jogo
passoGeral dt jogo = if _atrasoPonto jogo > 0
                     then return $ over atrasoPonto (+(-dt)) jogo
                     else (detectColisao jogo) >>= (moverJogadores passoJogador) 
                                               >>= moverBola
                                               >>= bolaFora

-- Lidar com eventos de tecla
eventoTecla :: Event -> Jogo -> IO Jogo
eventoTecla evento jogo = 
  let tecla = lerTecla evento 
  in if isNothing tecla 
     then return jogo
     else let (p,d) = fromJust tecla
          in do return $ set (p . dirMov) d jogo

-- Renderizar todo o mundo
renderizar :: Jogo -> IO Picture
renderizar jogo = return $ pictures $ (renderBola b) : (map renderJogador js)
  where js = zip [Esq,Dir] [_player1 jogo, _player2 jogo]
        b = _pong jogo

