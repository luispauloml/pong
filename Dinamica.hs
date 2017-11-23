module Dinamica where

import Control.Lens
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

import Global
import Tipos
import Colisao

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
                        
-- # UI # ---------------------------------------------------------------
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
