module Colisao where

import Control.Lens
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import Global
import Tipos

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
