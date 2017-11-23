module Graficos where

import Graphics.Gloss

import Global
import Tipos

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
