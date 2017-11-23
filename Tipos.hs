{-# LANGUAGE TemplateHaskell #-}

module Tipos where

import Control.Lens

-- Direção do movimento da barra do jogador
data Direcao = Cima | Baixo | Parado deriving (Eq, Show)

-- Lados da tela
data Lado = Topo | Base | Dir | Esq deriving (Eq, Show)

data Jogador = Jogador
             { _posY   :: Float    -- posicao vertical da barra
             , _dirMov :: Direcao  -- direcao atual do movimento
             , _pts    :: Int      -- pontuacao do jogador
             } deriving (Eq)

instance Show Jogador where
    show (Jogador { _posY     = y
                  , _dirMov   = d 
                  , _pts      = p }) = "P {" ++ show y ++ ", "
                                             ++ show d ++ ", "
                                             ++ show p ++ "}"

data Bola = Bola
          { _veloc   :: Float         -- Magnitude da velocdiade
          , _angulo  :: Float         -- angulo do movimento da bola
          , _posXY   :: (Float,Float) -- Posição XY da bola
          } deriving (Eq)

instance Show Bola where
    show (Bola { _veloc   = m
               , _angulo  = a
               , _posXY   = v }) = "B {"  ++ show m ++ ", "
                                          ++ show a ++ ", "
                                          ++ show v ++ "}"
                                          
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
