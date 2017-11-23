module Global where

-- # GERAL # ---------------------------------------------------------------
fps          = 60 :: Float
delayInit    = 1 :: Float
tamJanela    = (800,600) :: (Int,Int)
raioBola     = 0.02 * (fromIntegral $ snd tamJanela) :: Float
ladoJogador  = raioBola
compJogador  = 10 * ladoJogador
passoJogador = 0.2 * compJogador

limJanela :: (Float, Float, Float, Float)
limJanela    = ( ((fromIntegral   (fst tamJanela))  / 2)   --fundo direita
               , ((fromIntegral (-(fst tamJanela))) / 2)   --fundo esquerda
               , ((fromIntegral   (snd tamJanela))  / 2)   --superior / ponto na direita
               , ((fromIntegral (-(snd tamJanela))) / 2) ) --inferior / ponto na esquerda