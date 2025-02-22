module Pfestudos where

-- Ficha 3 -----


data Hora = H Int Int
          deriving Show


type Etapa = (Hora,Hora)

type Viagem = [Etapa]

testesViagem :: Viagem -> Bool
testesViagem ((H h1 m1, H h2 m2):t) 
     | h1 > h2 = False
     | h1 == h2 && m1 > m2 = False
     | h1 > 24 || h2 > 24 && m1 > 60 && m2 > 60 = False 
     | otherwise = True