module Pf9 where

import System.Random 

-- gerar datas de nascimentos aleatorios d/m/a

dataNasc :: IO (Int,Int,Int)
dataNasc = do d <- randomRIO (1,31)
              m <- randomRIO (1,12) 
              a <- randomRIO (1900, 2023)
              return (d,m,a)

-- gera matriz (linha, coluna) com valores aleatorios
-- num dado intervalo

type Mat a = [[a]]

geraMatriz :: (Int, Int) -> (Int,Int) -> IO (Mat Int)
geraMatriz (0,_) _ = return [ ]
geraMatriz (nl,nc) (x,y) = 
    do  l <- geralinha nc (x,y)
        ls <- geraMatriz (nl-1,nc) (x,y)
        return (l:ls)

geralinha :: Int -> (Int, Int) -> IO [Int]
geralinha 0 _ = return [ ]
geralinha c (x,y) = do i <- randomRIO (x,y)
                       is <- geralinha (c-1) (x,y)
                       return (i:is)

bingo :: IO ()
bingo = do <- acumulaNumeros []
        print l

acumulaNumeros :: [Int] -> IO [Int]
acumulaNumeros l | length l == 9 = return l
                 | otherwise = do <- randomRIO (1,9)
                                 -- print v
                                  --getChar
                                  if v `elem` l then acumulaNumeros l
                                  else do print v 
                                          getChar acumulaNumeros (l ++ [v]) 


geraChaveSecreta :: IO (Int,Int,Int,Int)
geraChaveSecreta = do cs1 <- randomRIO (0,9)
                      cs2 <- randomRIO (0,9)  
                      cs3 <- randomRIO (0,9)
                      cs4 <- randomRIO (0,9)
                      return (cs1,cs2,cs3,cs4)

sugereChave :: IO (Int,Int,Int,Int)
sugereChave = do print "Introduza 4 Digitos: "
                 s1 <- getChar
                 s2 <- getChar
                 s3 <- getChar
                 s4 <- getChar
                 return (digitToInt s1,
                         digitToInt s2,
                         digitToInt s3,
                         digitToInt s4)


mastermind :: IO ()
mastermind = do cs <- geraChaveSecreta

joga :: (Int,Int,Int,Int) -> IO (Int,Int,Int,Int)
joga cs = do sugs <- sugereChave
             if cs == sugs
             then return sugs
             else do feedback cs sugs
                     joga cs

feedback :: (Int,Int,Int,Int) 
            -> (Int,Int,Int,Int) -> (Int,Int)
feedback (c1,c2,c3,c4) (s1,s2,s3.s4) =  