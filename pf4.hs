module Pf4 where

import Data.Char
import Data.List


nzp :: [Int] -> (Int,Int,Int)
nzp [ ] = (0,0,0)
nzp (x:xs)
    | x < 0 =(1+ a,b,c)
    | x == 0 = (a,1+b ,c)
    | x > 0 = (a,b,1+c)
   where (a,b,c) = nzp xs

nzp1 :: [Int] -> (Int,Int,Int)
nzp1 l = nzpAC l (0,0,0)
 
nzpAc ::[Int] -> (Int,Int,Int) -> (Int,Int,Int)
nzpAC [ ] (a,b,c) = (a,b,c)
nzpAc (x:xs) (a,b,c)
    | x < 0 = nzpAC xs (1+ a,b,c)
    | x == 0 = nzpAC xs(a,1+b ,c)
    | x > 0 = nzpAc xs (a,b,1+c)

digitAlpha :: String -> (String,String)    
digitAlpha [ ] = ([],[])
digitAlpha (x:xs) 
            | isDigit x = (x: a,b)
            | isAlpha x = (a, x :b)
            | otherwise = (a,b)
         where (a,b) = digitAlpha xs

mydiv :: Int -> Int -> Int
mydiv x y
   | x >= y = 1 + div (x-y) y
   | x < y = 0

mymod :: Int -> Int -> Int
mymod x y
    | x >= y = mod (x-y) y 
    | x < y = x 


mydivMod :: Integral a => a -> a -> (a, a)
mydivMod x y
     | x >= y = (1+q,r)
     | x < y = (0,x)
    where (q,r) = mydivMod (x-y) y

fromDigits :: [Int] -> Int
fromDigits [] = 0
fromDigits (h:t) = h*10^(length t) + fromDigits t

maxSumInit :: (Num a, Ord a) => [a] -> a
maxSumInit l = maximum [sum m | m <- inits l] 

{-Summax :: (Num a, Ord a) => [a] -> a
Summax [ ] = 0
Summax l = aux l 0 0


aux [ ] s m = m
aux (x:xs) s m 
         | s > m = s
         | otherwise = m
      where s = x + s xs
      where m = maximum s 
-}
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibonacci :: Integer -> Integer 
fibonacci n = fibAc n (0,1)

fibAc :: Integer -> (Integer,Integer) -> Integer
fibAc 0 (a,b) = a
fibAc 1 (a,b) = b 
fibAc n (a,b) = fibAc (n-1) (b, a+b)

--intToStr :: Integer -> String
--intToStr (x:xs) = 
[]
