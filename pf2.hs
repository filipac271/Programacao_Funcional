module Pf2 where

import Data.Time.Format.ISO8601 (yearFormat)

import Data.Char

funA :: [Double] -> Double
funA [] = 0
funA (y:ys) = y^2 + (funA ys)

{- fun [2,3,5,1]
= fun (2:[3,5,1])
= 2^2 + (funA [3,5,1])
= 4 + funA (3:[5,1])
= 4 + 3^2 + funA([5,1])
= 13 + funA ([5,1])
= 13 + funA (5: [1])
= 13 + 5^2 + funA ([1])
= 38 + funA (1: [ ])
= 38 + 1^2 + funA ([ ])
= 39 + funA [ ]
= 39 +0 
= 39
-}

funD l = g [] l
g acc [] = acc
g acc (h:t) = g (h:acc) t 

{- 





-}


-- Exercicio 2

dobros :: [Float] -> [Float]
dobros [ ] = [ ]
dobros (x:xs) = x*2 : (dobros xs)

numOcorre :: Char -> String -> Int
numOcorre x (y:ys) = if x == y then 1 + (numOcorre x ys) else 0 + (numOcorre x ys) 

{- positivos :: [Int] -> Bool
positivos [ ] = False
positivos (x:xs) = (mod x 2) == 0  positivos xs -}

soPos :: [Int] -> [Int]
soPos [ ] = [ ]
soPos (x:xs) = if x < 0 then soPos xs else x : soPos xs

somaNeg :: [Int] -> Int
somaNeg (x:xs) = if x < 0 then x + somaNeg xs else somaNeg xs 

tresUlt :: [a] -> [a]
tresUlt (a:b:c:d:t) = tresUlt (b:c:d:t)
tresUlt l = l

segundos :: [(a,b)] -> [b]
segundos [ ] = [ ]
segundos ((x,xs):t) = (xs:segundos t)

{- nosPrimeiros :: (Eq a) => a -> [(a,b)] -> Bool
nosPrimeiros x [ ] = False
nosPrimerios x ((y:ys):t) = if x == y then True
                            else nosPrimeiros x t  -}

--sumTriplos :: (Num a, Num b, Num c) => [(a,b,c)] -> (a,b,c)
--sumTriplos ((x:xs:xss),(y:ys:yss):t) = 
 
--Exercicio 3
soDigitos :: [Char] -> [Char]
soDigitos [ ] = [ ]
soDigitos (x:xs)
          | ord x >= ord '0' && ord x <= ord '9' = x : soDigitos xs
          | otherwise = soDigitos xs
        