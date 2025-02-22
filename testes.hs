module Testes where

total1 :: [[a]] -> Int
total1 [ ] = 0
total1 (x:t) = length x + total1 t

fun :: [(a,b,c)] -> [(a,c)]
fun [ ] = [ ]
fun ((x,xs,xss):t) = ((x,xss): fun t)

cola :: [(String,b,c)] -> String
cola [ ] = ""
cola (((x:ys),xs,xss):t) = ((x:ys) ++ cola t)

idade :: Int -> Int -> [(String,Int)] -> [String]
idade _ _[]= []
idade d i ((x,y):t)
      | y - d >= i = x : idade d i t
      | otherwise = idade d i t


union :: Eq a => [a] -> [a] -> [a]
union l [ ] = l
union l (h:t)
       | h `elem` l = union l t
       | otherwise = union (l ++ [h]) t

intersect1 :: Eq a => [a] -> [a] -> [a]
intersect1 _ [ ] = [ ]
intersect1 [ ] _ = [ ]
intersect1 (x:xs) (y:ys) 
          | x == y = (x : intersect1 xs ys)
          | otherwise = (intersect1 xs ys)