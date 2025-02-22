module Pf50q where 

-- Questao 1



-- Questão 2
enumFromThenTo1 :: Int -> Int -> Int -> [Int]
enumFromThenTo1 x y z
            | x > z && y >= x || x < z && y < x = [ ]
            | otherwise = x : enumFromThenTo1 y (2 * y-x) z

-- Questao 3
reserve :: [a] -> [a]
reserve [ ] = [ ]
reserve (x:xs) = reserve xs ++ [x]


-- Questao 4
take1 :: Int -> [a] -> [a]
take1 _ [ ] = [ ]
take1 n (x:xs)
       | n <= 0 = [ ]
       | n == 1 = [x]
       | n >= 1 = x : take1 (n-1) xs 



-- Questão 5


-- Questao 6



-- Questao 7


-- Questao 8



-- Questao 9


-- Questão 10


--Questao 11


--Questao 12

--Questao 13

--Questao 14

--Questao 15

--Questao 16

--Questao 17

--Questao 18

--Questao 19


--Questao 20


--Questao 21

--Questao 22

--Questao 23

--Questao 24

--Questao 25

--Questao 26

--Questao 27

--Questao 28



--Questao 29

union :: Eq a => [a] -> [a] -> [a]
union l [ ] = l
union (x:xs) (y:ys)
       | x == y = x : union xs ys
       | otherwise = x : union xs (y:ys)


intersect :: Eq a => [a] -> [a] -> [a]
intersect _ [ ] = [ ]
intersect (x:xs) l
          | x `elem` l = x : intersect xs l
          | otherwise = intersect xs l 


--Questao 30
inser :: Ord a => a -> [a] -> [a]
inser n [ ] = [n]
inser n (x:xs)
      | n == x = (x:xs)
      | n < x = n : x : xs
      | otherwise = x : inser n xs

-- Questão 31
unword :: [String] -> String
unword [ ] = ""
unword (x:xs) = x ++ " " ++ unword xs 

-- Questão 32 
unline :: [String] -> String
unline [ ] = " "
unline (x:xs) = x ++ "\n" ++ unline xs

-- Questão 39

elemSet :: Eq a => a -> [(a,Int)] -> Bool
elemSet _ [ ] = False
elemSet n ((x,y):xs) = n == x || elemSet n xs

-- Questão 40

covertMSet :: [(a,Int)] -> [a]
covertMSet ((x,y):xs) = if y > 0 then replicate y x  ++ covertMSet xs else covertMSet xs 

catMaybes :: [Maybe a] -> [a]
catMaybes [ ] = [ ]
catMaybes (Just x :t) = x : catMaybes t
catMaybes (Nothing: t) = catMaybes t


enumFromThenTo :: Int -> Int -> Int -> [Int]
enumFromThenTo 