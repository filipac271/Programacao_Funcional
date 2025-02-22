module Testes2 where

mysortOn :: Ord b => (a -> b) -> [a] -> [a]
mysortOn f [ ] = [ ]
mysortOn f [a] = [a]
mysortOn f (h:t:xs)
     | f h > f t = t : mysortOn f (h:xs)
     | otherwise = h : mysortOn f (t:xs)


partes :: String -> Char -> [String]
partes [ ] c = [ ]
partes (x:xs) c 
     | x == c = "," : (partes xs c)
     | otherwise = [x] : (partes xs c)