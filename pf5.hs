module PF5 where

any1 :: (a -> Bool) -> [a] -> Bool
any1 f [ ] = False
any1 f (h:t) = f h || any1 f t


zipWith1 :: (a->b->c) -> [a] -> [b]-> [c]
zipWith1 f (x:xs) (y:ys) = f x y : zipWith1 f xs ys
zipWith _ _ _ = [ ]

takeWhile1 :: (a->Bool) -> [a] -> [a]
takeWhile1 f [ ] = [ ]
takeWhile1 f (x:xs) 
           | f x = x : takeWhile1 f xs
           | otherwise = [ ]

dropWhile1 :: (a->Bool) -> [a] -> [a]
dropWhile1 f (x:xs)
          | f x = dropWhile1 f xs
          | otherwise = x : dropWhile1 f xs

 {-         
span1 :: (a-> Bool) -> [a] -> ([a],[a])
span1 f (x:xs)
      | f x = [x] : span1 f xs
      | otherwise = span1 f xs : [x]
      -}

deleteBy1 :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteBy1 f x (h:t)
          | f x h = t
          | otherwise = h : deleteBy1 f x t

sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f [ ] = [ ]
sortOn f (x:xs) = insert f x (sortOn f xs)

insert :: Ord b => (a -> b)-> a -> [a] -> [a]
insert f x [ ] = [x]
insert f x (y:ys)
       | f x <= f y = x : y : ys
       | otherwise = y : insert f x ys


type Polinomio = [Monomio]
type Monomio = (Float,Int)

conta1 :: Int -> Polinomio -> Int
conta1 n pol = foldr aux1 0 pol
  where aux1 (c,e) r = if e == n then 1 + r else r 

calcula :: Float -> Polinomio -> Float 
calcula n ((x,xs):y) = x*n^xs + calcula n y

calcula1 n pol = foldr (\(c,e) r -> (c*(n^e))+r) 0

--calcula2 :: Float -> Polinomio -> Float 
--calcula2 n pol = sum (map (\ x (c,e) -> (c*n^e)) pol)
   

selgrau :: Int -> Polinomio -> Polinomio
selgrau n pol = filter ((==n) . snd) pol

--selgrau n pol = filter (\x-> (snd x)==n) pol

--selgrau n pol = filter (\(c,e)-> e==n) pol


--selgrau n pol = filter aux pol
   -- where aux (c,e) = e == n

deriv :: Polinomio -> Polinomio
deriv pol = foldr (\(c,e) r -> (c*(fromIntegral e), e-1):r) [ ] pol

--deriv pol = map (\ (c,e) -> (c*(fromIntegral e),e -1)) pol

simp :: Polinomio -> Polinomio
simp ((x,y):xs)
      | y == 0 = simp xs
      | otherwise  = (x,y) : simp xs

mult ::Monomio -> Polinomio -> Polinomio
mult (x,xs) ((y,ys):yss) = (x*y, xs+ys) : mult (x,xs) yss

-- ordena :: Polinomio -> Polinomio
-- ordena ((x,y):xs) = isort y snd xs                   --está mal

--falta a normaliza e a soma



type Mat a = [[a]]

mat1 = [[1,2,3],[0,4,5],[0,0,6]]

dimOK :: Mat a -> Bool
dimOK [ ] = False 
dimOK m  =  let (x:xs) = map length m 
            in x/=0 && (filter (/=x) xs) == [ ]


addMat :: Num a => Mat a -> Mat a -> Mat a
addMat (la: las) (lb : lbs) = (zipWith1 (+)la lb) : addMat las lbs
addMat [ ] [ ] = [ ]

transpose :: Mat a -> Mat a
transpose ([ ]:t) = [ ]
transpose m = (map head m) : transpose (map tail m)

multMat :: Num a => Mat a -> Mat a -> Mat a
multMat (l:ls) m = (linha l m) : multMat ls m

linha :: Num a => [a]-> Mat a -> [a]
linha l ([]:_) = [ ]
linha l m = sum (zipWith1 (*) l (map head m)) : linha l (map tail m)
