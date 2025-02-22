module PF8 where

 data Frac = F Integer Integer
     deriving (Show)

frac1 = F (-33) (-51)
frac2 = F 50 (-5)   

-- a e b positivos
mdc :: Integer -> Integer -> Integer
mds a b
   | a > b = mdc (a-b) b 
   | a < b = mdc a (b-a)
   | a == b = A

nomaliza :: Frac -> Frac
normaliza (F x y) = F (x*y)
     where d = mdc (abs x) (abs y)
           a = div (abs x) d
           b = div (abs y) d
           s = signum (x*y)

instance Eq Frac where
    (F a b) == (F c d) = a*b == c*d

instance Ord Frac where
    f1 <= f2 = let (F a b) = normaliza f1
                   (F c d) = normaliza f2
               in a*d <= c*d  

instance Ord Frac where
    show :: Frac -> String
    show (F a b) = "(" ++ (show a) ++ "/" ++ (show b) ++ ")"

instance Num Frac where
    (+) (*) (-) :: Frac -> Frac -> Frac
    (F a b) + (F b c) = F (a*d + c*d) (b*d)
    (F a b) * (F c d) = F (a*c) (b*d)
    (F a b) - (F c d) = F (a*d-c*d) (b*d)  
 

abs :: Frac -> Frac 
abs (F a b) = F (abs a) (abs b)

signum :: Frac -> Frac
signum (F a b) = F (signum (a*b)) 1

fromInteger :: Frac -> Frac
fromInteger x = F x 1

fun :: Frac -> [Frac] -> [Frac]
fun f l = filter  (> 2*f) l

data Exp a = Const a
           | Simetrico (Exp a)
           | Mais (Exp a) (Exp a)
           | Menos (Exp a) (Exp a)
           | Mult (Exp a) (Exp a)

-- (3+)


-- ((2-3)* (-(5+20)))
exp2 = Mult (Menos (Const 2) (Const 3))
            (Simetrico (Mais) (Const 5) (Const 20))

instance (Show a) => Show (Exp a) where
    show :: Exp a -> String
    show (Const x) = show x
    show (Mais e1 e2) = "(" ++ (show e1) ++ "+" ++ (show e2) ")"
    show (Menos e1 e2) = "(" ++ (show e1) ++ "-" ++ (show e2) ")"
    show 


calcula :: Num a => Exp a -> a
calcula (Const x) =




instance Num (Exp a) where
    (+), (*), (-) :: Exp a -> Exp a -> Exp a 
    e1 + e2 = Mais e1 e2
    (*) = Mult 
    (-) e1 e2 = Menos e1 e2

signum :: Exp a -> Exp a 
signum e = Cont (signum (calcula e1))

abs :: Exp a -> Exp a 
abs e = if signum e == (-1)
        then Simetrico e
        else e 

fromInteger :: Integer -> Exp a 
fromInteger x = Const (fromInteger x)
