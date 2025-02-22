module Ficha1 where

perimetro :: Float -> Float
perimetro raio = 2*pi*raio

dist :: (Double, Double) -> (Double, Double) -> Double
dist (x1, y1) (x2, y2) = sqrt ((x1-x2)^2 + (y1-y2)^2)

primUlt :: [a] -> (a,a)
primUlt lista = (head lista, last lista)

multiplo :: Int -> Int -> Bool
multiplo m n = mod m n == 0

truncaImpar :: [a] -> [a] 
truncaImpar l = if mod (length l) 2 == 0
                then l
                else tail l

max2 :: Int -> Int -> Int
max2 x y = if x > y
           then x
           else y

max3 :: Int -> Int -> Int -> Int
max3 a b c = if c > max2 a b
              then c
              else max2 a b

max3' :: Int -> Int -> Int -> Int
max3' a b c = max2 c (max2 a b)

-- Exercicio 2

nRaizes :: Int -> Int -> Int -> Int 
nRaizes a b c = if d > 0
                then 2
                else (if d == 0
                 then 1
                 else 0)
  where d = (b^2)-4*a*c

raizes :: Float -> Float -> Float -> [Float]
raizes a b c = if delta > 0
               then [((-b + (sqrt delta))/2*a), ((-b - (sqrt delta))/2*a)]  
               else (if ((b^2)-4*a*c) == 0
                 then [(-b/2*a)]
                 else [ ])
  where delta = (b^2)-4*a*c

-- Exercicio 3 
                
type Hora = (Int,Int)


valida :: Hora -> Bool
valida (h,m) = h >= 0 && h < 24 && m >= 0 && m < 60

-- testar se h1 vem primeiro que h2

depois :: Hora -> Hora -> Bool
depois (h1,m1) (h2,m2) = if h1 > h2 
                         then True
                         else if h2 > h1 then False
                         else m1 > m2

depois' :: Hora -> Hora -> Bool
depois' (h1,m1) (h2,m2)
  | h1 > h2   = True
  | h1 < h2   = False 
  | otherwise = m1 > m2

minutos :: Hora -> Int
minutos (h3,m3) = h3*60 + m3

horas :: Int -> Hora
horas m4 = (div m4 60, mod m4 60 ) 

difh :: Hora -> Hora -> Int
difh (h5,m5) (h6,m6) = m7 - m8
 where m7 = h5*60 + m5; m8 = h6*60 + m6

maism :: Hora -> Int -> Hora
maism (h9,m9) x = if y > 59
                  then (h9 + div y 60, mod y 60)
                  else (h9,y)
  where y = m9 + x

-- Exercicio 4

data HORA = H Int Int 
    deriving (Show,Eq)

valida1 :: HORA -> Bool
valida1 (H h' m') = h' >= 0 && h' < 24 && m' >= 0 && m' < 60

depois1 :: HORA -> HORA -> Bool
depois1 (H h1' m1') (H h2' m2') = if h1' > h2' 
                         then True
                         else if h2' > h1' then False
                         else m1' > m2'

minutos1 :: HORA -> Int
minutos1 (H h3' m3') = h3'*60 + m3'

horas1 :: Int -> HORA
horas1 x = let h4' = div x 60
               m4' = mod x 60
           in (H h4' m4') 

difh1 :: HORA-> HORA -> Int
difh1 (H h5' m5') (H h6' m6') = m7' - m8'
 where m7' = h5'*60 + m5'; m8' = h6'*60 + m6'

-- Exercicio 5

data Semaforo = Verde | Amarelo | Vermelho deriving (Show,Eq)

next :: Semaforo -> Semaforo 
next cor1 = if cor1 == Verde then Amarelo
            else if cor1 == Amarelo then Vermelho
            else Verde

next' :: Semaforo -> Semaforo
next' Verde = Amarelo
next' Amarelo = Vermelho
next' Vermelho = Verde

stop :: Semaforo -> Bool
stop cor2 = if cor2 == Vermelho 
            then True 
            else False

stop' :: Semaforo -> Bool
stop' Vermelho = True
stop' x = False

safe :: Semaforo -> Semaforo -> Bool
safe cors1 cors2 = if cors1 == Verde && cors2 == Vermelho then True
                   else if cors1 == Vermelho && cors2 == Verde then True
                   else False 

safe' :: Semaforo -> Semaforo -> Bool
safe' Vermelho _ = True
safe' _ Vermelho = True
safe' _ _ = False

-- Exercicio 6
data Ponto = Cartesiano Double Double | Polar Double Double 
  deriving (Show,Eq)

posx :: Ponto -> Double
posx (Cartesiano p4 p5) = p4
posx (Polar p6 p7) = cos p7

posy :: Ponto -> Double 
posy (Cartesiano p8 p9) = p9
posy (Polar a1 a2) = sin a2

raio :: Ponto -> Double
raio (Cartesiano a3 a4) = sqrt (a3^2 + a4^2)
raio (Polar a5 a6) = a5

angulo :: Ponto -> Double
angulo (Cartesiano a7 a8) = asin a8
angulo (Polar a9 a') = a'

dist2 :: Ponto -> Ponto -> Double
dist2 (Cartesiano x3 x4) (Cartesiano x5 x6) = sqrt ((x3-x5)^2 + (x4-x6)^2)
dist2 (Polar y3 y4) (Polar y5 y6) = sqrt ((cos y3 - cos y5)^2 + (sin y4 - sin y6)^2)


-- Exercicio 7
data Figura = Circulo Ponto Double
 | Rectangulo Ponto Ponto
 | Triangulo Ponto Ponto Ponto  
 deriving (Show,Eq)

poligono :: Figura -> Bool
poligono (Rectangulo r1 r2) 
  | (posx r1 /= posx r2 && posy r1 /= posy r2) = True
  | otherwise = False
poligono (Triangulo t1 t2 t3) 
  | (posx t1 /= posx t2 && posx t2 /= posx t3 && posy t1 /= posy t2 && posy t2 /= posy t3) = True
  | otherwise = False 
poligono _ = False

--poligono (Circulo (Cartesiano c3 c4) r) = False
--poligono (Rectangulo (Cartesiano d1 d2) (Cartesiano d3 d4)) = if d1 /= d3 && d2 /= d4 then True else False
--poligono (Triangulo (Cartesiano t1 t2) (Cartesiano t3 t4) (Cartesiano t5 t6)) = if t1 /= t3 && t3 /= t5 && t2 /= t4 && t4 /= t6 then True else False

vertices :: Figura -> [Ponto]
vertices (Rectangulo r3 r4) = [(r3), (r4), (r5), (r6)]
  where r5 = ( Cartesiano (posx r3) (posy r3 + posy r4) )
        r6 = (Cartesiano (posx r3 + posx r4) (posy r4))
vertices (Triangulo t4 t5 t6) = [(t4), (t5), (t6)]
vertices _ = [ ]

--vertices (Circulo (Cartesiano c5 c6) r') = [ ]
--vertices (Rectangulo (Cartesiano d5 d6) (Cartesiano d7 d8)) = [(Cartesiano d5 d6), (Cartesiano d7 d8)]
--vertices (Triangulo (Cartesiano t7 t8) (Cartesiano t9 t') (Cartesiano t'' t''')) = [(Cartesiano t7 t8), (Cartesiano t9 t'), (Cartesiano t'' t''')]

area :: Figura -> Double
area (Triangulo l1 l2 l3) =
  let a = dist2 l1 l2
      b = dist2 l2 l3
      c = dist2 l3 l1
      s = (a+b+c) / 2 -- semi-perimetro
  in sqrt (s*(s-a)*(s-b)*(s-c)) -- formula de Heron
area (Rectangulo r7 r8) = 
  let d = (posx r8) - (posx r7)
      f = (posy r8) - (posy r7)
      s' = (d*f) / 2 -- semi perimetro
  in d*f 
area (Circulo c1 r) = 2*pi*r 

-- Exercicio 8
 
islower :: Char -> Bool
islower a = if (ord a) >= (ord 'a') && (ord a) <= (ord 'z') then True
            else False

isdigit :: Char -> Bool
isdigit b = if (ord b) >= (ord '0') && (ord b) <= (ord '9') then True
            else False 

isAlpha :: Char -> Bool
isAlpha c = if (ord c) >= (ord 'A') && (ord c) <= (ord 'Z') || (ord c) >= (ord 'a') && (ord c) <= (ord 'z')
            then True else False

toUpper1 :: Char -> Char
toUpper1 d
        | (ord d) >= (ord 'a') && (ord d) <= (ord 'z') = chr ((ord d) - 32)

{- intToDigit :: Int -> Char
intToDigit x
          | x >= 0 && x <= 9 = chr (x + 40)

-}