module Pf3 where

data HORA = H Int Int 
    deriving (Show,Eq)

valida :: HORA -> Bool
valida (H h' m') = h' >= 0 && h' < 24 && m' >= 0 && m' < 60

depois :: HORA -> HORA -> Bool
depois (H h1' m1') (H h2' m2') = if h1' > h2' 
                         then True
                         else if h2' > h1' then False
                         else m1' > m2'

minutos :: HORA -> Int
minutos (H h3' m3') = h3'*60 + m3'

horas :: Int -> HORA
horas x = let h4' = div x 60
              m4'= mod x 60
           in (H h4' m4') 

difh :: HORA-> HORA -> Int
difh (H h5' m5') (H h6' m6') = m7' - m8'
 where m7' = h5'*60 + m5'; m8' = h6'*60 + m6'
 


type Etapa = (HORA,HORA)
type Viagem = [Etapa]

etapaValida :: Etapa -> Bool
etapaValida (p,c) = valida p && valida c && depois c p

viagemVal :: Viagem -> Bool
viagemVal [ ] = False
viagemVal ((p1,c1):(p2,c2):t) = etapaValida (p1,c1) && depois p2 c1 && viagemVal ((p2,c2):t) 
viagemVal [e] = etapaValida e
           


{-tempototal :: Viagem -> Float
tempototal [ ] = 0
tempototal ((p3,c3):(p4,c4):t) = (c3-p3) + (c4-p4) + tempototal t -}

totalespera ::Viagem -> HORA
totalespera v = horas (espera v)


espera :: Viagem -> Int
espera ((p5,c5):(p6,c6):t) = ((minutos p6) - (minutos c5)) + espera ((p6,c6):t)
espera _ = 0 



-- Ex 3

data Contacto = Casa Integer
             | Trab Integer
             | Tlm Integer
             | Email String
    deriving Show

type Nome = String
type Agenda = [(Nome, [Contacto])]

ag1 :: Agenda
ag1 = [("ANA", [Casa 253123123, Tlm 911111111, Email "ana@uminho.pt"]),
       ("Nuno", [Tlm 933333333, Tlm 911232312]),
       ("Joao", [Trab 222222222, Email "joao@gmail.com" ])
      ]

acrescEmail :: Nome -> String -> Agenda -> Agenda
acrescEmail n s ((x,l):t)
            | n==x = (x,(Email s):l) :t 
            | otherwise = (x,l): acrescEmail n s t
acrescEmail n s [ ] = [(n,[Email s])] 

verEmails :: Nome -> Agenda -> Maybe [String]
verEmails n [ ] = Nothing
verEmails n ((x,l):t) 
          | n == x = Just (procEmails l )
          | otherwise = verEmails n t

procEmails :: [Contacto] -> [String]
procEmails ((Email s):t) = s: procEmails t
procEmails (h:t) = procEmails t
procEmails [ ] = [ ]

consTelefs :: [Contacto] -> [Integer]
consTelefs [ ] = [ ]
consTelefs ((Casa c):(Trab t):(Tlm m):(Email e):xs) = c : t : m : consTelefs xs
consTelefs ((Email e):xs) = consTelefs xs

consTelefs1 :: [Contacto] -> [Integer]
consTelefs1 [ ] = [ ]
consTelefs1 ((Casa c):xs) = c : consTelefs1 xs
consTelefs1 ((Trab t):xs) = t : consTelefs1 xs
consTelefs1 ((Tlm m):xs) = m : consTelefs1 xs
consTelefs1 ((Email e):xs) = consTelefs xs
 
casa :: Nome -> Agenda -> Maybe [Integer]
casa n [ ] = Nothing
casa n ((x,l):t)
     | n == x = Just (proctcasa l ) 
     | otherwise = casa n t


proctcasa :: [Contacto] -> [Integer]
proctcasa ((Casa s):t) = s: proctcasa t 
proctcasa (h:t) = proctcasa t 
proctcasa [ ] = [ ] 

-- Exercicio 4

type Dia = Int
type Mes = Int
type Ano = Int
type Nome1 = String
data Data = D Dia Mes Ano
       deriving Show

type TabDN = [(Nome1,Data)]


tabela1 :: TabDN
tabela1 = [("Ana", D 1 1 2020), ("Mario", D 2 2 2020), ("Maria", D 3 3 2020), ("Joao", D 4 4 2020)]


procura :: Nome -> TabDN -> Maybe Data
procura n [ ] = Nothing
procura n ((x,xs):t)
        | n == x = Just xs
        | otherwise = procura n t
        
idade :: Data -> Nome -> TabDN -> Maybe Int
idade (D d m a) n ((x,(D d1 m1 a1)):t) 
     | n == x = idadeaux (D d m a) (D d1 m1 a1)
     | otherwise = idade (D d m a) n t

idade1 :: Data -> Nome -> TabDN -> Maybe Int
idade1 d n tab = case (procura n tab) of
                Nothing -> Nothing
                Just x -> idadeaux d x

idadeaux :: Data -> Data -> Maybe Int
idadeaux (D d m a) ( D d1 m1 a1)
      | a1 > a = Nothing
      | a > a1 && m < m1 = Just (a - a1 -1)
      | a > a1 && m > m1 = Just (a - a1)
      | a > a1 && m == m1 && d < d1 = Just (a - a1 -1)
      | a > a1 && m == m1 && d == d1 = Just (a -a1)

calcula :: Data -> Data -> Int
calcula (D d m a) ( D d1 m1 a1)
      | a > a1 && m < m1 = (a - a1 -1)
      | a > a1 && m > m1 = (a - a1)
      | a > a1 && m == m1 && d < d1 = (a - a1 -1)
      | a > a1 && m == m1 && d == d1 = (a -a1)


anterior :: Data -> Data -> Bool
anterior (D d m a) ( D d1 m1 a1)
         | a < a1 = True
         | a == a1 && m < m1 = True
         | a == a1 && m == m1 && d < d1 = True
         | otherwise = False 


ordena :: TabDN -> TabDN
ordena [ ] = [ ]
ordena (h:t) = insere h (ordena t)

insere :: (Nome1,Data) -> TabDN -> TabDN
insere x [ ] = [x]
insere (x, D d m a) ((y, D d1 m1 a1):t)
        | anterior (D d m a) (D d1 m1 a1) = (x, D d m a) : (y, (D d1 m1 a1)) : t
        | otherwise = (y, D d1 m1 a1) : insere (x, D d m a) t

porIdade:: Data -> TabDN -> [(Nome1,Int)]
porIdade d (h:t) = aux d (reverse (ordena (h:t)))

aux:: Data -> TabDN -> [(Nome1, Int)]
aux _ [ ] = [ ]
aux d ((n,x):t) = (n,(calcula d x)) : aux d t 

{-porIdade:: Data -> TabDN -> [(Nome,Int)]
porIdade (D d m a ) ((x, (D d1 m1 a1)),(y,(D d2 m2 a2)):t)
        | idadeaux (D d m a) (D d1 m1 a1) < idadeaux (D d m a) (D d2 m2 a2) = (x,idadeaux (D d m a) (D d1 m1 a1) ) : porIdade ((y,(D d2 m2 a2):t)) 
        | otherwise = (y,idadeaux (D d m a) (D d2 m2 a2)) : porIdade ((x, (D d1 m1 a1)):t)
-}
-- Exercicio 5

data Movimento = Credito Float | Debito Float
              deriving Show

data Data1 = D1 Int Int Int
            deriving Show

data Extrato = Ext Float [(Data, String, Movimento)]
              deriving Show


extValor :: Extrato -> Float -> [Movimento]
extValor 