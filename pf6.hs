module Pf6 where

data BTree a = Empty
         | Node a (BTree a) (BTree a)
       deriving Show

arv1 = Node 10 arv2 arv3
arv2 = Node 7 (Node 3 Empty Empty) Empty
arv3 = Node 16 (Node 13 Empty Empty) (Node 20 (Node 18 Empty Empty) Empty)


altura :: BTree a -> Int
altura Empty = 0
altura (Node x e d) = 1 + max (altura e) (altura d)

contaNodos :: BTree a -> Float
contaNodos Empty = 0
contaNodos (Node x e d) = 1 + (contaNodos e) + (contaNodos d)

folhas :: BTree a -> Int
folhas Empty = 0
folhas (Node x Empty Empty) = 1
folhas (Node x e d ) = (folhas e) + (folhas d) 

listaFolhas :: BTree a -> [a]
listaFolhas Empty = [ ]
listaFolhas (Node x Empty Empty) = [x]
listaFolhas (Node x e d) = listaFolhas e ++ listaFolhas d

prune :: Int -> BTree a -> BTree a
prune _ Empty = Empty
prune 0 _ = Empty
prune n (Node x e d) = Node x (prune (n-1) e) (prune (n-1)d)

path ::[Bool] -> BTree a -> [a]
path _ Empty = [ ]
path [ ] (Node x e d) = [x]
path (p:ps) (Node x e d) = if p then x : path ps d
                                else x : path ps e

mirror :: BTree a -> BTree a
mirror Empty = Empty
mirror (Node x e d) = Node x (mirror d) (mirror e)

zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
zipWithBT f Empty Empty = Empty
zipWithBT f (Node x e d) (Node x1 e1 d1) = (Node (f x x1) (zipWithBT f e e1) (zipWithBT f d d1) )

unzipBT :: BTree (a,b,c) -> (BTree a,BTree b,BTree c)
unzipBT Empty = (Empty, Empty, Empty)
unzipBT (Node (a,b,c) e d) = ((Node a ae ad), (Node b be bd), (Node c ce cd) )
                          where  (ae, be, ce) = unzipBT e
                                 (ad, bd, cd) = unzipBT d

--Exercicio 2

minimo :: Ord a => BTree a -> a
minimo (Node x Empty _) = x
minimo (Node x e d) = minimo e

preorder :: BTree a -> [a]
preorder Empty = []
preorder (Node x e d) = [x] ++ (preorder e) ++ (preorder d)


semMinimo :: Ord a => BTree a -> BTree a
semMinimo (Node x Empty d) = d
semMinimo (Node x e d) = (Node x (semMinimo e) d )

minSmin :: Ord a => BTree a -> (a,BTree a)
minSmin (Node x Empty d) = (x, d)
minSmin (Node x e d) =  (m,Node x a d)
         where (m,a) = minSmin e

{-remove :: Ord a => a -> BTree a -> BTree a
remove x Empty = Empty
remove x (Node z e d)
        | x < z = Node z (remove x e) d
        | x > z = Node z e (remove x d)
        | x == z = case d of
                    Empty -> e
                     _    -> let (y, d') =  minSmin d
                             in Node y e d'

-}

type Aluno = (Numero,Nome,Regime,Classificacao)
type Numero = Int
type Nome = String
data Regime = ORD | TE | MEL deriving Show 
data Classificacao = Aprov Int
                   | Rep
                   | Faltou
  deriving Show
type Turma = BTree Aluno -- ´arvore bin´aria de procura (ordenada por n´umero)

inscNum :: Numero -> Turma -> Bool
inscNum _ Empty = False
inscNum n (Node (nu,no,re,c) e d)
        | n == nu = True 
        | n < nu = inscNum n e
        | otherwise = inscNum n d

inscNome :: Nome -> Turma -> Bool
inscNome _ Empty = False
inscNome n (Node (nu,no,re,c)  e d)
        | n == no = True
        | inscNome n e || inscNome n d = True 

trabEst :: Turma -> [(Numero,Nome)]
trabEst Empty = []
trabEst (Node (nu,no,re,c) e d) 
      | "TE" == re = [(nu,no)] ++ trabEst e ++ trabEst d

nota :: Numero -> Turma -> Maybe Classificacao
nota _ Empty = Nothing
nota n (Node (nu,no,re,c) e d)
     | n == nu = Just c
     | n < nu = nota n e
     | n > nu = nota n d

percFaltas :: Turma -> Float
percFaltas Empty = 0
percFaltas (Node x e d) = div ((faltas(Node x e d))*100) (contaNodos ((Node x e d)))


faltas :: Turma -> Float
faltas Empty = 0
faltas (Node (nu,no,re,Faltou) e d) = 1 + faltas e + faltas d
           
mediaAprov :: Turma -> Float
mediaAprov Empty = 0
mediaAprov (Node x e d) = div (notast (Node x e d)) (contaNodos ((Node x e d)))


notast :: Turma -> Float
notast Empty = 0 
notast (Node (nu,no,re, Aprov n) e d) = n + notast e + notast d

-- aprovAv :: Turma -> Float nao entendi a pergunta

aprovAv :: Turma -> Float
aprovAv Empty = 0
aprovAv turma = uncurry (/) (sumAprovAv turma)
          
sumAprovAv :: Turma -> (Float, Float)
sumAprovAv Empty = (0,0)
sumAprovAv (Node (_,_,_,clas) l r) = case clas of Aprov nota -> (ap+1,av+1) 
                                                  Rep -> (ap,av+1)
                                                  _ -> (ap,av)
    where (ap,av) = addPairs (sumAprovAv l) (sumAprovAv r)
          addPairs (a,b) (c,d) = (a+c,b+d)

