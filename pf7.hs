module PF7 where

data ExpInt = Const Int
            | Simetrico ExpInt
            | Mais ExpInt ExpInt
            | Menos ExpInt ExpInt
            | Mult ExpInt ExpInt   
        deriving (Show) 


-- 3 + (2-5)
exp1 = (Mais (Const 3) (Menos (Const 2) (Const 5))) 

-- (2-3)* -(5+20)
exp2 = Mult (Menos (Const 2)(Const 3))
            (Simetrico (Mais (Const 5)(Const 20)))

calcula :: ExpInt -> Int
calcula (Const x) = x
--calcula (Simetrico e) = (- e)
calcula (Mais e1 e2) = (calcula e1) + (calcula e2)
calcula (Menos e1 e2) = (calcula e1) - (calcula e2)
calcula (Mult e1 e2) = (calcula e1) * (calcula e2)

infixa :: ExpInt -> String
infixa (Const x) = show x
infixa (Simetrico e) = "-" ++ (infixa e)
infixa (Mais e1 e2) = "(" ++ (infixa e1) ++ " + " ++ (infixa e2) ++ ")"
infixa (Menos e1 e2) ="(" ++ (infixa e1) ++ " - " ++ (infixa e2) ++ ")"
infixa (Mult e1 e2) = "(" ++ (infixa e1) ++ " * " ++ (infixa e2) ++ ")"

posfixa :: ExpInt -> String
posfixa (Const x) = show x
posfixa (Simetrico e) = (posfixa e) ++ "~"
posfixa (Mais e1 e2) = (posfixa e1) ++ " " ++ (posfixa e2) ++ "+"
posfixa (Menos e1 e2) = (posfixa e1) ++  " " ++ (posfixa e2) ++ "-"
posfixa (Mult e1 e2) = (posfixa e1) ++  " " ++ (posfixa e2) ++ "*" 

data RTree a = R a [RTree a]
     deriving (Show)

rtree1 = R 5 [ R 4 [ R 3 [R 17 []], R 2 [], R 7 []],
               R 10 [],
               R 1 [ R 8 [ R 0 [], R 20 [], R 15 [], R 39 [] ],
               R 12 [] ]
             ]


soma :: Num a => RTree a -> a
soma (R x [ ]) = x 
soma (R x lista) = x + sum (map soma lista )

altura :: RTree a -> Int
altura (R x [ ]) = 1
altura (R x lista) = 1 + maximum (map altura lista) 

prune :: Int -> RTree a -> RTree a
prune 1 (R x _) = (R x [ ])
prune n (R x lista) 
      | n > 1 = (R x (map (prune (n-1)) lista)) --cuidado com os ( )

mirror :: RTree a -> RTree a
mirror (R x lista) = R x (reverse (map mirror lista))

data BTree a = Empty 
             | Node a (BTree a) (BTree a)
        deriving (Show)

data LTree a = Tip a 
             | Fork (LTree a) (LTree a)
        deriving (Show)

ltree1 = Fork (Tip 3) (Fork (Fork (Tip 7)(Tip 8))(Tip 2))

ltSum :: Num a => LTree a -> a
ltSum (Tip x) = x
ltSum (Fork e d) = ltSum e + ltSum d

listaLT :: LTree a -> [a]
listaLT (Tip x) = [x]
listaLT (Fork e d) = listaLT e ++ listaLT d

ltHeight :: LTree a -> Int
ltHeight (Tip x) = 1
ltHeight (Fork e d) = 1 + max (ltHeight e) (ltHeight d)

data FTree a b = Leaf b 
               | No a (FTree a b) (FTree a b)
            deriving (Show)

ftree1 = No 7 (Leaf 'A') (No 2 (Leaf 'B') (Leaf 'c'))


splitFTree :: FTree a b -> (BTree a, LTree b)
splitFTree (Leaf x) = (Empty, Tip x)
splitFTree (No x e d) = (Node x be bd, Fork le ld) 
      where (be,le) = (splitFTree e) 
            (bd,ld) = (splitFTree d)

joinTrees :: BTree a -> LTree b -> Maybe (FTree a b)
joinTrees Empty (Tip x ) = Just (Leaf x)
joinTrees (Node x e1 d1) (Fork e2 d2) =
    case (joinTrees e1 e2) of
        Nothing -> Nothing
        Just fte -> case (joinTrees d1 d2) of
                    Nothing -> Nothing 
                    Just ftd -> Just (No x fte ftd)

joinTrees _ _ = Nothing 


