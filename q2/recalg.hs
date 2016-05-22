data Expr = Lit Int |
            Add Expr Expr |
            Sub Expr Expr

data NTree = NilT |
             Node Int NTree NTree
             deriving (Show)

sumTree :: NTree -> Int
sumTree NilT = 0
sumTree (Node n t1 t2)
             = n + sumTree t1 + sumTree t2

size :: NTree -> Int
size NilT = 0
size (Node n t1 t2) = 1 + size t1 + size t2

member :: NTree -> Int -> Bool
member NilT p = False
member (Node n t1 t2) p
  | n == p    = True
  | otherwise = (member t1 p) || (member t2 p)

change :: NTree -> Int -> Int -> NTree
change NilT _ _ = NilT
change (Node n t1 t2) a b
  | n == a    = (Node b (change t1 a b) (change t2 a b))
  | otherwise = (Node n (change t1 a b) (change t2 a b))

-- note: there's no states on functional programming
-- such poor thing :((

data Pairs a = Pr a a -- wow this works

data List a = NilList | Cons a (List a)
              deriving (Eq, Ord, Show, Read)
              
length' NilList = 0
length' (Cons h t) = 1 + length' t

data Tree' a = Nil | NodeB a (Tree' a) (Tree' a)
               deriving (Eq, Ord, Show, Read)

inorder :: Tree' a -> [a]
inorder Nil = []
inorder (NodeB n t1 t2) =
    inorder t1 ++ [n] ++ inorder t2

data Either' a b = Left' a | Right' b
                  deriving (Eq,Ord,Read,Show)
