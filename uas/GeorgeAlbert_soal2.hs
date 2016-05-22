-- George Albert
-- 1406569781
-- Pemdek C
-- Tugas Mandiri

module Tree
  (Tree,
  nil, -- Tree a
  balancing, -- Tree a -> Tree a
  ) where

data Tree a = Nil | Node a (Tree a) (Tree a)
  deriving Show

nil :: Tree a
nil = Nil

-- Soal 2

inorder :: Ord a => Tree a -> [a]
inorder Nil = []
inorder (Node n Nil Nil) = [n]
inorder (Node n a b)
  = inorder a ++ [n] ++ inorder b

inorderPartInit :: Ord a => [a] -> ([a], [a], [a])
inorderPartInit xt = inorderPart ([], [], xt) ((length xt) `div` 2)

inorderPart :: Ord a => ([a], [a], [a]) -> Int -> ([a], [a], [a])
inorderPart (xs, [a], xt) 0 = (xs, [a], xt)
inorderPart (xs, [a], []) _ = ([], [a], [])
inorderPart ([], [], (x:xt)) y = inorderPart ([], [x], xt) (y)
inorderPart (xs, [a], (x:xt)) y = inorderPart ((xs ++ [a]), [x], xt) (y-1)

fstT3 :: (a,b,c) -> a
fstT3 (a,_,_) = a

sndT3 :: (a,b,c) -> b
sndT3 (_,b,_) = b

thdT3 :: (a,b,c) -> c
thdT3 (_,_,c) = c

balancing :: Ord a => Tree a -> Tree a
balancing t = makeBalanced (inorder t)

makeBalanced :: Ord a => [a] -> Tree a
makeBalanced [] = Nil
makeBalanced xs = ( Node 
                (head (sndT3 (inorderPartInit xs) ))
                ( makeBalanced (fstT3 (inorderPartInit xs) ))
                ( makeBalanced (thdT3 (inorderPartInit xs) ))
              )
