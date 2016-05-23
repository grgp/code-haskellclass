module Soal3.MakeTree ( makeTree ) --[(Char,Int)]->Tree
where
import Soal3.Types
import Soal2.PriorityQueue
makeTree :: [(Char,Int)] -> Tree
makeTree = matrix . petrification

petrification :: [(Char,Int)] -> PrQueue Tree
petrification = build.map (uncurry Leaf)

matrix :: PrQueue Tree -> Tree
matrix xyz
 | size xyz == 1 = snd (extractMin xyz)
 | otherwise = matrix (allforone xyz)
 
allforone :: PrQueue Tree -> PrQueue Tree
allforone abc
 | size abc < 2 = abc
 | otherwise = allforone (insert albion (couple druid misha))
  where 
  (ddraig,druid) = extractMin abc
  (albion,misha) = extractMin ddraig

couple :: Tree -> Tree -> Tree
couple t1 t2 = Node (value t1 + value t2) t1 t2

value :: Tree -> Int
value (Leaf _ n) = n
value (Node n _ _ ) = n