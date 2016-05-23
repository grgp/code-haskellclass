module Soal3.MakeTree (makeTree) where
-- [(Char, Int)] -> Tree
import Soal3.Types
import Soal2.PriorityQueue

makeTree :: [(Char,Int)] -> Tree
makeTree = makeCodes . toTreeList

toTreeList :: [(Char,Int)] -> PrQueue Tree
toTreeList = build . map (uncurry Leaf)

makeCodes :: PrQueue Tree -> Tree
makeCodes pt = getList pt
makeCodes ts = makeCodes (amalgamate ts)

amalgamate :: PrQueue Tree -> PrQueue Tree
amalgamate (t1:t2:ts) = insTree (pair t1 t2) ts

insTree :: Tree -> PrQueue Tree -> Tree
insTree t pt = t

pair :: Tree -> Tree -> Tree
pair t1 t2
  = Node (value t1 + value t2) t1 t2

value :: Tree -> Int
value (Leaf _ n)    = n
value (Node n _ _ ) = n