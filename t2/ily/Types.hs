module Soal3.Types (Tree (Leaf,Node), Bit (L,R),HCode, Table) 
where

data Bit = L | R deriving (Eq, Show)
type HCode = [Bit]
type Table = [ (Char,HCode) ]
data Tree = Leaf Char Int | Node Int Tree Tree deriving (Show)

instance Ord Tree where
  Leaf _ i1 <= Leaf _ i2 = i1 <= i2
  Node i1 _ _ <= Node i2 _ _ = i1 <= i2
  Leaf _ i1 <= Node i2 _ _ = i1 <= i2 
  Node i2 _ _ <= Leaf _ i1 = i2 <= i1   
  
instance Eq Tree where
  Leaf _ i1 == Leaf _ i2 = i1 == i2
  Node i1 _ _ == Node i2 _ _ = i1 == i2
  Leaf _ i1 == Node i2 _ _ = i1 == i2
  Node i2 _ _ == Leaf _ i1 = i1 == i2