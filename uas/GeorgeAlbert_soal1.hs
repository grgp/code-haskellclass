-- George Albert
-- 1406569781
-- Pemdek C
-- Tugas Mandiri

module PriorQueue ( PQueue, emptyQ, add, removeMin ) where

newtype PQueue a = PQ [a] deriving (Show) -- representasi sorted list

emptyQ :: PQueue a
emptyQ = PQ []

-- insert sebuah elemen ke priority queue, ingat terurut!
add :: Ord a => PQueue a -> a -> PQueue a
add (PQ []) y     = PQ [y]
add (PQ (x:xs)) y
  | x >= y        = PQ (y:x:xs)
  | otherwise     = addPrepend x ( add (PQ xs) y )

addPrepend :: a -> PQueue a -> PQueue a
addPrepend y (PQ [])    = PQ [y]
addPrepend y (PQ (xs))  = PQ (y:xs)

-- kembalikan elemen min dan priority queue baru setelah removeMin
removeMin :: PQueue a -> (a, PQueue a)
removeMin (PQ [])       = error "no element to be removed"
removeMin (PQ (x:xs))   = (x, PQ (xs))