module PriorityQueue
(
  PrQueue,
  build,      -- Ord a => [a] -> PrQueue a
  insert,     -- Ord a => PrQueue a -> a -> PrQueue a
  minElement, -- PrQueue a -> a
  extractMin, -- Ord a => PrQueue a -> (PrQueue a,a)
  size        -- PrQueue a -> Int
) where

newtype PrQueue a = PrQ [a] deriving Show

build :: Ord a => [a] -> PrQueue a
build [] = PrQ []
build (l:ls) = insert (build ls) l

minElement :: PrQueue a -> a
minElement (PrQ ls) = head ls

size :: PrQueue a -> Int
size (PrQ ls) = length ls

insert :: Ord a => PrQueue a -> a -> PrQueue a
insert (PrQ []) x = PrQ [x]
insert (PrQ ls) x
  | ls!!((length ls) `div` 2 - 1) <= x  = PrQ (ls ++ [x])
  | otherwise = insert 
                (PrQ (
                  take ((length ls) `div` 2 - 1) ls ++ 
                  [x] ++ drop ((length ls) `div` 2) ls
                )) (ls!!((length ls) `div` 2 - 1))

parent :: PrQueue a -> Int -> Int


extractMin :: Ord a => PrQueue a -> (PrQueue a,a)
extractMin (PrQ ls) = 
  (PrQ (
    last(ls) : tail(init(ls))
  )
  ,head(ls))
 
--parent :: PrQueue a -> a

--fromList :: Ord a => [a] -> Heap a
--fromList = foldr insert mempty