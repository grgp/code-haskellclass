module PriorityQueue
(
  PrQueue,
  build,      -- Ord a => [a] -> PrQueue a
  insert,     -- Ord a => PrQueue a -> a -> PrQueue a
  minElement, -- PrQueue a -> a
  extractMin, -- Ord a => PrQueue a -> (PrQueue a,a)
  size        -- PrQueue a -> Int
) where

newtype PrQueue a = PrQ [a] deriving (Show)

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
  | ls!!(parentEnd ls) <= x   = PrQ (ls ++ [x])
  | otherwise = PrQ 
                (
                  (getList (insert 
                    (PrQ 
                      ( take (parentEnd ls) ls )
                    ) x
                  ))
                  ++ (drop ((parentEnd ls)+1) ls)
                  ++ [ls!!(parentEnd ls)]
                )

parentEnd :: [a] -> Int
parentEnd [l] = 0
parentEnd ls = ((length ls) `div` 2 - 1)

extractMin :: Ord a => PrQueue a -> (PrQueue a,a)
extractMin (PrQ ls) = 
  (build ( last(ls) : tail(init(ls)) ) , head(ls))

getList :: Ord a => PrQueue a -> [a]
getList (PrQ xs) = xs