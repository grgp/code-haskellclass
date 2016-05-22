length' :: [Int] -> Int
length' xs = foldr (\b c -> c + 1) 0 xs

map' :: (a -> b) -> [c] -> [d]
map' f xs = foldl (\n e -> f e) []