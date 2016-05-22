numEqual :: Eq a => [a] -> a -> Int
numEqual [] y = 0
numEqual (x:xs) y
  | x == y    = 1 + numEqual xs y
  | otherwise = numEqual xs y

--instance Visible Char where
--  toString ch = [ch]
--  size _      = 1

instance Eq Bool where
  True == True    = True
  False == False  = True

compare' :: Visible n => a -> a -> Bool
compare' x y = size x <= size y