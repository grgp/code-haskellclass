max3Ku :: Int -> Int -> Int -> Int
max3Ku x y z
  | (x >= y) && (x >= z) = x
  | (y >= z) = y
  | otherwise = z