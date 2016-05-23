middleNumber :: Int -> Int -> Int -> Int
middleNumber x y z
  | between x y z = y
  | between y x z = x
  | otherwise = z

between :: Int -> Int -> Int -> Bool
between x y z = weakAscOrder x y z || weakAscOrder z y x

weakAscOrder :: Int -> Int -> Int -> Bool
weakAscOrder x y z = (x <= y) && (y <= z)