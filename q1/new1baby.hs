min3 :: Int -> Int -> Int -> Int
min3 a b c
  | a <= b && a <= c = a
  | b <= c = b
  | otherwise = c

threeEqual :: Int -> Int -> Int -> Bool
threeEqual m n p = (m == n) && (n == p)

fourEqual :: Int -> Int -> Int -> Int -> Bool
fourEqual m n p q = threeEqual m n p && (p == q)

