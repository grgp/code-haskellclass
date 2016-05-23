-- primitive recursion

fac :: Int -> Int
fac n
  | n==0 = 1
  | n>0 = fac(n-1) * n
  | otherwise = error "error bruh"

power2 :: Int -> Int
power2 n
  | n==0 = 1
  | n>0 = power2(n-1) * 2
-- latihan rangeproduct ( m * m+1 * ... * (n-1) * n )
rangeProduct :: Int -> Int -> Int
rangeProduct m n
  | n<m = 0
  | n==m = n
  | otherwise = m * (rangeProduct (m+1) n)

facRange :: Int -> Int
facRange m = rangeProduct 1 m

-- latihan mult dengan primitive recursive
mult :: Int -> Int -> Int
mult m n
  | m==0 = 0
  | m>0 = (mult (m-1) n) * n