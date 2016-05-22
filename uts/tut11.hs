-- SOAL 1
fibo :: Int -> Int
fibo 0 = 0
fibo 1 = 1
fibo x = fibo x-1 + fibo x-2

-- SOAL 2
gcdku :: Int -> Int -> Int
gcdku a 0 = a
gcdku 0 b = b
gcdku a b = gcdku b (mod a b)

-- SOAL 6
coin134 :: Int -> Int -> Int
coin134 6 = 2
coin134 x = (n `div` 4) + (n `mod` 4) `div` 3 + (n `mod` 4) `mod` 3
