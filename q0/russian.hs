--  Russian Peasant Multiplication
rpm :: Int -> Int -> Int
rpm m 0 = 0
rpm m n
    | (mod n 2 == 0)  = rpm (m*2) (div n 2)
    | otherwise     = m + rpm (m*2) (div n 2)
    
--  Mod with general recursion
rem2 :: Int -> Int -> Int
rem2 m n
    | (m < n) = m
    | otherwise = rem2 (m-n) n
    
notDiv :: Int -> Int -> Bool
notDiv d n = (mod n d /= 0)
    
--  test
test :: Int -> Int -> Int -> Bool
test a b c
    | a == b    = notDiv b c
    | otherwise = (notDiv a c) && (test (a+1) b c)
    
--  prime
prime :: Int -> Bool
prime n
    | n < 2     = False
    | n == 2    = True
    | otherwise = True

-- nextDetik
nextDetik :: (Int, Int, Int) -> (Int, Int, Int)
nextDetik (x, y, z) = ( (x+(div y 60 * div z 60)), y+(div (z+1) 60), mod (z+1) 60 )