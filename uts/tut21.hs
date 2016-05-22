headOr :: Int -> [Int] -> Int
headOr q [] = q
headOr q (x:xs) = if (x == q) then q else headOr q xs

sum' :: [Int] -> Int
sum' [] = 0
sum' (x:xs) = x + sum' xs

length' :: [Int] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs

product' :: [Int] -> Int
product' [] = 0
product' [a] = a
product' (x:xs) = x * product' xs

toDigits :: Int -> [Int]
toDigits 0 = []
toDigits x = 
  ( x `div` 10 ^ ((length "123") - 1) ) :
  ( x `mod` 10 ^ ((length "123") - 1) ) : []

digs :: Int -> [Int]
digs 0 = []
digs x = digs (x `div` 10) ++ [x `mod` 10]

reverse' :: [Int] -> [Int]
reverse' (x:xs) = xs ++ [x]

doubleEveryOther :: [Int] -> [Int]
doubleEveryOther [] = []
doubleEveryOther (x:[]) = [x]
doubleEveryOther (x:y:xs) = x : (y*2) : doubleEveryOther xs

localMaxima :: [Int] -> [Int]
localMaxima [] = []
localMaxima (x:y:[])
  | y >= x = [y]
  | otherwise = []
localMaxima (x:y:z:xs)
  | x < y && y > z = y : (localMaxima (z:xs))
  | otherwise = localMaxima (y:z:xs)

--all' :: (a -> Bool) -> [Int] -> Bool

toDigits' :: Int -> [Int]
toDigits' 0 = []
toDigits' x  = (x `mod` 10):(toDigits' (x `div` 10))

localMaxima' :: [Int] -> [Int]
localMaxima' [] = []
--localMaxima (x:xs) = 

doubleEveryOther' :: [Int] -> [Int]
doubleEveryOther' [] = []
doubleEveryOther' (x:y:zs) = x:(y*2):(doubleEveryOther' zs)