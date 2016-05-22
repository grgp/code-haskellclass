import Prelude hiding (foldrl)

-- [SOAL 1]
sum' :: [Int] -> Int
sum' x = foldl (+) 0 x

-- [SOAL 2]
product' :: [Int] -> Int
product' x = foldl (*) 1 x

addNumo :: Int -> (Int -> Int)
addNumo n = addNumo where addNumo m = n+m

addNum :: Int -> Int
addNum n = (\"a" -> n + 3) "a"

addNum2 :: Int -> Int -> String -> Int
addNum2 m n o = (\"a" -> m + n) o

addNum3 :: Int -> Int -> Int
addNum3 m n = m + n + 3

addNum4 :: Int -> Int -> Int -> Int
addNum4 m n o = (\n o -> m + n + o) n o

comp2' :: (a -> b) -> (b -> b -> c) -> (a -> a -> c)
comp2' f g = (\x y -> g (f x) (f y))

filter' :: (c -> Bool) -> [c] -> [c]
filter' p xs = [ x | x <- xs, p x]

isEven :: Int -> Bool
isEven x = (mod x 2 == 0)

isEven' :: Int -> Bool
isEven' x = (x `mod` 2 == 0)

--foldr1 :: (a -> a -> a) -> [a] -> a
--foldr1 f [] = []
--foldr1 f (x:xs) = f x (foldr1 f xs)

--only if an empty list would return a value
--foldr' :: (a -> a -> a) -> a -> [a] -> a
foldr' f s [] = s
foldr' f s (x:xs) = f x (foldr f s xs)

-- [SOAL 3]
incMe :: Int -> Int -> Int
incMe x y = x^0 + y

length' :: [Int] -> Int
length' (x:xs) = foldr incMe 1 xs

length'' :: [Int] -> Int
length'' x = foldr (\x acc -> acc + 1) 0 x

-- [SOAL 4]
all' :: (a -> Bool) -> [a] -> Bool
all' f xs = foldr (\x acc -> acc && f x) True xs

-- [SOAL 5]
any' :: (a -> Bool) -> [a] -> Bool
any' f xs = foldr (\x acc -> acc || f x) False xs

-- [SOAL 6]
map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> (f x):acc) [] xs

-- [SOAL 7]
filter'' :: (a -> Bool) -> [a] -> [a]
filter'' f xs = foldr (\x acc -> (if f x == True then (x):acc else acc)) [] xs

-- [SOAL 8]
reverse' :: [a] -> [a]
reverse' xs =  foldr (\x acc -> acc ++ [x]) [] xs 

-- [SOAL 9]
flatten :: [[a]] -> [a]
flatten xs = foldr (\x acc -> x ++ acc) [] xs

-- [SOAL 10]


-- [SOAL 11]
loop :: Int -> (Int -> a) -> [a]
loop 0 f = []
loop i f = (f i):loop (i-1) f

-- [SOAL 12]
histogram :: [Int] -> String
histogram 

--foldr' :: (a -> a -> a) -> a -> [a] -> a
--foldr' f s [] = s
--foldr' f s (x:xs) = f x (foldr f s xs)

--(.) :: (b -> c) -> (a -> b) -> (a -> c)
--map' f xs = foldr (\y xs -> [f y] ++ xs) [] xs 