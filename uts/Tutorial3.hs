import Data.List

--Function 1
sum' :: [Int] -> Int
sum' x = foldl (+) 0 x

--Function 2
product' :: [Int] -> Int
product' x = foldl (*) 1 x

--Function 3
length' :: [Int] -> Int
length' = foldr (\_ n -> 1 + n) 0  

--Function 4
all' :: (a -> Bool) -> [a] -> Bool
all' f l = foldr (\n b -> b && f n) True l

all'' :: (a -> Bool) -> [a] -> Bool
all'' f l = foldr (&&) True (map f l)

all''' :: (Eq a) => (a -> Bool) -> [a] -> Bool
all''' f l = (filter f l) == l

--Function 5
any' :: (a -> Bool) -> [a] -> Bool
any' f l = foldr (||) False (map f l)

--Function 6
map' :: (a -> b) -> [a] -> [b]
map' f l = foldr (\n a -> a ++ [f n]) [] l

--Function 7
filter' :: (a -> Bool) -> [a] -> [a]
filter' f l = foldr (\n a -> (if ((f n) == True) then [n] else []) ++ a) [] l

--Function 8
reverse' :: [a] -> [a]
reverse' l = foldr (\n a -> a ++ [n]) [] l

--Function 9
flatten :: [[a]] -> [a]
flatten ll = foldr (\n a -> n ++ a) [] ll

--Function 10
sumEvenCollatz :: Int -> Int
sumEvenCollatz x = sum' (filter even (takeWhile (/= 1) (iterate ((\y -> if (even y) then (div y 2) else (3 * y + 1))) x)))

--Function 11
loop :: Int -> (Int -> a) -> [a]
loop 1 f = [f 1]
loop x f = (loop (x-1) f) ++ [(f x)]

--Function 12
--histogram :: [Int] -> String
--histogram l = map 

--histoGen :: Int -> String

--genBottom :: String
--genBottom = "\n==========\n0123456789" 

--Function 13
cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = flatten (cartProdHelp xs ys)

cartProdHelp :: [a] -> [b] -> [[(a, b)]]
cartProdHelp xs ys = [(cartProdHelper x ys) | x <- xs]

cartProdHelper :: a -> [b] -> [(a, b)]
cartProdHelper x ys = [(x, y) | y <- ys]

sieveSundaram :: Int -> [Int]
sieveSundaram x = map (\n -> 2*n + 1) ([1..x] \\ (map (\(a, b) -> a + b + 2*a*b) filtered))
                where filtered = filter (\(a, b) -> a + b + 2*a*b <= x && 1 <= a && (a <= b)) (cartProd [1..x] [1..x])