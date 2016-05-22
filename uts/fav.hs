twice :: (a -> a) -> (a -> a)
twice f = (f . f)

double :: Int -> Int
double x = x + x

triple :: Int -> Int
triple x = x + x + x

add' :: Int -> Int -> Int
add' x y = x + y

inc :: Int -> Int
inc x = x + 1

sum7 :: Int -> Int
sum7 = add' 7

sumTwo :: Int -> Int -> Int
sumTwo x y = x + y

uncurry' :: (a -> b -> c) -> ((a,b) -> c)
uncurry' g (x,y) = g x y

-- (inc . (+2)) 3 = 6   bisa
-- (inc . (+)) 3 5      tidak bisa, krn argumen func comp cuma 1

map (map (+2)) [[1,2,3], [1,2], [3]]
~> map (\xs -> map (+2) xs) [[1,2,3],[1,2],[3]]
 
foldr (jum 5) 0 [1,2,3,4]
       |-> (\x y -> 5 +x + y)