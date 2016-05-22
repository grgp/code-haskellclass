-- fibotuple

f :: Int -> Bool
g :: Double -> Int

a :: Double -> Bool
a x = f (g x)

isEven :: Int -> Bool
isEven x = (mod x 2 == 0)

allEven :: [Int] -> Bool
allEven xs = (xs == [x | x<-xs, isEven x])

allOdd :: [Int] -> Bool
allOdd xs = ([] == [x | x<-xs, isEven x])

-- latihan list comprehension

divisors :: Int -> [Int]
divisors n =
	[x | x<-[1..0] mod n x == 0]

matches :: Int -> [Int] -> [Int]
matches n xs = [x | x<-xs, x==n]

isElem n xs (matches n xs /= 0)

double :: [Int] -> [Int]
double [[1,2], [2], [3,5]] = [[2,4],[4],[6,10]]
double x xs = [ [y*2 | y<-x] | x <-xs ]

