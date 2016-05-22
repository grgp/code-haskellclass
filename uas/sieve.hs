from :: Int -> [Int]
from k = k : from (k + 1)

hasil :: [Int]
hasil = take 5 (filter (\x -> mod x 2 /= 0) (from 1))

sieve :: [Int] -> [Int]
sieve (x:xs) = x : (sieve (filter (\y -> mod y x /= 0) xs))

primes :: [Int]
primes = sieve (from 2)

fibo :: [Int]
fibo = fiboH 0 1

fiboH :: Int -> Int -> [Int]
fiboH a b = a : fiboH b (a + b)

interleave :: [Int] -> [Int] -> [Int]
interleave (x:xs) (y:ys) = x : y : interleave xs ys