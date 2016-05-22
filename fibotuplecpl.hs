sqrNum :: Int -> [(Int, Int)]
sqrNum n = [(x, x*x) | x <- [0..n]]