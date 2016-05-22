-- 12. Deklarasikan fungsi isTerurutMenaik :: [Int] -> Bool yang mengembalikan nilai True jika list terurut menaik dan False jika sebaliknya. * terurut menaik semu: untuk input [2,2,2] akan mengebalikan True.

isTerurutMenaik :: [Int] -> Bool
isTerurutMenaik [] = False
isTerurutMenaik [a] = True
isTerurutMenaik (x:(y:ys))
  | x <= y     = isTerurutMenaik (y:ys)
  | otherwise = False

-- 13. Deklarasikan fungsi findMax :: [Int] -> Int yang mengembalikan nilai paling maksimal dari sebuah list. List diasumsikan mempunyai minimal 1 elemen. Anda bisa menggunakan local definitions disini, untuk mendefinisikan fungsi max2. Contoh : findMax [1,2,3,4,5,4,3,2,1] akan mengembalikan 5

findMax :: [Int] -> Int
findMax [a]     = a
findMax (x:(y:ys))
  | x <= y    = x + if findMax (y:ys) /= 0 then findMax (y:ys) else 0
  | otherwise = findMax (y:ys)


-- 14. Deklarasikan fungsi split :: [Int] -> ([Int], [Int]) sehingga split [x1,x2,x3,x4,x5, ...,xn] = ([x1,x3,x5, ...], [x2,x4,x6, ...]) Contoh : split [3,2,4,3,6,7,8] akan mengembalikan ([3,4,6,8], [2,3,7])

split :: [Int] -> ([Int], [Int])
split [] = [[], []]
split (x:y:xs) = [(x:jump2(xs)), (jump2(y:xs))]

jump2 :: [Int] -> [Int]
jump2 [] = []
jump2 x [] = x
jump2 (x:y:xs) = x : jump2 xs

-- 15. Misalkan, ada sebuah struktur data modified list yang mempunyai format berikut [(val1, count1), (val2, count2), (val3, count3), ..., (valn, countn)] Properti dari struktur data tersebut adalah: 
--  • Untuk setiap pair pada list, val bersifat unik
--  • count merupakan informasi ada berapa banyak val

-- Deklarasikan fungsi insert’ :: Int -> [(Int, Int)] -> [(Int, Int)] yang melakukan penyisipan val ke dalam list. Jika val sudah ada, maka count dari val tersebut akan dinaikkan +1. Jika val belum ada pada list, maka akan ditambahkan val tersebut ke dalam list dengan count awal = 1.

-- Contoh : 
-- insert’ 4 [(3,3), (4,2), (2,9)]) = [(3,3), (4,3), (2,9)]
-- insert’ 6 [(3,3), (4,2), (2,9)]) = [(3,3), (4,2), (2,9), (6,1)]