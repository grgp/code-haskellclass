--0. Reverse
rev :: [a] -> [a]
rev [] = []
rev (x:xs) = (rev xs) ++ [x]

--1. Definisikan fungsi evenN :: Int -> [Int] sehingga evenN n akan menghasilkan list yang berisi “n bilangan genap pertama yang positif” (n >= 0) Contoh: evenN 5 akan menghasilkan [2,4,6,8,10]

evenN :: Int -> [Int]
evenN 0 = []
evenN x = evenN (x-1) ++ [2*x]


--2. Definisikan fungsi doubleList [Int]-> [Int] sehingga doubleList [x1, x2, x3, ..., xn] = [2*x1, 2*x2, 2*x3, ..., 2*xn] ContohdoubleList [1,2,3,4] akan mengembalikan [2,4,6,8]

doubleList :: [Int] -> [Int]
doubleList [] = []
doubleList (x:xs) = x*2 : doubleList xs

--3. Definisikan fungsi numOdd :: [Int] -> Int yang mengembalikan nilai banyaknya bilangan ganjil dalam sebuah list of integers. Contoh : numOdd [2,3,4,5,6,7,8] akan mengembalikan 3

numOdd :: [Int] -> Int
numOdd [] = 0
numOdd (x:xs) = (if mod x 2 == 1 then 1 else 0) + numOdd xs

-- 4. Definisikan fungsi multiplicity x ls yang bertipe Int -> [Int] -> Int. Fungsi ini mengembalikan nilai banyaknya kemunculan elemen x di dalam list ls. Contoh : multiplicity 2 [2,3,4,2,5,2,6,2] akan mengembalikan 4

multiplicity :: Int -> [Int] -> Int
multiplicity x [] = 0
multiplicity x (l:ls) = (if x == l then 1 else 0) + multiplicity x ls

-- 5. Definisikan fungsi listParity ls :: [a] -> Bool yang mengembalikan True jika panjang list adalah genap dan mengembalikan False jika sebaliknya. Hint: agar lebih elegan, coba gunakan operasi not dalam hal ini. Contoh : listParity [3,2,4,5] akan mengembalikan True

listParity :: [a] -> Bool
listParity [] = True
listParity (x:xs) = True && not (listParity xs)