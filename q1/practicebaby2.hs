-- 6. Definisikan fungsi removeEven :: [Int] -> [Int] yang membuang semua elemen genap pada sebuah list. Contoh : removeEven [1,2,3,4,5,6] akan engembalikan [1,3,5]

removeEven :: [Int] -> [Int]
removeEven [] = []
removeEven (x:xs)
  | mod x 2 /= 0 = x : removeEven xs
  | otherwise = removeEven xs

-- 7. Deklarasikan fungsi throwLastElmt : [a] -> [a] yang membuang elemen terakhir pada list. Perhatikan bahwa list masukan harus minimal punya 1 elemen. Contoh : throwLastElmt [1,2,3,4,5] akan mengembalikan [1,2,3,4]

throwLastElmt :: [a] -> [a]
throwLastElmt [a] = []
throwLastElmt (x:xs)
  | length xs /= 0  = x:throwLastElmt xs
  | otherwise       = []

-- 8. Deklarasikan fungsi getLastElmt : [a] -> a yang mengambil elemen terakhir dari sebuah list. Perhatikan bahwa list masukan harus minimal punya 1 elemen. Contoh : getLastElmt [1,2,3,4,5] akan mengembalikan 5

getLastElmt :: [a] -> a
getLastElmt [a]     = a
getLastElmt (x:xs)
  | length xs /= 0  = getLastElmt xs
  | otherwise       = xs!!0

-- 9. Deklarasikan fungsi removeFirst x ls yang mempunyai tipe Int -> [Int] -> [Int]. Fungsi akan menghapus kemunculan pertama x dalam list ls. Contoh : removeFirst 2 [1,2,3,2,4,2,5,2] akan mengembalikan [1,3,2,4,2,5,2]

removeFirst :: Int -> [Int] -> [Int]
removeFirst x [] = []
removeFirst x (y:ys)
  | x == y    = ys
  | otherwise = y : removeFirst x ys

--10. Deklarasikan fungsi addLast x ls yang bertipe Int -> [Int] -> [Int]. Fungsi ini akan menambahkan elemen x pada posisi terakhir di list ls, jika elemen x belum ada pada list. Jika x sudah ada pada list, maka x tidak akan ditambahkan ke dalam list. Contoh : 

--  addLast 4 [1,2,3] akan mengembalikan [1,2,3,4]
--  addLast 4 [1,4,2,3] akan mengembalikan [1,4,2,3]

addLast :: Int -> [Int] -> [Int]
addLast x [] = [x]
addLast x (y:ys)
  | x == y    = y : ys
  | otherwise = y : addLast x ys

-- 11. Deklarasikan fungsi insert x ls yang bertipe Int -> [Int] -> [Int]. Kita asumsikan list ls sudah terurut menaik. Fungsi insert akan menyisimpkan elemen x ke dalam list dengan tetap menjaga keterurutan dari list tersebut. Contoh : insert 5 [1,2,3,7,8,9] akan mengembalikan [1,2,3,5,7,8,9]

insert :: Int -> [Int] -> [Int]
insert x [] = [x]
insert x (y:ys)
  | y > x     = x : y : ys
  | otherwise = y : insert x ys
