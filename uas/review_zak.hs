
-- A (foo, goo, koo)
foo :: (Num a, Eq a) => a -> [a] -> [a]
foo x y = [x' | x' <- map (+3) y, x == x']

goo :: (Num a, Ord a) => a -> a -> [a] -> Bool
goo x y z = foo x z > foo y z

koo :: (Ord a) => a -> [a] -> [a]
koo x y = [x' | (x',i) <- zip y [1..z], x' < x]
   where z = length y

-- B (Segitiga)
data Segitiga = Segitiga {
                  a :: Double,
                  t :: Double
                }

-- C (Eq Segitiga)
instance (Eq Segitiga) where
  s1 == s2 = (a s1 * t s1) / 2 == (a s2 * t s2) / 2

-- D (Output [Segitiga 1 2, Segitiga 3 1] == [Segitiga 2 1, Segitiga 1 3])
-- Output : True (List dianggap sama bila elemen yang indeksnya bersesuaian dianggap sama)
testEqSegitiga :: Bool
testEqSegitiga = [Segitiga 1 2, Segitiga 3 1] == [Segitiga 2 1, Segitiga 1 3]

-- E (Visible Segitiga)
class Visible a where
  cetak :: a -> String

instance Visible Segitiga where
  cetak s = "seg a:" ++ show (a s) ++ " t:" ++ show (t s)

instance Visible a => Visible [a] where
  cetak []     = []
  cetak (x:xs) = cetak x ++ (if cetak xs == "" then "" else ", ") ++ cetak xs

-- F (MyClass)
class Eq a => MyClass a where

foo' :: Eq a => a -> Maybe a -> Maybe a
foo' _ _ = Nothing

goo' :: MyClass a => [a]
goo' = []

koo' :: (Eq a, Eq b) => (a,b) -> Maybe a -> Maybe b
koo' _ _ = Nothing

hoo' :: MyClass a => (Bool,[a])
hoo' = (False, [])

curry' :: ((a,b) -> c) -> a -> b -> c
curry' = curry

loo' :: a -> a
loo' = id

-- Tipe dari:
-- foo' goo' :: MyClass a => Maybe [a] -> Maybe [a]
-- koo' hoo' :: (MyClass a) => Maybe Bool -> Maybe [a]
-- curry koo' hoo' :: (MyClass a, Eq b) => b -> Maybe (Bool,[a]) -> Maybe b

-- G (Tree, mapTree, foldTree)
data Tree a = Nil | Node a (Tree a) (Tree a) deriving (Show)

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f Nil                   = Nil
mapTree f (Node val left right) = Node (f val) (mapTree f left) (mapTree f right)

foldTree1 :: (a -> a -> a) -> Tree a -> a
foldTree1 f (Node val Nil Nil)    = val
foldTree1 f (Node val left Nil)   = f val (foldTree1 f left)
foldTree1 f (Node val Nil right)  = f val (foldTree1 f right)
foldTree1 f (Node val left right) = f val (f (foldTree1 f left) (foldTree1 f right))

foldTree :: (a -> a -> a) -> a -> Tree a -> a
foldTree f v Nil = v
foldTree f v (Node val left right) = f val (f (foldTree f v left) (foldTree f v right))

-- H (Lazy evaluation)
filter' :: (a -> Bool) -> [a] -> [a]
filter' p [] = []
filter' p (x:xs)
  | p x       = x : filter p xs
  | otherwise = filter p xs

ff :: (Num a) => a -> a -> [a]
ff a b = a : ff b (a+b)

even' :: Integral a => a -> Bool
even' x = mod x 2 == 0

get :: Int -> [a] -> a
get 0 (x:_)  = x
get n (x:xs) = get (n-1) xs

-- get 2 (filter even (ff 1 2))
-- ~> get 2 (filter even (1:ff 2 (1+2))) (evaluasi)
-- ~> get 2 (filter even (ff 2 (1+2))) (pattern match filter' p (x:xs) -> otherwise)
-- ~> get 2 (filter even (2:ff (1+2) ((1+2)+2))) (evaluasi)
-- ~> get 2 (2:(filter even (ff (1+2) ((1+2)+2)))) (pattern match filter' p (x:xs) -> p x)
-- ~> get 1 (filter even (ff (1+2) ((1+2)+2))) (pattern match get n (x:xs))
-- ~> get 1 (filter even ((1+2):(ff ((1+2)+2) (((1+2)+2)+(1+2))))) (evaluasi)
-- ~> get 1 (filter even (3:(ff (3+2) ((3+2)+3)))) (evaluasi)
-- ~> get 1 (filter even (ff (3+2) ((3+2)+3))) (pattern match filter' p (x:xs) -> otherwise)
-- ~> get 1 (filter even ((3+2):ff ((3+2)+3) (((3+2)+3)+(3+2)))) (evaluasi)
-- ~> get 1 (filter even (5:ff (5+3) ((5+3)+5))) (evaluasi)
-- ~> get 1 (filter even (ff (5+3) ((5+3)+5))) (pattern match filter' p (x:xs) -> otherwise)
-- ~> get 1 (filter even ((5+3):(ff ((5+3)+5) (((5+3)+5)+(5+3))))) (evaluasi)
-- ~> get 1 (filter even (8:ff (8+5) ((8+5)+8))) (evaluasi)
-- ~> get 1 (8:(filter even (ff (8+5) ((8+5)+8)))) (pattern filter' p (x:xs) -> p x)
-- ~> get 0 (filter even (ff (8+5) ((8+5)+8))) (pattern match get n (x:xs))
-- ~> get 0 (filter even ((8+5):ff ((8+5)+8) (((8+5)+8)+(8+5)))) (evaluasi)
-- ~> get 0 (filter even (13:ff (13+8) ((13+8)+13))) (evaluasi)
-- ~> get 0 (filter even (ff (13+8) ((13+8)+13))) (pattern match filter' p (x:xs) -> otherwise)
-- ~> get 0 (filter even ((13+8):(ff ((13+8)+13) (((13+8)+13)+(13+8))))) (evaluasi)
-- ~> get 0 (filter even (21:ff (21+13) ((21+13)+21))) (evaluasi)
-- ~> get 0 (filter even (ff (21+13) ((21+13)+21))) (pattern match filter' p (x:xs) -> otherwise)
-- ~> get 0 (filter even ((21+13):(ff ((21+13)+21) (((21+13)+21)+(21+13))))) (evaluasi)
-- ~> get 0 (filter even (34:ff (34+21) ((34+21)+34))) (evaluasi)
-- ~> get 0 (34:filter even (ff (34+21) ((34+21)+34))) (pattern match filter' p (x:xs) -> p x)
-- ~> 34 (pattern match get 0 (x:_))

-- I (tigaAksi)
tigaAksi :: (IO a, IO b, IO c) -> IO (a,b,c)
tigaAksi (x,y,z) = do x' <- x
                      y' <- y
                      z' <- z
                      return (x',y',z')

test :: IO ()
test = do hasil <- tigaAksi (getLine, getLine, getLine)
          putStrLn $ show hasil