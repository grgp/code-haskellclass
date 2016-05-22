newtype Poly a = P [a]

x :: Num a => Poly a
x = P [0, 1]

instance (Eq a, Num a) => Eq (Poly a) where
  (P [])    == (P [])     = True
  (P [])    == (P ys)   = all (==0) ys
  (P xs)  == (P [])     = all (==0) xs
  (P (x:xs))== (P (y:ys)) = (x == y) && ((P xs) == (P ys))

instance (Num a, Eq a, Show a) => Show (Poly a) where
  show (P []) = "0"
  show (P ps) = showPoly 0 ps
    where
      showPoly _ [] = ""
      showPoly e (c:cs) = nextTermsStr ++ delim ++ curTermStr
        where
          curTermStr = showTerm c e
          nextTermsStr = showPoly (e+1) cs
          delim
            | null curTermStr || null nextTermsStr = ""
            | otherwise = " + "
      showTerm c e
        | c == 0 = ""
        | e == 0 = show c
        | c == 1 && e == 1 = "x"
        | c == -1 && e == 1 = "-x"
        | c == 1 = "x^"
        | e == 1 = show c ++ "x"
        | otherwise = show c ++ "x^" ++ show e

instance Num a => Num (Poly a) where
  (+) = plus

plus :: Num a => Poly a -> Poly a -> Poly a
plus (P xs) (P ys) = (P (tambah xs ys))
  where
     tambah [] [] = []
     tambah xs [] = xs
     tambah [] xs = xs
     tambah (x:xs) (y:ys) = (x+y) : tambah xs ys

negate :: Num a => Poly a -> Poly a
