newtype Poly a = P [a]
x :: Num a => Poly a
x = P [0,1]

instance (Eq a, Num a) => Eq (Poly a) where
  P ps == P [] = all (==0) ps
  P [] == P qs = all (==0) qs 
  P (x:xs) == P (y:ys) = (x == y) && P xs == P ys

instance (Num a, Eq a, Show a) => Show (Poly a) where
  show (P []) = ""
  show (P ps) =
    showPoly 0 ps
    where
      showPoly _ [] = ""
      showPoly e (c:cs) =
        nextTermsStr ++ delim ++ curTermStr
          where
            curTermStr = showTerm c e
            nextTermsStr = showPoly (e+1) cs
            delim
              | null curTermStr || null nextTermsStr = ""
              | otherwise = " + "
      showTerm c e
        | c == 0 = ""
        | e == 0 = show c
        | (c == 1 && e == 1) = "x"
        | (c == -1 && e == 1) = "-x"
        | c == 1 = "x^" ++ show e
        | e == 1 = show c ++ "x"
        | otherwise = show c ++ "x^" ++ show e

plus :: Num a => Poly a -> Poly a -> Poly a
plus (P xs) (P ys) = (P (tambah xs ys))
  where
    tambah [] [] = []
    tambah xs [] = xs
    tambah [] ys = ys
    tambah (x:xs) (y:ys) = (x + y) : tambah xs ys

instance (Num a) => Num (Poly a) where
  (+) = plus
  (*) = times
  negate = negasi
  fromInteger = drInt

times :: (Num a) => Poly a -> Poly a -> Poly a
times (P xs) (P ys) = (P (multPoly xs ys)) 

multPoly :: (Num a) => [a] -> [a] -> [a]
multPoly [] [] = []
multPoly _ [] = []
multPoly [] _ = []
multPoly (p:p1) p2 = let pTimesP2 = map (p*) p2
                         xTimesP1Timesp2 = 0 : (multPoly p1 p2)
                     in addPoly pTimesP2 xTimesP1Timesp2 

addPoly p1 p2 = if (length p1 >= length p2)
                then zipWith (+) p1 (p2 ++ repeat 0)
                else addPoly p2 p1

negasi :: (Num a) => Poly a -> Poly a
negasi (P xs) = (P (map ((-1)*) xs))

drInt :: (Num a) => Integer -> Poly a
drInt x = (P ((fromIntegral x) : []))

applyP :: (Num a) => Poly a -> a -> a
applyP (P xs) y = applyHelper 0 xs y

applyHelper :: (Num a, Integral b) => b -> [a] -> a -> a
applyHelper p [] y = 0
applyHelper p (x:xs) y = x*(y^p) + (applyHelper (p+1) xs y)
