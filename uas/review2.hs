class Eq a => MyClass a where

  koo :: (Eq a, Eq b) => (a,b) -> Maybe a -> Maybe b
  koo _ _ = Nothing

  hoo :: MyClass a => (Bool,[a])
  hoo = (False, [])

--curry :: ((a,b) -> c) -> a -> b -> c
--loo :: a -> a

--foo goo :: (MyClass a, Eq a) => Maybe [a] -> Maybe [a]
--koo hoo :: (MyClass a, MyClass [a]) => Maybe Bool -> Maybe [Bool]

class Wht a where
  uncurry' :: (a -> b) -> (c, d) -> e
  id' :: a -> a

data Tree a = Nil | Node a (Tree a) (Tree a) deriving Show

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f Nil = Nil
mapTree f (Node a t1 t2) = Node (f a) (mapTree f t1) (mapTree f t2)

foldTree :: (a -> a -> a) -> Tree a -> a
foldTree f t = foldr1 f (foldTreeHelp t)

foldTreeHelp :: Tree a -> [a]
foldTreeHelp Nil = []
foldTreeHelp (Node a t1 t2) = [a] ++ foldTreeHelp t1 ++ foldTreeHelp t2

--get 2 (filter even (ff 1 2)) = 34, third index dmmit

tigaAksi :: (IO a, IO b, IO c) -> IO (a, b, c)
tigaAksi (a, b, c) = do ra <- a
                        rb <- b
                        rc <- c
                        return (ra, rb, rc)

test = do hasil <- tigaAksi (getLine, getLine, getLine)
          putStrLn $ "(" ++ (fstT3 hasil) ++ ","
                         ++ (sndT3 hasil) ++ ","
                         ++ (thdT3 hasil) ++ ")"

fstT3 :: (a,b,c) -> a
fstT3 (a,_,_) = a

sndT3 :: (a,b,c) -> b
sndT3 (_,b,_) = b

thdT3 :: (a,b,c) -> c
thdT3 (_,_,c) = c