data Stream a = Cons a (Stream a)

streamRepeat :: a -> Stream a
streamRepeat a = Cons a (streamRepeat a)

streamToList :: Stream a -> [a]
streamToList (Cons a s) = a : streamToList s

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons a ss) = Cons (f a) (streamMap f ss)

streamInterleave :: Stream a -> Stream a -> Stream a
streamInterleave (Cons a as) bs = Cons a (streamInterleave bs as)

--ruler :: Stream Integer
--ruler = streamInterleave 