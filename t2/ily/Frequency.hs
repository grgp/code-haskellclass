module Soal3.Frequency ( frequency ) where

import Soal2.PriorityQueue

frequency :: [Char] -> [(Char,Int)]
frequency x = map rotate (formation(henshin(jogres(urut(mulai x)))))

mulai :: [Char] -> [(Char,Int)]
mulai = map start
 where
 start ch = (ch,1)

urut :: [(Char,Int)] -> PrQueue (Char,Int)
urut x = build x

jogres :: PrQueue (Char,Int) -> PrQueue (Char,Int)
jogres tae
 | size tae < 2 = tae
 | ch1 == ch2 =  jogres (insert iblis (ch1,i1+i2))
 | otherwise = insert (jogres (insert iblis (ch2, i2))) (ch1,i1)
  where
  (setan,(ch1,i1)) = extractMin tae
  (iblis,(ch2,i2)) = extractMin setan
 
henshin :: PrQueue (Char,Int) -> PrQueue (Int,Char)
henshin anj = build (map rotate (formation anj))
 
formation :: Ord a => PrQueue a -> [a]
formation asu
 | size asu == 0 = []
 | otherwise = snd(extractMin asu):formation (fst(extractMin asu)) 

rotate :: (a,b) -> (b,a)
rotate x = (snd x,fst x) 
-- insert (fst (extractMin (fst (extractMin tae)))) (snd (snd (extractMin (fst (extractMin tae))))) + (snd (snd (extractMin tae)))