Type
	__monomorphic =
			'a' :: Char
			True :: Bool
			5 :: Int

	__polymorphic = 
			length :: [a] -> Int
				[Int] -> Int
				[(Int, Bool)] -> Int

	__constraint
			TypeClass member Eq a => a -> [a] -> Bool


Haskell is Strong Type yo
	Before run, check if
		Expression
		Definition
	already follows

Apa well typed?
f e 			e u
	e :: s -> t
	well typed ketika 
		1. s == u
		2. unifikasi s & u berhasil

Soal no 4 docx
	f (a [a]) -> a

	f :: (2, [3]) :: Int

	H1 Asumsi Literal 2,3
		monomorphic (Int)
		(Int, [Int])
		(a, [a])

	3 :: Num t => t
	Num t => (t, [t])
	(a, [a])

(Eq [b], Ord b) => ...
	bisa jadi
	member e :: Ord b => [b] -> Bool

--

curry :: ((a,b) -> c) -> a -> b -> c
curry g x y = g (x,y)

uncurry (+) ___> Num t => t -> t -> t
	 |
	 |
	 v
	 (a -> b -> c) -> (a, b) -> c

	Num t => (t, t) -> t

	(a -> b -> c)
	Num t => (t -> t -> t)

uncurry (+)
(+) :: Num a => a -> a -> a

uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry g (x, y) = g x y

uncurry (+) :: Num c => (c, c) -> c

zipWith (+) :: 

curry (uncurry (+))
zipWith (+)
((+) . (+) 1)
(max . (+1))
curry (uncurry id)
uncurry id
uncurry (uncurry id)
curry (curry id)
uncurry uncurry
uncurry curry
curry uncurry
(+) . (+) 1