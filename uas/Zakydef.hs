module Zakydef (
	from,
	get,
	zipWith,
	double,
	incr,
	gg,
	ff
) where

import Prelude hiding (zipWith)

from n = n : from (n+1)

get 0 (x:_) 	= x
get n (x:xs)	= get (n-1) xs

zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys
zipWith f _ _ = []

double x = x + x

incr x = x + 1

gg x y = y + y

ff x y
	| x < 0			= y
	| otherwise = x