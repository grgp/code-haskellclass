curry :: ((a,b) -> c) -> a -> b -> c
curry g x y = g (x,y)

