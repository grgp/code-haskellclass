newtype Queue a = Qu [a]

emptyQ :: Queue a
emptyQ = Qu []

addQ :: a -> Queue a -> Queue a
addQ x (Qu q) = Qu (q ++ [x])

remQ :: Queue a -> a -> (a, Queue a)
remQ (Qu q) x = ((last q) , Qu (init q))