newtype Queue a = Qu [a] [a] deriving (Show)

emptyQ :: Queue a
emptyQ = Qu [] []

isEmptyQ :: Queue a -> Bool
isEmptyQ (Qu [] []) = True
isEmptyQ _          = False

addQ   :: a -> Queue a -> Queue a
addQ x (Qu xs ys) = Qu xs (x:ys)

remQ   :: Queue a -> (  a , Queue a )
remQ (Qu (x:xs) ys)   = (x , Qu xs ys)
remQ (Qu [] (y:ys))   = remQ (Qu (reverse (y:ys)) [])
remQ (Qu [] [])       = error "remQ"