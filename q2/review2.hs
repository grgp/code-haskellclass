class Eq a => MyClass a where
  foo :: Eq a => a -> Maybe a -> Maybe a
  goo :: MyClass a => [a]
  koo :: (Eq a, Eq b) => (a,b) -> Maybe a -> Maybe b
  hoo :: MyClass a => (Bool,[a])
  curry :: ((a,b) -> c) -> a -> b -> c
  loo :: a -> a

 