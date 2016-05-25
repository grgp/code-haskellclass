module Data.AVLTree (AVLTree (Nil, Node),
										 empty,
										 singleton
										) where

data AVLTree a
		= Nil | Node {value  :: a,
									left   :: AVLTree a,
									right  :: AVLTree a,
									height :: Int}

empty :: AVLTree a
empty = Nil

singleton :: a -> AVLTree a
singleton x = Node {value = x, left = Nil, right = Nil, height = 0}

insert :: Ord a => AVLTree a -> a -> AVLTree a
insert t a = 

fromList :: Ord a => [a] -> AVLTree a
fromList [] = empty
fromList (x:xs) = insert (fromList xs) x