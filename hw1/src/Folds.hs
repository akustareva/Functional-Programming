module Folds
       ( splitOn
       , joinWith
       ) where

import           ADTWorld.Trees (Tree (..), insertKey)

-- task 1
instance (Ord a) => Monoid (Tree a) where
    mempty                    = Leaf
    mappend x Leaf            = x
    mappend Leaf y            = y
    mappend node (Node x l r) = mappend (mappend (insertKey node x) l) r

instance Foldable Tree where
    foldr _ z Leaf         = z
    foldr f z (Node x l r) = foldr f (f x (foldr f z r)) l
    foldMap _ Leaf         = mempty
    foldMap f (Node x l r) = mappend (foldMap f l) (mappend (f x) (foldMap f r))

-- task 2
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn del = foldr (\d (x:xs) -> if d == del then []:x:xs else (d:x):xs) [[]]

joinWith :: Eq a => a -> [[a]] -> [a]
joinWith del xs = foldr (\(x, i) res -> x ++ if i == (length xs - 1) then res else del:res) [] (zip xs [1..])
