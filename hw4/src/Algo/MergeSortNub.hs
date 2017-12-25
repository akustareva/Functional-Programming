module Algo.MergeSortNub where

sNub :: (Ord a) => [a] -> [a]
sNub xs = removeDuplicates sorted
  where
    sorted = mergeSort xs
    removeDuplicates :: (Eq a) => [a] -> [a]
    removeDuplicates []  = []
    removeDuplicates [x] = [x]
    removeDuplicates (x:xs)
      | x == head xs = removeDuplicates xs
      | otherwise    = x : removeDuplicates xs

mergeSort :: (Ord a) => [a] -> [a]
mergeSort []  = []
mergeSort [z] = [z]
mergeSort zs  = merge (mergeSort $ take mid zs) (mergeSort $ drop mid zs)
  where
    mid = div (length zs) 2
    merge :: (Ord a) => [a] -> [a] -> [a]
    merge xs []         = xs
    merge [] ys         = ys
    merge (x:xs) (y:ys) = if x < y then x:merge xs (y:ys) else y:merge (x:xs) ys
