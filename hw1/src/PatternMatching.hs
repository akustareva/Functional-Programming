module PatternMatching
       ( removeAt
       , removeAtBase
       , collectEvery
       , stringSum
       , randomIntList
       , mergeSort
       ) where

import           Data.List     (partition)
import           System.Random (newStdGen, randomRs)
import           Control.Arrow ((***))

removeAtBase :: Int -> [a] -> [a]
removeAtBase n xs = ys ++ if null zs then zs else tail zs
  where
    (ys, zs) = splitAt n xs

removeAt :: Int -> [a] -> (Maybe a, [a])
removeAt n xs
  | length xs > n && n >= 0 = (Just $ xs!!n, take n xs ++ drop (n + 1) xs)
  | otherwise               = (Nothing, xs)

collectEvery :: Int -> [a] -> ([a], [a])
collectEvery nth xs = (map snd *** map snd) parted
  where
    partitionFunc :: Int -> [a] -> ([(Int, a)], [(Int, a)])
    partitionFunc n = partition (\(x, _) -> mod x n /= 0) . zip [1..]
    parted = partitionFunc nth xs

stringSum :: String -> Int
stringSum str = sum $ map readFunc $ words str
  where
    readFunc :: String -> Int
    readFunc s = if head s == '+' then read $ tail s else read s

randomIntList :: Int -> Int -> Int -> IO [Int]
randomIntList n from to = take n . randomRs (from, to) <$> newStdGen

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
