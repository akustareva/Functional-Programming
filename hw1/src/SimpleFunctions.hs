module SimpleFunctions
        ( order3
        , highestBitBase
        , highestBit
        , smartReplicate
        , contains
        ) where

import           Data.List (sort)

order3 :: (Ord a) => (a, a, a) -> (a, a, a)
order3 (x, y, z) = (\[e1, e2, e3] -> (e1, e2, e3)) $ sort [x, y, z]

highestBitBase :: Integer -> Integer
highestBitBase n
  | n == 1    = 1
  | otherwise = 2 * highestBitBase (div n 2)

highestBit :: Integer -> (Integer, Integer)
highestBit n
  | n == 1    = (1, 0)
  | otherwise = (2 * fst res, 1 + snd res)
  where
    res = highestBit (div n 2)

smartReplicate :: [Int] -> [Int]
smartReplicate = foldr (\x -> (++) (replicate x x)) []

contains :: (Eq a) => a -> [[a]] -> [[a]]
contains = filter . elem
