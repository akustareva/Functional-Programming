module NondeterministicCalculations
       ( bin
       , combinations
       , permutations
       ) where

bin :: Int -> [[Int]]
bin 0 = [[]]
bin n = bin (n - 1) >>= \set -> [0:set, 1:set]

combinations :: Int -> Int -> [[Int]]
combinations n k = filter (\set -> length set == k) $ subsets [1..n]
  where
    subsets :: [a] -> [[a]]
    subsets []     = [[]]
    subsets (x:xs) = subsets xs >>= \set -> [set, x:set]

permutations :: [a] -> [[a]]
permutations []     = [[]]
permutations (x:xs) = permutations xs >>= \set -> insert x set
  where
    insert :: a -> [a] -> [[a]]
    insert x set = map (\i -> take i set ++ [x] ++ drop i set) [0..length set]
