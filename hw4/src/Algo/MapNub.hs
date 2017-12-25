module Algo.MapNub where

import           Data.Map (empty, insert, member)

mNub :: (Ord a) => [a] -> [a]
mNub = go empty
  where
    go _ []        = []
    go m (x:xs)
      | member x m = go m xs
      | otherwise  = x : go (insert x 1 m) xs
