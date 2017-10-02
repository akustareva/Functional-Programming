module Monoids
       ( maybeConcat
       , NonEmpty(..)
       ) where

import           Data.Maybe     (Maybe (..), fromJust, isJust)
import           Data.Semigroup (Semigroup (..))

-- task 1
maybeConcat :: [Maybe [a]] -> [a]
maybeConcat []     = []
maybeConcat (x:xs)
  | isJust x  = fromJust x ++ maybeConcat xs
  | otherwise = maybeConcat xs

-- task 2
data NonEmpty a = a :| [a]
    deriving(Eq)

instance Semigroup (NonEmpty a) where
    (x:|xs) <> (y:|ys) = x :| (ys ++ [y] ++ xs)
