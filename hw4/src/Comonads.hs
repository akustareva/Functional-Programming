{-# LANGUAGE MultiParamTypeClasses #-}

module Comonads where

import           Control.Comonad (Comonad (..))
import           Data.Monoid     (mappend)

---- Task 1 ----
data Renew s e a = Renew (e -> a) s

class Monoid e => MonoidAction s e where
    act :: s -> e -> s

instance Functor (Renew s e) where
    fmap f (Renew g s) = Renew (f . g) s

-- extract   :: w a -> a
-- duplicate :: w a -> w (w a)
-- extend    :: (w a -> b) -> w a -> w b

instance MonoidAction s e => Comonad (Renew s e) where
    extract (Renew f _) = f mempty
    duplicate (Renew f s) = Renew (\x -> Renew (\y -> f (mappend y x)) (act s x)) s
    extend f (Renew g s)  = Renew (\x -> f (Renew (\y -> g (mappend y x)) (act s x))) s

---- Task 3 ----
data Tree a = Node a [Tree a] deriving (Show)

instance Functor Tree where
    fmap f (Node v s) = Node (f v) $ map (fmap f) s

instance Comonad Tree where
    extract (Node v _  ) = v
    extend f n@(Node _ s) = Node (f n) (map (extend f) s)
