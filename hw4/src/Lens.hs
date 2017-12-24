{-# LANGUAGE RankNTypes #-}

module Lens where

import           Data.Functor.Const    (Const (..))
import           Data.Functor.Identity (Identity (..), runIdentity)

type Lens s t a b = forall f . Functor f => (a -> f b) -> s -> f t
type Lens' s a  = Lens s s a a

set :: Lens' s a -> a -> s -> s
set lens value object = runIdentity $ lens (\_ -> Identity value) object

view :: Lens' s a -> s -> a
view lens object = getConst $ lens Const object

over :: Lens' s a -> (a -> a) -> s -> s
over lens func object = runIdentity $ lens (Identity . func) object

-- _1 :: Functor f => (a -> f b) -> (a, x) -> f (b, x)
_1 :: Lens (a, x) (b, x) a b
_1 f (a, x) = (\z -> (z, x)) <$> f a

-- _2 :: Functor f => (a -> f b) -> (x, a) -> f (x, b)
_2 :: Lens (x, a) (x, b) a b
_2 f (x, b) = (\ z -> (x, z)) <$> f b
