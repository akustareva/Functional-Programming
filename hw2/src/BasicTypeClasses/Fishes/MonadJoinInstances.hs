{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE UndecidableInstances #-}

module BasicTypeClasses.Fishes.MonadJoinInstances where

import           BasicTypeClasses.Fishes.Monads
import           Prelude                        (($))

instance (Functor m, MonadJoin m) => Monad m where
  return = returnJoin
  m >>= f = join $ fmap f m

instance (Functor m, MonadJoin m) => MonadFish m where
  returnFish = returnJoin
  f >=> g = \x -> join $ fmap g (f x)
