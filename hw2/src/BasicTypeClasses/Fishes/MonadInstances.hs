{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE UndecidableInstances #-}

module BasicTypeClasses.Fishes.MonadInstances where

import           BasicTypeClasses.Fishes.Monads
import           Prelude                        (id)

instance Monad m => MonadFish m where
  returnFish = return
  f >=> g = \x -> return x >>= f >>= g

instance Monad m => MonadJoin m where
  returnJoin = return
  join = (>>= id)
