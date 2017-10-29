{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE UndecidableInstances #-}

module BasicTypeClasses.Fishes.MonadFishInstances where

import           BasicTypeClasses.Fishes.Monads
import           Prelude                        (id)

instance MonadFish m => Monad m where
  return = returnFish
  m >>= f = (id >=> f) m

instance MonadFish m => MonadJoin m where
  returnJoin = returnFish
  join = id >=> id
