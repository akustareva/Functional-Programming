{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeOperators      #-}

module OptionalMonad.PartialFunctions
       ( type(~>)
       , partial
       , total
       , apply
       ) where

import           Control.Applicative
import           Control.Category
import           Control.Monad
import           Prelude             hiding ((.))

data a ~> b
    = Partial   (a -> Maybe b) -- a partial function
    | Defaulted (a ~> b) b     -- a partial function with a default value

instance Category (~>) where
  id = Partial Just
  f . g = Partial $ apply g >=> apply f

partial :: (a -> Maybe b) -> a ~> b
partial = Partial

total :: (a -> b) -> a ~> b
total f = Partial (Just . f)

apply :: (a ~> b) -> a -> Maybe b
apply (Partial f) x      = f x
apply (Defaulted pf d) x = apply pf x <|> Just d
