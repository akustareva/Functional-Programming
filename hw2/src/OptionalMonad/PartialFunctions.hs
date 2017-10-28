{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeOperators      #-}

module OptionalMonad.PartialFunctions
       ( type(~>)
       , partial
       , total
       , apply
       ) where

import           Control.Applicative ((<|>))
import           Control.Category    (Category(..))
import           Control.Monad       ((>=>))
import           Data.Maybe          (fromMaybe, isNothing)
import           Prelude             hiding (id, (.))

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

applyOrElse :: (a ~> b) -> a -> b -> b
applyOrElse pf x d = fromMaybe d $ apply pf x

withDefault :: (a ~> b) -> b -> (a ~> b)
withDefault pf@(Partial _) d   = Defaulted pf d
withDefault (Defaulted pf _) d = Defaulted pf d

isDefinedAt :: (a ~> b) -> a -> Bool
isDefinedAt (Partial f) x      = isNothing $ f x
isDefinedAt (Defaulted pf _) x = isDefinedAt pf x

orElse :: (a ~> b) -> (a ~> b) -> a ~> b
orElse (Partial f) (Partial g) = Partial $ \x -> f x <|> g x
orElse (Defaulted pf d) (Defaulted pg _) = Defaulted (orElse pf pg) d
orElse pf@(Partial _) (Defaulted pg d)   = Defaulted (orElse pf pg) d
orElse (Defaulted pf d) pg@(Partial _)   = Defaulted (orElse pf pg) d

-- Proofs of category laws for (~>) using Equational reasoning technique:
--
-- Left identity: id . f ≡ f
-- id . f = Partial $ apply f >=> apply id
--        = Partial $ apply f >=> Just
--        = (*)
--  1. f ≡ Partial f'
--    (*) = Partial $ f' >=> Just
--        = Partial $ f'
--        = f
--  2. f ≡ Defaulted pf d
--    2.1 apply pf x ≡ Nothing ⇒ apply pf x <|> Just d ≡ Just d
--       (*) = Partial $ Just >=> Just
--           = Partial $ Just
--    2.2 otherwise ⇒ apply pf x <|> Just d ≡ apply pf x
--       (*) = Partial $ apply pf >=> Just
--           = ...
--
-- Right identity: f . id ≡ f
-- f . id = Partial $ apply id >=> apply f
--        = Partial $ Just >=> apply f
--        = (*)
--  1. f ≡ Partial f'
--    (*) = Partial $ Just >=> f'
--        = Partial $ f'
--        = f
--  2. f ≡ Defaulted pf d
--    2.1 apply pf x ≡ Nothing
--       (*) = Partial $ Just >=> Just
--           = Partial $ Just
--    2.2 otherwise
--       (*) = Partial $ Just >=> apply pf
--           = ...
--
-- Association: h . (g . f) = (h . g) . f
-- h . (g . f) = h . (Partial $ apply f >=> apply g) = (*)
-- (h . g) . f = (Partial $ apply g >=> apply h) . f = (**)
-- 1. f ≡ Partial f'
--    g ≡ Partial g'
--    h ≡ Partial h'
--   (*) = h . (Partial $ f' >=> g')
--       = Partial $ apply (Partial $ f' >=> g') >=> apply h
--       = Partial $ (f' >=> g') >=> h'
--       = Partial $ f' >=> g' >=> h'
--   (**) = (Partial $ g' >=> h') . f
--        = Partial $ apply f >=> apply (Partial $ g' >=> h')
--        = Partial $ f' >=> (g' >=> h')
--        = Partial $ f' >=> g' >=> h'
-- 2. f ≡ Partial f'
--    g ≡ Partial g'
--    h ≡ Defaulted ph d
--   (*) = h . (Partial $ f' >=> g')
--       = Partial $ apply (Partial $ f' >=> g') >=> apply h
--       = Partial $ (f' >=> g') >=> apply h = (***)
--   (**) = (Partial $ g' >=> apply h) . f
--        = Partial $ apply f >=> apply (Partial $ g' >=> apply h)
--        = Partial $ f' >=> apply (Partial $ g' >=> apply h) 
--        = Partial $ f' >=> (g' >=> apply h) = (****)
--   2.1 apply ph x ≡ Nothing ⇒ apply ph x <|> Just d ≡ Just d
--      (***) = Partial $ (f' >=> g') >=> Just
--            = Partial $ f' >=> g'
--      (****) = Partial $ f' >=> (g' >=> Just)
--             = Partial $ f' >=> g'
--   2.2 otherwise ⇒ apply pf x <|> Just d ≡ apply pf x
--      (***) = Partial $ (f' >=> g') >=> apply ph
--            = Partial $ f' >=> g' >=> apply ph
--      (****) = Partial $ f' >=> (g' >=> apply ph)
--             = Partial $ f' >=> g' >=> apply ph
-- 3. ...
