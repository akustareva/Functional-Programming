{-# LANGUAGE NoImplicitPrelude #-}

module BasicTypeClasses.ChaoticGood where

import           Control.Applicative (Applicative (..))
import           Data.Foldable       (Foldable (..))
import           Data.Functor        (Functor (..), (<$>))
import           Data.Monoid         (Monoid (..))
import           Data.Traversable    (Traversable (..))
import           Prelude             (($))

newtype Identity a = Identity { runIdentity :: a }

data Either a b  =  Left a | Right b

data Tree a = Leaf | Node a (Tree a) (Tree a)

newtype Const a b = Const { getConst :: a }

data Pair a b = Pair a b


instance Functor Identity where
  fmap f (Identity x) = Identity $ f x

instance Functor (Either a) where
  fmap f (Right x) = Right $ f x
  fmap _ (Left x)  = Left x

instance Functor Tree where
  fmap f (Node x l r) = Node (f x) (fmap f l) (fmap f r)
  fmap _ Leaf         = Leaf

instance Functor (Const a) where
  fmap _ (Const x) = Const x

instance Functor (Pair a) where
  fmap f (Pair x y) = Pair x $ f y


instance Applicative Identity where
  pure = Identity
  (Identity f) <*> x = fmap f x

instance Applicative (Either a) where
  pure = Right
  (Right f) <*> x = fmap f x
  (Left f) <*> _  = Left f

instance Applicative Tree where
  pure x = Node x Leaf Leaf
  Leaf <*> _         = Leaf
  (Node f _ _) <*> x = fmap f x

instance Monoid m => Applicative (Const m) where
  pure _ = Const mempty
  Const f <*> Const x = Const $ mappend f x

instance Monoid m => Applicative (Pair m) where
  pure = Pair mempty
  (Pair x f) <*> (Pair y z) = Pair (mappend x y) $ f z


instance Foldable Identity where
  foldMap f (Identity x) = f x

instance Foldable (Either a) where
  foldMap f (Right x) = f x
  foldMap _ (Left _)  = mempty

instance Foldable Tree where
  foldMap _ Leaf         = mempty
  foldMap f (Node x l r) = mappend (foldMap f l) $ mappend (f x) $ foldMap f r

instance Foldable (Const a) where
  foldr _ x _ = x

instance Foldable (Pair a) where
  foldMap f (Pair _ y) = f y


instance Traversable Identity where
  traverse f (Identity x) = Identity <$> f x
  
-- f :: Int -> Maybe Char
-- x :: [Int]
-- Maybe [Char]
-- :t traverse f x

instance Traversable (Either a) where
  traverse f (Right x) = Right <$> f x
  traverse _ (Left x)  = pure $ Left x

instance Traversable Tree where
  traverse _ Leaf         = pure Leaf
  traverse f (Node x l r) = Node <$> f x <*> traverse f l <*> traverse f r

instance Traversable (Const a) where
  traverse _ (Const x) = pure $ Const x

instance Traversable (Pair a) where
  traverse f (Pair x y) = Pair x <$> f y
