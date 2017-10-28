{-# LANGUAGE InstanceSigs #-}

module ParserCombinators.AParser
       ( Parser(..)
       , abParser
       , abParser_
       , intPair
       , intOrUppercase
       ) where

import           Control.Applicative (Alternative, empty, (<|>))
import           Control.Monad       (void)
import           Data.Char           (isDigit, isUpper)

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- task 1
instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f p = Parser $ \s -> first f <$> runParser p s

-- task 2
instance Applicative Parser where
  pure :: a -> Parser a
  pure a = Parser $ \s -> Just(a, s)

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  p1 <*> p2 = Parser $ \s -> let m1 = runParser p1 s
                             in  case m1 of
                                     Nothing      -> Nothing
                                     Just (f, r1) -> let m2 = runParser p2 r1
                                                     in case m2 of
                                                            Nothing      -> Nothing
                                                            Just (g, r2) -> Just(f g, r2)

-- task 3
abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'

abParser_ :: Parser ()
abParser_ = (\_ _ -> ()) <$> char 'a' <*> char 'b'

intPair :: Parser [Integer]
intPair = (\f _ s -> [f, s]) <$> posInt <*> char ' ' <*> posInt

-- task 4
instance Alternative Parser where
  empty :: Parser a
  empty = Parser $ const Nothing

  (<|>) :: Parser a -> Parser a -> Parser a
  p1 <|> p2 = Parser $ \s -> runParser p1 s <|> runParser p2 s

-- task 5
intOrUppercase :: Parser ()
intOrUppercase = const () <$> void (satisfy isUpper) <|> void posInt

-- functions from the document
first :: (a -> b) -> (a, c) -> (b, c)
first f (x, y) = (f x, y)

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing
    f (x:xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing

char :: Char -> Parser Char
char c = satisfy (== c)

posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs
