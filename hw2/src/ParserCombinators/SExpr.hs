module ParserCombinators.SExpr
       ( zeroOrMore
       , oneOrMore
       , spaces
       , ident
       , Ident
       , Atom(..)
       , SExpr(..)
       , skipSpaces
       , parseAtom
       , parseSExpr
       ) where

import           Control.Applicative       ((<|>))
import           Data.Char                 (isSpace, isAlpha, isAlphaNum)
import           ParserCombinators.AParser

-- task 1
zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p

-- task 2
spaces :: Parser String
spaces = zeroOrMore (satisfy isSpace)

ident :: Parser String
ident = (:) <$> satisfy isAlpha <*> zeroOrMore (satisfy isAlphaNum)

-- task 3
-- An identifier is represented as just a String; the format for valid
-- identifiers is represented by the ident parser from previous task
type Ident = String

-- An “atom” is either an integer value (which can be parsed with posInt)
-- or an identifier
data Atom = N Integer | I Ident
  deriving (Show, Eq)

-- S-expression is either an atom, or a list of S-expressions
data SExpr = A Atom
           | Comb [SExpr]
  deriving (Show, Eq)

parseSExpr :: Parser SExpr
parseSExpr = parseComb <|> parseA
  where
    parseComb = Comb <$> (skipSpaces (char '(') *> zeroOrMore parseSExpr <* skipSpaces (char ')'))
    parseA = A <$> parseAtom

parseAtom :: Parser Atom
parseAtom = parseN <|> parseI
  where
    parseN = N <$> skipSpaces posInt
    parseI = I <$> skipSpaces ident

skipSpaces :: Parser a -> Parser a
skipSpaces p = spaces *> p <* spaces