module ExpressionParser
       ( sc
       , lexeme
       , symbol
       , parens
       , sword
       , integer
       , identifier
       , letParser
       , exprParser
       ) where

import           ArithmeticExpression
import           Control.Applicative        (empty, (*>), (<$>))
import           Data.Void                  (Void)
import           Text.Megaparsec            (Parsec, between, many,
                                             notFollowedBy, try, (<|>))
import           Text.Megaparsec.Char       (alphaNumChar, letterChar, space1,
                                             string)
import qualified Text.Megaparsec.Char.Lexer as L (decimal, lexeme, space,
                                                  symbol)
import           Text.Megaparsec.Expr       (Operator (..), makeExprParser)

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

sword :: String -> Parser ()
sword w = lexeme (string w *> notFollowedBy alphaNumChar)

integer :: Parser Integer
integer = lexeme L.decimal

identifier :: Parser String
identifier = (lexeme . try) ((:) <$> letterChar <*> many alphaNumChar)

exprParser :: Parser Expr
exprParser = makeExprParser term operators

term :: Parser Expr
term = parens (letParser <|> exprParser)
  <|> Lit <$> integer
  <|> Var <$> identifier

letParser :: Parser Expr
letParser = do
            sword "let"
            name <- identifier
            _ <- symbol "="
            val <- exprParser
            sword "in"
            expr <- exprParser
            return (Let name val expr)

operators :: [[Operator Parser Expr]]
operators =
  [ [ InfixL (symbol "*" *> pure Mul)
    , InfixL (symbol "/" *> pure Div) ]
  , [ InfixL (symbol "+" *> pure Add)
    , InfixL (symbol "-" *> pure Sub) ]
  ]
