module VariableCreationParser
       ( Statement(..)
       , Action(..)
       , actionParser
       , crtParser
       , updParser
       , stmtParser
       ) where

import           ArithmeticExpression
import           Control.Applicative  (empty)
import           ExpressionParser
import           Text.Megaparsec      ((<|>))
import           Text.Megaparsec.Expr (makeExprParser)

data Statement = Statement { name :: String, value :: Expr}
    deriving(Show, Eq)

data Action
    = Create Statement
    | Update Statement
    deriving(Show, Eq)

actionParser :: Parser Action
actionParser = makeExprParser term empty

term :: Parser Action
term = crtParser
    <|> updParser

crtParser :: Parser Action
crtParser = do
            sword "mut"
            stmt <- stmtParser
            return (Create stmt)

updParser :: Parser Action
updParser = do
            stmt <- stmtParser
            return (Update stmt)

stmtParser :: Parser Statement
stmtParser = do
            name <- identifier
            _ <- symbol "="
            value <- exprParser
            return (Statement name value)
