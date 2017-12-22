module VariableCreationParser
       ( Statement(..)
       , getStmtName
       , getStmtValue
       , Action(..)
       , isCreateAction
       , getStmnt
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

getStmtName :: Statement -> String
getStmtName (Statement name _) = name

getStmtValue :: Statement -> Expr
getStmtValue (Statement _ value) = value

data Action
    = Create Statement
    | Update Statement
    deriving(Show, Eq)

isCreateAction :: Action -> Bool
isCreateAction Create{} = True
isCreateAction _        = False

getStmnt :: Action -> Statement
getStmnt (Create stmt) = stmt
getStmnt (Update stmt) = stmt

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
updParser = do -- Update <$> stmtParser
            stmt <- stmtParser
            return (Update stmt)

stmtParser :: Parser Statement
stmtParser = do
            name <- identifier
            _ <- symbol "="
            value <- exprParser
            return (Statement name value)
