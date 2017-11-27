{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module ConsoleInput
       ( ReadStatement(..)
       , readStmtParser
       , readStmt
       ) where

import           Control.Applicative  (empty)
import           Control.Monad.Except (MonadError)
import           Control.Monad.Reader (MonadIO, liftIO)
import           Control.Monad.State  (MonadState)
import           CustomError
import           Data.Map             (Map)
import           ExpressionParser
import           Text.Megaparsec.Expr (makeExprParser)
import           UpdateVariables

newtype ReadStatement = ReadStatement { name :: String }
    deriving(Show, Eq)

getStmtName :: ReadStatement -> String
getStmtName (ReadStatement name) = name

readStmtParser :: Parser ReadStatement
readStmtParser = makeExprParser term empty

term :: Parser ReadStatement
term = do
        _ <- symbol ">"
        var <- identifier
        return (ReadStatement var)

readStmt :: ( MonadState (Map String Integer)  m
            , MonadError CustomError m
            , MonadIO m
            )
         => ReadStatement -> m ()
readStmt stmt = do
                let stmtName = getStmtName stmt
                val <- liftIO readLn
                addVar stmtName val
