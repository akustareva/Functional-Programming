{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module ConsoleOutput
       ( WriteStatement(..)
       , getStmtValue
       , writeStmtParser
       , printResult
       ) where

import           ArithmeticExpression
import           Control.Applicative  (empty)
import           Control.Monad.Except (MonadError, catchError, throwError)
import           Control.Monad.Reader (MonadIO, liftIO, runReaderT)
import           Control.Monad.State  (MonadState, get)
import           CustomError
import           Data.Map             (Map, fromList, toList)
import           ExpressionParser
import           FullValueAssignment
import           Text.Megaparsec.Expr (makeExprParser)

newtype WriteStatement = WriteStatement { value :: Expr }
    deriving(Show, Eq)

getStmtValue :: WriteStatement -> Expr
getStmtValue (WriteStatement val) = val

writeStmtParser :: Parser WriteStatement
writeStmtParser = makeExprParser term empty

term :: Parser WriteStatement
term = do
        _ <- symbol "<"
        expr <- exprParser
        return (WriteStatement expr)

printResult :: ( MonadState (Map String Integer)  m
               , MonadError CustomError m
               , MonadIO m
               )
            => WriteStatement -> m ()
printResult stmt = do
                   let stmtExpr = getStmtValue stmt
                   mp <- get
                   let mp' = fromList $ convertIntToLit $ toList mp
                   val <- catchError (runReaderT (eval stmtExpr) mp') throwError
                   liftIO $ print val
