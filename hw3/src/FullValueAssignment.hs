{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module FullValueAssignment
       ( updateVarsSet
       ) where

import           ArithmeticExpression
import           Control.Monad.Except   (MonadError, catchError, throwError)
import           Control.Monad.Reader   (runReaderT)
import           Control.Monad.State    (MonadState, get)
import           Data.Map               (Map, fromList, toList)
import           UpdateVariables
import           VariableCreationParser

updateVarsSet :: ( MonadState (Map String Integer)  m
                 , MonadError CustomError m
                 )
              => [Action] -> m ()
updateVarsSet [] = return ()
updateVarsSet (a:actns) = do
                          let stmt = getStmnt a
                          let stmtName = getStmtName stmt
                          let stmtExpr = getStmtValue stmt
                          mp <- get
                          let mp' = fromList $ convertIntToLit $ toList mp
                          varValue <- catchError (runReaderT (eval stmtExpr) mp') throwError
                          if isCreateAction a then addVar stmtName varValue
                                              else updateVar stmtName varValue
                          updateVarsSet actns

convertIntToLit :: [(String, Integer)] -> [(String, Expr)]
convertIntToLit []               = []
convertIntToLit ((name, val):xs) = (name, Lit val) : convertIntToLit xs
