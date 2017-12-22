{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module UpdateVariables
       ( addVar
       , updateVar
       ) where

import           CustomError
import           ArithmeticExpression
import           Control.Monad.Except (MonadError, throwError)
import           Control.Monad.State  (MonadState, get, modify)
import           Data.Map             (Map, insert, lookup)
import           Prelude              hiding (lookup, map)

addVar :: ( MonadState (Map String Integer)  m
          , MonadError CustomError m
          )
       => String -> Integer -> m ()
addVar name val = do
                  map <- get -- gets (lookup name) >>= \case -- LambdaCase
                  case lookup name map of
                      Just _  -> throwError VarAlreadyExists
                      Nothing -> modify $ insert name val

updateVar :: ( MonadState (Map String Integer)  m
             , MonadError CustomError m
             )
          => String -> Integer -> m ()
updateVar name val = do
                     map <- get
                     case lookup name map of
                        Just _  -> modify $ \map' -> insert name val map'
                        Nothing -> throwError VarNotDeclared
