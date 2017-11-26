{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module UpdateVariables
       ( UpdateVarsError(..)
       , addVar
       , updateVar
       ) where

import           Control.Monad.Except (MonadError, throwError)
import           Control.Monad.State  (MonadState, get, modify)
import           Data.Map             (Map, insert, lookup)
import           Prelude              hiding (map, lookup)

data UpdateVarsError
    = VarAlreadyExists
    | VarNotDeclared
    | NoError
    deriving(Eq)

instance Show UpdateVarsError where
  show VarAlreadyExists = "Variable already exists"
  show VarNotDeclared   = "Variable is not declared"
  show NoError          = "OK"

addVar :: ( MonadState (Map String Integer)  m
          , MonadError UpdateVarsError m
          )
     => String -> Integer -> m ()
addVar name val = do
                  map <- get
                  case lookup name map of
                      Just _  -> throwError VarAlreadyExists
                      Nothing -> modify $ \map' -> insert name val map'

updateVar :: ( MonadState (Map String Integer)  m
          , MonadError UpdateVarsError m
          )
     => String -> Integer -> m ()
updateVar name val = do
                     map <- get
                     case lookup name map of
                        Just _  -> modify $ \map' -> insert name val map'
                        Nothing -> throwError VarNotDeclared
