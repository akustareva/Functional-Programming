module UpdateVariables
       ( UpdateVarsError(..)
       , addVar
       , updateVar
       ) where

import           Control.Monad.State (State, state)
import           Data.Map            (Map, insert, lookup, member)
import           Data.Maybe          (isNothing)
import           Prelude             hiding (lookup)

data UpdateVarsError
    = VarAlreadyExists
    | VarNotDeclared
    | NoError
    deriving(Eq)

instance Show UpdateVarsError where
  show VarAlreadyExists = "Variable already exists"
  show VarNotDeclared   = "Variable is not declared"
  show NoError          = "OK"

addVar :: String -> Int -> State (Map String Int) UpdateVarsError
addVar name val = state $ \context -> if member name context then (VarAlreadyExists, context)
                                      else (NoError, insert name val context)

updateVar :: String -> Int -> State (Map String Int) UpdateVarsError
updateVar name val = state $ \context -> let oldVal = lookup name context in
                                         if isNothing oldVal then (VarNotDeclared, context)
                                         else (NoError, insert name val context)
