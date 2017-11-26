{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module ArithmeticExpression
       ( Expr(..)
       , CustomError(..)
       , eval
       ) where

import           Control.Monad        (liftM2)
import           Control.Monad.Except (MonadError, throwError)
import           Control.Monad.Reader (MonadReader, ask, local)
import           Data.Map             (Map, insert, lookup)
import           Prelude              hiding (lookup)

data Expr
    = Lit Integer
    | Var String
    | Add Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
    | Div Expr Expr
    | Let String Expr Expr
    deriving(Show, Eq)

data CustomError
    = DivByZero
    | IncorrectInputMap
    | VarAlreadyExists
    | VarNotDeclared
    deriving(Eq)

instance Show CustomError where
  show DivByZero         = "Division by zero"
  show IncorrectInputMap = "Not all variables have assigned values"
  show VarAlreadyExists  = "Variable already exists"
  show VarNotDeclared    = "Variable is not declared"

eval :: ( MonadReader (Map String Expr)  m
        , MonadError  CustomError m
        )
     => Expr -> m Integer
eval (Lit x)             = return x
eval (Var name)          = ask >>= \m -> let val = lookup name m
                                         in maybe (throwError IncorrectInputMap) eval val
eval (Add x y)           = liftM2 (+) (eval x) (eval y)
eval (Sub x y)           = liftM2 (-) (eval x) (eval y)
eval (Mul x y)           = liftM2 (*) (eval x) (eval y)
eval (Div x y)           = eval x >>= \xres -> eval y >>= \yres -> safeDiv xres yres
eval (Let name val expr) = local (insert name val) (eval expr)

safeDiv :: MonadError  CustomError m => Integer -> Integer -> m Integer
safeDiv _ 0 = throwError DivByZero
safeDiv x y = return $ x `div` y
