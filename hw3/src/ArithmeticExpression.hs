module ArithmeticExpression
       ( Expr(..)
       , MyError(..)
       , eval
       ) where

import           Control.Monad        (liftM2)
import           Control.Monad.Reader (Reader, ask, local)
import           Data.Map             (Map, insert, lookup)
import           Prelude              hiding (lookup)

data Expr
    = Lit Int
    | Var String
    | Add Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
    | Div Expr Expr
    | Let String Expr Expr
    deriving(Show)

data MyError
    = DivByZero
    | IncorrectInputMap
    deriving(Eq)

instance Show MyError where
  show DivByZero         = "Division by zero"
  show IncorrectInputMap = "Not all variables have assigned values"

eval :: Expr -> Reader (Map String Expr) (Either MyError Int)
eval (Lit x)             = return $ Right x
eval (Var name)          = ask >>= \m -> let val = lookup name m
                                         in maybe (return $ Left IncorrectInputMap) eval val
eval (Add x y)           = eval x >>= \xres -> eval y >>= \yres -> return $ liftM2 (+) xres yres
eval (Sub x y)           = eval x >>= \xres -> eval y >>= \yres -> return $ liftM2 (-) xres yres
eval (Mul x y)           = eval x >>= \xres -> eval y >>= \yres -> return $ liftM2 (*) xres yres
eval (Div x y)           = eval x >>= \xres -> eval y >>= \yres -> return $ safeDiv xres yres
eval (Let name val expr) = local (insert name val) (eval expr)

safeDiv :: Either MyError Int -> Either MyError Int -> Either MyError Int
safeDiv err@(Left _) _      = err
safeDiv _ err@(Left _)      = err
safeDiv _ (Right 0)         = Left DivByZero
safeDiv (Right x) (Right y) = Right $ x `div` y
