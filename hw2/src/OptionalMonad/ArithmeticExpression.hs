module OptionalMonad.ArithmeticExpression
       ( Expr(..)
       , ArithmeticError(..)
       , eval
       , safeDiv
       ) where

data Expr
    = Const Int
    | Add Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
    | Div Expr Expr
    | Pow Expr Expr
    deriving(Show)

data ArithmeticError
    = DivByZero
    deriving(Eq)

instance Show ArithmeticError where
  show DivByZero = "Division by zero"

eval :: Expr -> Either ArithmeticError Int
eval (Const x) = return x
eval (Add x y) = eval x >>= \xres -> eval y >>= \yres -> return (xres + yres)
eval (Sub x y) = eval x >>= \xres -> eval y >>= \yres -> return (xres - yres)
eval (Mul x y) = eval x >>= \xres -> eval y >>= \yres -> return (xres * yres)
eval (Div x y) = eval x >>= \xres -> eval y >>= \yres -> safeDiv xres yres
eval (Pow x y) = eval x >>= \xres -> eval y >>= \yres -> return (xres ^ yres)

safeDiv :: Int -> Int -> Either ArithmeticError Int
safeDiv _ 0 = Left DivByZero
safeDiv x y = Right $ x `div` y
