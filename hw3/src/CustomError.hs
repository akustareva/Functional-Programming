module CustomError
       ( CustomError(..)
       ) where

data CustomError
    = DivByZero
    | IncorrectInputMap
    | VarAlreadyExists
    | VarNotDeclared
    | ParseError
    deriving(Eq)

instance Show CustomError where
  show DivByZero         = "Division by zero"
  show IncorrectInputMap = "Not all variables have assigned values"
  show VarAlreadyExists  = "Variable already exists"
  show VarNotDeclared    = "Variable is not declared"
