module ArithmeticExpressionSpec
       ( main
       , spec
       ) where

import           CustomError
import           ArithmeticExpression
import           Control.Monad.Except (runExceptT)
import           Data.Map             (empty, fromList)
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
    it "Arithmetic expressions calculation" $ do
        let expr = Var "x" `Add` (Lit 3 `Mul` ("x" `Let` Lit 2 $ Var "x"))
        runExceptT (eval expr) (fromList [("x", Lit 1)]) `shouldBe` Right 7
        let expr = Var "x" `Add` (Lit 3 `Mul` ("x" `Let` (Lit 2 `Add` Var "y") $ (Var "x" `Sub` Lit 9)))
        runExceptT (eval expr) (fromList [("x", Lit 1), ("y", Lit 8)]) `shouldBe` Right 4
        let expr = Var "x" `Add` Var "y" `Add` Var "z"
        runExceptT (eval expr) (fromList [("x", Lit 1), ("y", Lit 8)]) `shouldBe` Left IncorrectInputMap
        let expr = "x" `Let` (Var "y" `Add` Var "z") $ (Var "x" `Div` Lit 4)
        runExceptT (eval expr) (fromList [("y", Lit 3), ("z", Lit 2 `Add` Lit 3)]) `shouldBe` Right 2
        let expr = Var "x" `Div` Var "y"
        runExceptT (eval expr) (fromList [("x", Lit 10), ("y", Lit 2 `Sub` Lit 2)]) `shouldBe` Left DivByZero
        let expr = Lit 5 `Div` Var "y"
        runExceptT (eval expr) empty `shouldBe` Left IncorrectInputMap
        
