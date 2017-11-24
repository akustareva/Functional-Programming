module ArithmeticExpressionSpec
       ( main
       , spec
       ) where

import           ArithmeticExpression
import           Control.Monad.Reader (runReader)
import           Data.Map             (fromList)
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
    it "ArithmeticExpressionCalc" $ do
        let expr = Var "x" `Add` (Lit 3 `Mul` ("x" `Let` Lit 2 $ Var "x"))
        runReader (eval expr) (fromList [("x", Lit 1)]) `shouldBe` Right 7
        let expr = Var "x" `Add` (Lit 3 `Mul` ("x" `Let` (Lit 2 `Add` Var "y") $ (Var "x" `Sub` Lit 9)))
        runReader (eval expr) (fromList [("x", Lit 1), ("y", Lit 8)]) `shouldBe` Right 4
        let expr = Var "x" `Add` Var "y" `Add` Var "z"
        runReader (eval expr) (fromList [("x", Lit 1), ("y", Lit 8)]) `shouldBe` Left IncorrectInputMap
        let expr = "x" `Let` (Var "y" `Add` Var "z") $ (Var "x" `Div` Lit 4)
        runReader (eval expr) (fromList [("y", Lit 3), ("z", Lit 2 `Add` Lit 3)]) `shouldBe` Right 2
        let expr = Var "x" `Div` Var "y"
        runReader (eval expr) (fromList [("x", Lit 10), ("y", Lit 2 `Sub` Lit 2)]) `shouldBe` Left DivByZero
