module OptionalMonad.ArithmeticExpressionSpec
       ( main
       , spec
       ) where

import           OptionalMonad.ArithmeticExpression
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
    it "eval" $ do
        eval (Const 8)                                   `shouldBe` Right 8
        eval (Add (Const 11) (Const 13))                 `shouldBe` Right 24
        eval (Add (Add (Const 11) (Const 13)) (Const 5)) `shouldBe` Right 29
        eval (Add (Const 11) (Const (-13)))              `shouldBe` Right (-2)
        eval (Sub (Const 19) (Const 13))                 `shouldBe` Right 6
        eval (Sub (Add (Const 14) (Const 3)) (Const 8))  `shouldBe` Right 9
        eval (Sub (Const 19) (Const (-6)))               `shouldBe` Right 25
        eval (Sub (Const 19) (Const 37))                 `shouldBe` Right (-18)
        eval (Mul (Const 4) (Const 5))                   `shouldBe` Right 20
        eval (Mul (Const 4) (Mul (Const 3) (Const 6)))   `shouldBe` Right 72
        eval (Div (Const 5) (Const 3))                   `shouldBe` Right 1
        eval (Div (Const 100) (Const 10))                `shouldBe` Right 10
        eval (Div (Const 100) (Const 0))                 `shouldBe` Left DivByZero
        eval (Pow (Const 3) (Pow (Const 3) (Const 3)))   `shouldBe` Right 7625597484987