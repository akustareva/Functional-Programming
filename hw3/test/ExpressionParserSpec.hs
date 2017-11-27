module ExpressionParserSpec
       ( main
       , spec
       ) where

import           ArithmeticExpression
import           ExpressionParser
import           Test.Hspec
import           Text.Megaparsec      (parseMaybe)

main :: IO ()
main = hspec spec

spec :: Spec
spec =
    it "Expression parser" $ do
        let str = "x + 3 * (let x = 2 in x)"
        parseMaybe exprParser str `shouldBe`
            Just (Add (Var "x") (Mul (Lit 3) (Let "x" (Lit 2) (Var "x"))))
        let str = "x + y * 3 / 2 - 1"
        parseMaybe exprParser str `shouldBe`
            Just (Sub (Add (Var "x") (Div (Mul (Var "y") (Lit 3)) (Lit 2))) (Lit 1))
        parseMaybe exprParser "(let x = 2 in x)" `shouldBe` Just (Let "x" (Lit 2) (Var "x"))
        parseMaybe exprParser "(3 + 4)" `shouldBe` Just (Add (Lit 3) (Lit 4)) 
