module VariableCreationParserSpec
       ( main
       , spec
       ) where

import           ArithmeticExpression
import           Test.Hspec
import           Text.Megaparsec        (parseMaybe)
import           VariableCreationParser

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    it "Statement parser" $ do
         parseMaybe stmtParser "x = 3 + 4" `shouldBe` Just Statement {name = "x", value = Add (Lit 3) (Lit 4)}
         parseMaybe stmtParser "x = 3 + (let y = 3 in y + 1)" `shouldBe`
            Just Statement {name = "x", value = Add (Lit 3) (Let "y" (Lit 3) (Add (Var "y") (Lit 1)))}
    it "Action parser" $ do
        parseMaybe actionParser "mut x = 3 + 4"  `shouldBe`
            Just (Create Statement {name = "x", value = Add (Lit 3) (Lit 4)})
        parseMaybe actionParser "x = 3 * 5 + 4 / 2" `shouldBe`
            Just (Update Statement {name = "x", value = Add (Mul (Lit 3) (Lit 5)) (Div (Lit 4) (Lit 2))})
