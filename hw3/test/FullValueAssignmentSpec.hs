module FullValueAssignmentSpec
       ( main
       , spec
       ) where

import           CustomError
import           ArithmeticExpression
import           Control.Monad.State    (runStateT)
import           Data.Map               (empty, fromList)
import           FullValueAssignment
import           Test.Hspec
import           VariableCreationParser

main :: IO ()
main = hspec spec

spec :: Spec
spec =
    it "Update vars set" $ do
        let s1 = Statement "x" (Lit 5)
        let a1 = Create s1
        let s2 = Statement "y" (Lit 8)
        let a2 = Create s2
        let s3 = Statement "x" (Lit 18 `Add` Lit 15)
        let a3 = Update s3
        let s4 = Statement "z" (Var "x")
        let a4 = Create s4
        let s5 = Statement "x" (Lit 18 `Sub` Lit 18)
        let a5 = Update s5
        let s6 = Statement "x1" (Var "x" `Div` Var "x")
        let a6 = Create s6
        let s7 = Statement "z" (Var "x")
        let a7 = Update s7
        runStateT (updateVarsSet [a1, a2]) empty `shouldBe` Right ((), fromList [("x",5), ("y", 8)])
        runStateT (updateVarsSet [a1, a1]) empty `shouldBe` Left VarAlreadyExists
        runStateT (updateVarsSet [a1, a2, a3]) empty `shouldBe`
            Right ((), fromList [("x",33), ("y", 8)])
        runStateT (updateVarsSet [a1, a2, a4, a3, a7]) empty `shouldBe`
            Right ((), fromList [("z",33), ("x",33), ("y",8)])
        runStateT (updateVarsSet [a4]) empty `shouldBe` Left IncorrectInputMap
        runStateT (updateVarsSet [a3]) (fromList [("y", 8)]) `shouldBe` Left VarNotDeclared
        runStateT (updateVarsSet [a1, a5, a6]) empty `shouldBe` Left DivByZero

