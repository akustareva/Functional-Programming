module UpdateVariablesSpec
       ( main
       , spec
       ) where

import           Control.Monad.State  (runStateT)
import           Data.Map             (empty, fromList)
import           Test.Hspec
import           UpdateVariables

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    it "Add var" $ do
        runStateT (addVar "x" 5) empty `shouldBe` Right ((), fromList [("x",5)])
        runStateT (addVar "x" 5) (fromList [("x",5)]) `shouldBe`
            Left VarAlreadyExists
        runStateT (addVar "x" 5) (fromList[("y", 3), ("z", 8)]) `shouldBe`
            Right ((), fromList [("x",5),("y",3),("z",8)])
        runStateT (addVar "x" 2) (fromList[("x", 1), ("z", 8)]) `shouldBe`
            Left VarAlreadyExists
    it "Update var" $ do
        runStateT (updateVar "x" 5) (fromList [("x",5)]) `shouldBe`
            Right ((), fromList [("x",5)])
        runStateT (updateVar "x" 5) (fromList[("x", 3), ("y", 8)]) `shouldBe`
            Right ((), fromList [("x",5),("y",8)])
        runStateT (updateVar "x" 5) (fromList[("z", 3), ("y", 8)]) `shouldBe`
            Left VarNotDeclared
