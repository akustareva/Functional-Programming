module UpdateVariablesSpec
       ( main
       , spec
       ) where

import           Control.Monad.State (runState)
import           Data.Map            (empty, fromList)
import           Test.Hspec
import           UpdateVariables

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    it "Add var" $ do
        runState (addVar "x" 5) empty `shouldBe` (NoError, fromList [("x",5)])
        runState (addVar "x" 5) (fromList[("y", 3), ("z", 8)]) `shouldBe`
            (NoError, fromList [("x",5),("y",3),("z",8)])
        runState (addVar "x" 2) (fromList[("x", 1), ("z", 8)]) `shouldBe`
            (VarAlreadyExists, fromList [("x",1),("z",8)])
    it "Update var" $ do
        runState (updateVar "x" 5) (fromList[("x", 3), ("y", 8)]) `shouldBe`
            (NoError, fromList [("x",5),("y",8)])
        runState (updateVar "x" 5) (fromList[("z", 3), ("y", 8)]) `shouldBe`
            (VarNotDeclared, fromList [("y",8),("z",3)])
