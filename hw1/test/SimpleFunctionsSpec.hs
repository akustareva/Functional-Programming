module SimpleFunctionsSpec
       ( main
       , spec
       ) where

import           SimpleFunctions
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    it "order3" $ do
        order3(10, 5, 1) `shouldBe` (1, 5, 10)
        order3(5, 2, 10) `shouldBe` (2, 5, 10)
        order3(13, -5, -1) `shouldBe` (-5, -1, 13)

    it "highestBitBase" $ do
        highestBitBase 15 `shouldBe` 8
        highestBitBase 16 `shouldBe` 16
        highestBitBase 17 `shouldBe` 16

    it "highestBit" $ do
        highestBit 15 `shouldBe` (8, 3)
        highestBit 16 `shouldBe` (16, 4)
        highestBit 17 `shouldBe` (16, 4)

    it "smartReplicate" $ do
        smartReplicate [1,2,3] `shouldBe` [1,2,2,3,3,3]
        smartReplicate [2,0,1] `shouldBe` [2,2,1]

    it "contains" $ do
        contains 3 [[1..5], [2,0], [3,4]] `shouldBe` [[1,2,3,4,5],[3,4]]