module PatternMatchingSpec
       ( main
       , spec
       ) where

import           PatternMatching
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    it "removeAtBase" $ do
        removeAtBase 1 [1,2,3] `shouldBe` [1,3]
        removeAtBase 10 [1,2,3] `shouldBe` [1,2,3]
        removeAtBase 3 [1..5] `shouldBe` [1,2,3,5]
        removeAtBase 2 "abc" `shouldBe` "ab"

    it "removeAt" $ do
        removeAt 1 [1,2,3] `shouldBe` (Just 2, [1,3])
        removeAt 10 [1,2,3] `shouldBe` (Nothing, [1,2,3])
        removeAt 2 "abc" `shouldBe` (Just 'c', "ab")

    it "collectEvery" $ do
        collectEvery 3 [1..8] `shouldBe` ([1,2,4,5,7,8], [3,6])
        collectEvery 10 [1,2,3] `shouldBe` ([1,2,3], [])
        collectEvery 4 "abcdabcd" `shouldBe` ("abcabc", "dd")

    it "stringSum" $ do
        stringSum "1 1" `shouldBe` 2
        stringSum "100\n\t-3" `shouldBe` 97
        stringSum "\t12345\t" `shouldBe` 12345
        stringSum "+1" `shouldBe` 1
        stringSum "1 +1" `shouldBe` 2
        stringSum "-1 +1" `shouldBe` 0
        stringSum "-1 \t\n100 -90 +21 \t+1" `shouldBe` 31

    it "mergeSort" $ do
        mergeSort [2, 1, 0, 3, 10, 5] `shouldBe` [0, 1, 2, 3, 5, 10]
        mergeSort [-5, 10, 1, -3, 8, 9, 10, 1, 0] `shouldBe` [-5, -3, 0, 1, 1, 8, 9, 10, 10]
        mergeSort [-8,1,-8,-5,-7] `shouldBe` [-8,-8,-7,-5,1]
