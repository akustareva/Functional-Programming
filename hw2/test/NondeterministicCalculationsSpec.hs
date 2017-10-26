module NondeterministicCalculationsSpec
       ( main
       , spec
       ) where

import           NondeterministicCalculations
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    it "bin" $ do
        bin 1 `shouldBe` [[0], [1]]
        bin 2 `shouldBe` [[0, 0], [1, 0], [0, 1], [1, 1]]
        bin 3 `shouldBe` [[0, 0, 0], 
                          [1, 0, 0], 
                          [0, 1, 0], 
                          [1, 1, 0], 
                          [0, 0, 1], 
                          [1, 0, 1], 
                          [0, 1, 1], 
                          [1, 1, 1]]
    it "combinations" $ do
        combinations 4 3 `shouldBe` [[1,2,3],[1,2,4],[1,3,4],[2,3,4]]
        combinations 4 2 `shouldBe` [[1,2],[1,3],[2,3],[1,4],[2,4],[3,4]] 
        combinations 5 3 `shouldBe` [[1,2,3],[1,2,4],[1,3,4],[2,3,4],[1,2,5],[1,3,5],[2,3,5],[1,4,5],[2,4,5],[3,4,5]]
        combinations 5 2 `shouldBe` [[1,2],[1,3],[2,3],[1,4],[2,4],[3,4],[1,5],[2,5],[3,5],[4,5]]
        combinations 6 4 `shouldBe` [[1,2,3,4],[1,2,3,5],[1,2,4,5],[1,3,4,5],[2,3,4,5],
                                     [1,2,3,6],[1,2,4,6],[1,3,4,6],[2,3,4,6],[1,2,5,6],
                                     [1,3,5,6],[2,3,5,6],[1,4,5,6],[2,4,5,6],[3,4,5,6]]
    it "permutations" $ do
        permutations [22, 10, 5] `shouldBe` [[22,10,5],[10,22,5],[10,5,22],[22,5,10],[5,22,10],[5,10,22]]
        permutations "abcd"      `shouldBe` ["abcd","bacd","bcad","bcda","acbd","cabd","cbad","cbda",
                                             "acdb","cadb","cdab","cdba","abdc","badc","bdac","bdca",
                                             "adbc","dabc","dbac","dbca","adcb","dacb","dcab","dcba"]