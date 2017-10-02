module FoldsSpec
       ( main
       , spec
       ) where

import           ADTWorld.Trees (Tree (..), fromList, toList)
import           Folds
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    it "sorted" $ do
        toList (fromList [1, 9, 5, 2, 10]) `shouldBe` [1, 2, 5, 9, 10]

    it "foldr" $ do
        let t = Node 3 (Node 1 Leaf Leaf) $ Node 123 (Node 4 Leaf Leaf) Leaf
        foldr (+) 0 t `shouldBe` 131

    it "splitOn" $ do
        splitOn '/' "path/to/file" `shouldBe` ["path", "to", "file"]
        splitOn '/' "/a//b/" `shouldBe` ["", "a", "", "b", ""]

    it "joinWith" $ do
        joinWith '/' ["path", "to", "file"] `shouldBe` "path/to/file"
        joinWith '/' ["check", "", "", ""] `shouldBe` "check///"

    it "id" $ do
        joinWith '/' (splitOn '/' "path/to/file") `shouldBe` "path/to/file"
