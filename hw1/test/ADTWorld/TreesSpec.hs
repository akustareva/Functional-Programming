module ADTWorld.TreesSpec
       ( main
       , spec
       ) where

import           ADTWorld.Trees
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    it "isEmpty" $ do
        let t1 = Node 3 (Node 4 Leaf Leaf) $ Node 32 (Node 6 Leaf Leaf) Leaf
        let t2 = Leaf
        isEmpty t1 `shouldBe` False
        isEmpty t2 `shouldBe` True

    it "size" $ do
        let t1 = Node 3 (Node 4 Leaf Leaf) $ Node 32 (Node 6 Leaf Leaf) Leaf
        let t2 = Leaf
        size t1 `shouldBe` 4
        size t2 `shouldBe` 0

    it "findKey" $ do
        let t1 = Node 3 (Node 4 Leaf Leaf) $ Node 32 (Node 6 Leaf Leaf) Leaf
        let t2 = Leaf
        findKey t1 4 `shouldBe` True
        findKey t1 6 `shouldBe` True
        findKey t1 7 `shouldBe` False
        findKey t2 5 `shouldBe` False

    it "insertKey" $ do
        let t = Leaf
        insertKey t (4::Int) `shouldBe` Node 4 Leaf Leaf

    it "fromList" $ do
        fromList [(1::Int), (9::Int), (2::Int), (-5::Int), (0::Int)] `shouldBe`
            Node 1 (Node (-5) Leaf (Node 0 Leaf Leaf)) (Node 9 (Node 2 Leaf Leaf) Leaf)
