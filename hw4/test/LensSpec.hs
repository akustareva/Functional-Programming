module LensSpec
       ( main
       , spec
       ) where

import           Lens
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    it "set" $ do
        set _1 5 (2, 7) `shouldBe` (5, 7)
        set _1 199 (2, 7) `shouldBe` (199, 7)
        set _2 8 (2, 2) `shouldBe` (2, 8)
    it "view" $ do
        view _1 (2, 7) `shouldBe` 2
        view _2 (2, 7) `shouldBe` 7
    it "over" $ do
        over _1 (+1) (2, 7) `shouldBe` (3, 7)
        over _2 (*3) (2, 7) `shouldBe` (2, 21)
    it "(.~)" $ do
        (_1 .~ 4 $ (1, 2)) `shouldBe` (4, 2)
        (_2 .~ 4 $ (1, 2)) `shouldBe` (1, 4)
    it "(^.)" $ do
        (2, 7)^._1 `shouldBe` 2
        (2, 7)^._2 `shouldBe` 7
    it "(%~)" $ do
        (_1 %~ (+1) $ (2, 7)) `shouldBe` (3, 7)
        (_2 %~ (+1) $ (2, 7)) `shouldBe` (2, 8)
