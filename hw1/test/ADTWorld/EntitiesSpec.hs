module ADTWorld.EntitiesSpec
       ( main
       , spec
       ) where

import           ADTWorld.Entities
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    it "fight" $ do
        let k1 = Knight "K1" 5 33
        let k2 = Knight "K2" 8 21
        let m1 = Monster "M1" 6 29
        let m2 = Monster "M2" 5 32
        fight k1 m1 `shouldBe` (Knight {name = "K1", attack = 5, health = 3}, Monster {name = "M1", attack = 6, health = -1}, 11)
        fight k2 m2 `shouldBe` (Knight {name = "K2", attack = 8, health = 6}, Monster {name = "M2", attack = 5, health = 0}, 7)
        fight k1 k2 `shouldBe` (Knight {name = "K1", attack = 5, health = 1}, Knight {name = "K2", attack = 8, health = -4}, 9)
        fight m1 m2 `shouldBe` (Monster {name = "M2", attack = 5, health = 2}, Monster {name = "M1", attack = 6, health = -1}, 11)