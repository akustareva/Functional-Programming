module ADTWorld.WeekDaysSpec
       ( main
       , spec
       ) where

import           ADTWorld.WeekDays
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    it "nextDay" $ do
        nextDay Sun `shouldBe` Mon
        nextDay Mon `shouldBe` Tue
        nextDay Tue `shouldBe` Wed
        nextDay Wed `shouldBe` Thu
        nextDay Thu `shouldBe` Fri
        nextDay Fri `shouldBe` Sat
        nextDay Sat `shouldBe` Sun
        nextDay Sun `shouldBe` Mon
    
    it "afterDays" $ do
        afterDays Mon 5 `shouldBe` Sat
        afterDays Fri 7 `shouldBe` Fri
        afterDays Tue 16 `shouldBe` Thu
        afterDays Wed 14 `shouldBe` Wed
    
    it "isWeekend" $ do
        isWeekend Mon `shouldBe` False
        isWeekend Fri `shouldBe` False
        isWeekend Tue `shouldBe` False
        isWeekend Sat `shouldBe` True
        isWeekend Sun `shouldBe` True
    
    it "daysToParty" $ do
        daysToParty Mon `shouldBe` 4
        daysToParty Tue `shouldBe` 3
        daysToParty Wed `shouldBe` 2
        daysToParty Thu `shouldBe` 1
        daysToParty Fri `shouldBe` 0
        daysToParty Sat `shouldBe` 6
        daysToParty Sun `shouldBe` 5