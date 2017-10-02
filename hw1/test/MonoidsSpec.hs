module MonoidsSpec
       ( main
       , spec
       ) where

import           Monoids
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    it "maybeConcat" $ do
        maybeConcat [Just [1,2,3], Nothing, Just [4,5]] `shouldBe` [1,2,3,4,5]
