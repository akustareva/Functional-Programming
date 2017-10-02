module ADTWorld.NatNumbersSpec
       ( main
       , spec
       ) where

import           ADTWorld.NatNumbers
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    it "+" $ do
        let n1 = intToNat 34
        let n2 = intToNat 15
        n1 + n2 `shouldBe` 49

    it "*" $ do
        let n1 = intToNat 4
        let n2 = intToNat 15
        n1 * n2 `shouldBe` 60

    it "-" $ do
        let n1 = intToNat 34
        let n2 = intToNat 15
        n1 - n2 `shouldBe` 19

    it "natToInt" $ do
        natToInt Z `shouldBe` 0
        natToInt (S Z) `shouldBe` 1
        natToInt (S (S (S (S Z)))) `shouldBe` 4

    it "natToInt" $ do
        intToNat 0 `shouldBe` Z
        intToNat 1 `shouldBe` (S Z)
        intToNat 5 `shouldBe` (S (S (S (S (S Z)))))

    it "==" $ do
        let n1 = intToNat 34
        let n2 = intToNat 15
        n1 == n2 `shouldBe` False
        n2 == n1 `shouldBe` False
        n1 == n1 `shouldBe` True
        n2 == n2 `shouldBe` True

    it "<" $ do
        let n1 = intToNat 34
        let n2 = intToNat 15
        n1 < n2 `shouldBe` False
        n1 < n1 `shouldBe` False

    it ">=" $ do
        let n1 = intToNat 34
        let n2 = intToNat 15
        n1 >= n2 `shouldBe` True
        n1 >= n1 `shouldBe` True
        n2 >= n1 `shouldBe` False

    it "isNatEven" $ do
        let n1 = intToNat 34
        let n2 = intToNat 15
        isNatEven n1 `shouldBe` True
        isNatEven n2 `shouldBe` False

    it "natDiv" $ do
        let n1 = intToNat 34
        let n2 = intToNat 15
        natDiv n1 n2 `shouldBe` 2
        natDiv n2 n1 `shouldBe` 0

    it "natMod" $ do
        let n1 = intToNat 34
        let n2 = intToNat 15
        natMod n1 n2 `shouldBe` 4
        natMod n2 n1 `shouldBe` 15

    it "natGcd" $ do
        let n1 = intToNat 16
        let n2 = intToNat 28
        let n3 = intToNat 75
        let n4 = intToNat 6
        natGcd n1 n2 `shouldBe` 4
        natGcd n4 n3 `shouldBe` 3
