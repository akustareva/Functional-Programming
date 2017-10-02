module ADTWorld.VectorsSpec
       ( main
       , spec
       ) where

import           ADTWorld.Vectors
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    it "vectorLen" $ do
        let v1 = Vector2D (3::Double) (4::Double)
        let v2 = Vector3D (2::Double) (4::Double) (4::Double)
        vectorLen v1 `shouldBe` 5.0
        vectorLen v2 `shouldBe` 6.0

    it "vectorsSum" $ do
        let v1 = Vector2D (8::Int) (3::Int)
        let v2 = Vector3D (5::Int) (3::Int) (7::Int)
        vectorsSum v1 v2 `shouldBe` Vector3D 13 6 7
        vectorsSum v1 v1 `shouldBe` Vector2D 16 6
        vectorsSum v2 v2 `shouldBe` Vector3D 10 6 14

    it "scalarProduct" $ do
        let v1 = Vector2D (8::Int) (3::Int)
        let v2 = Vector3D (5::Int) (3::Int) (7::Int)
        scalarProduct v1 v2 `shouldBe` 49
        scalarProduct v1 v1 `shouldBe` 73
        scalarProduct v2 v2 `shouldBe` 83

    it "distance" $ do
        let v = Vector2D (8::Double) (3::Double)
        distance v v `shouldBe` 0.0

    it "vectorProduct" $ do
        let v1 = Vector2D (8::Int) (3::Int)
        let v2 = Vector3D (5::Int) (3::Int) (7::Int)
        vectorProduct v1 v1 `shouldBe` Vector3D 0 0 0
        vectorProduct v1 v2 `shouldBe` Vector3D 21 (-56) 9
