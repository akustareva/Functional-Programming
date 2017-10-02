module ADTWorld.Vectors
       ( Vector (..)
       , packVector
       , unpackVector
       , vectorLen
       , vectorsSum
       , scalarProduct
       , distance
       , vectorProduct
       ) where

data Vector a = Vector2D a a | Vector3D a a a
    deriving (Eq, Show)

packVector :: Vector a -> [a]
packVector (Vector2D x y)   = [x, y]
packVector (Vector3D x y z) = [x, y, z]

unpackVector :: [a] -> Vector a
unpackVector [x, y]    = Vector2D x y
unpackVector [x, y, z] = Vector3D x y z
unpackVector _         = error "Only 2D and 3D vectors are supported"

vectorLen :: Floating a => Vector a -> a
vectorLen = sqrt . sum . map (^2) . packVector

vectorsSum :: Num a => Vector a -> Vector a ->  Vector a
vectorsSum v1 v2 = unpackVector $ listSum (packVector v1) (packVector v2)
  where
    listSum :: Num a => [a] -> [a] -> [a]
    listSum [] []         = []
    listSum x []          = x
    listSum [] y          = y
    listSum (x:xs) (y:ys) = x + y : listSum xs ys

scalarProduct :: Num a => Vector a -> Vector a ->  a
scalarProduct v1 v2 = listProd (packVector v1) (packVector v2)
  where
    listProd :: Num a => [a] -> [a] -> a
    listProd [] []         = 0
    listProd [_] []        = 0
    listProd [] [_]        = 0
    listProd (x:xs) (y:ys) = x * y + listProd xs ys
    listProd _ _           = error "Unexpected state"

distance :: Floating a => Vector a -> Vector a ->  a
distance v1 v2 = sqrt . sum . map (^2) $ listOfDiffs (packVector v1) (packVector v2)
  where
    listOfDiffs :: Num a => [a] -> [a] -> [a]
    listOfDiffs [] []         = []
    listOfDiffs [x] []        = [-x]
    listOfDiffs [] [y]        = [y]
    listOfDiffs (x:xs) (y:ys) = y - x : listOfDiffs xs ys
    listOfDiffs _ _           = error "Unexpected state"

vectorProduct :: Num a => Vector a -> Vector a ->  Vector a
vectorProduct v1 v2 = unpackVector $ vecProductImpl (to3D $ packVector v1) (to3D $ packVector v2)
  where
    to3D :: Num a => [a] -> [a]
    to3D [x, y]    = [x, y, 0]
    to3D [x, y, z] = [x, y, z]
    to3D _         = error "Only 2D and 3D vectors are supported"
    vecProductImpl :: Num a => [a] -> [a] -> [a]
    vecProductImpl l1 l2 = [fstV, sndV, trdV]
      where
        fstV = l1!!1 * l2!!2 - l1!!2 * l2!!1
        sndV = l1!!2 * head l2 - head l1 * l2!!2
        trdV = head l1 * l2!!1 - l1!!1 * head l2
