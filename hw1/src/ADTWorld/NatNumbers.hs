module ADTWorld.NatNumbers
       ( Nat(..)
       , natToInt
       , intToNat
       , natSum
       , natMult
       , natSub
       , natDivWithMod
       , natDiv
       , natMod
       , isNatEven
       , natGcd
       ) where

data Nat = Z | S Nat

instance Show Nat where
    show = show . natToInt

instance Eq Nat where
    Z == Z         = True
    (S x) == (S y) = x == y
    _ == _         = False

instance Ord Nat where
    Z <= _         = True
    _ <= Z         = False
    (S x) <= (S y) = x <= y

instance Num Nat where
    x + y = natSum x y
    x * y = natMult x y
    x - y = natSub x y
    fromInteger = intToNat
    abs n = n
    signum n
      | n == Z    = 0
      | otherwise = 1

natToInt :: Nat -> Integer
natToInt Z     = 0
natToInt (S x) = 1 + natToInt x

intToNat :: Integer -> Nat
intToNat x
  | x < 0     = error "Nat number cannot be < 0"
  | x == 0    = Z
  | otherwise = S (intToNat $ x - 1)

natSum :: Nat -> Nat -> Nat
natSum Z y     = y
natSum (S x) y = S $ natSum x y

natMult :: Nat -> Nat -> Nat
natMult Z _     = Z
natMult (S x) y = natSum y $ natMult x y

natSub :: Nat -> Nat -> Nat
natSub x Z         = x
natSub (S x) (S y) = natSub x y
natSub Z _         = error "Subtraction from Z in Nat numbers"

natDivWithMod :: Nat -> Nat -> (Nat, Nat)
natDivWithMod Z _ = (Z, Z)
natDivWithMod _ Z = error "Devision by zero"
natDivWithMod a b  = divImpl (S Z) a b
  where
    divImpl :: Nat -> Nat -> Nat -> (Nat, Nat)  -- x / y = z
    divImpl Z _ _           = error "Unexpected state"
    divImpl z@(S prevZ) x y
      | checkX > x  = (prevZ, x - y * prevZ)
      | checkX == x = (z, Z)
      | otherwise   = divImpl (S z) x y
      where
        checkX = z * y

natDiv :: Nat -> Nat -> Nat
natDiv x y = fst $ natDivWithMod x y

natMod :: Nat -> Nat -> Nat
natMod x y = snd $ natDivWithMod x y

isNatEven :: Nat -> Bool
isNatEven x
  | natMod x 2 == 0 = True
  | otherwise       = False

natGcd :: Nat -> Nat -> Nat
natGcd a b
  | b == 0    = a
  | otherwise = natGcd b $ natMod a b
