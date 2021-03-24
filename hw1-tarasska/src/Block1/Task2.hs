{-# LANGUAGE InstanceSigs #-}

module Block1.Task2
  ( -- * Type
    Nat(..)
    -- * Functions
  , isEven
  , isOdd
  ) where

-- | 'Nat' data type represents natural numbers in Peano arithmetic.
data Nat
  = Z      -- ^ Zero value 'Nat' constructor
  | S Nat  -- ^ Constructor for 'Nat' number following the given one
  deriving Show

-- | 'Nat' is instance of 'Eq'.
-- You may check values for equality
instance Eq Nat where
  (==) :: Nat -> Nat -> Bool
  (==) Z Z         = True
  (==) Z _         = False
  (==) _  Z        = False
  (==) (S x) (S y) = x == y

-- | 'Nat' is instance of 'Ord'.
-- You may compare values with each other.
instance Ord Nat where
  (<=) :: Nat -> Nat -> Bool
  (<=) Z _         = True
  (<=) _ Z         = False
  (<=) (S x) (S y) = x <= y

-- | 'Nat' is instance of 'Num'
-- You may use such arithmetic operations as addition, subtraction,
-- multiplication and also build 'Nat' from an integer.
-- Also you may use abs, signum and negate, but they may not work intuitively.
instance Num Nat where
  (+) :: Nat -> Nat -> Nat
  (+) x Z     = x
  (+) x (S y) = S (x + y)

  (-) :: Nat -> Nat -> Nat
  (-) Z _         = Z
  (-) x Z         = x
  (-) (S x) (S y) = x - y

  (*) :: Nat -> Nat -> Nat
  (*) _ Z     = Z
  (*) x (S y) = x * y + x

  abs :: Nat -> Nat
  abs x = x

  signum :: Nat -> Nat
  signum Z = Z
  signum _ = S Z

  fromInteger :: Integer -> Nat
  fromInteger 0 = Z
  fromInteger x
    | x > 0     = S $ fromInteger (x - 1)
    | otherwise = error "Negative argument is prohibited."

-- | 'Nat' is instance of 'Enum'.
-- You may use convert 'Num' to Int and vice versa.
-- Also you may find succ and pred of 'Nut' object, be careful Zero has no pred.
instance Enum Nat where
  pred :: Nat -> Nat
  pred Z     = error "Zero has no predecessor."
  pred (S x) = x

  fromEnum :: Nat -> Int
  fromEnum Z     = 0
  fromEnum (S x) = 1 + (fromEnum x)

  toEnum :: Int -> Nat
  toEnum 0 = Z
  toEnum x
    | x > 0     = S $ toEnum (x - 1)
    | otherwise = error "Negative argument is prohibited."

-- | 'Nat' is instance of 'Real'.
-- You may get Rational by 'Nat', it's necessary to define 'Integral' instance.
instance Real Nat where
  toRational :: Nat -> Rational
  toRational x = toRational $ fromEnum x

-- | 'Nat' is instance of 'Integral'.
-- You may calculate integer division and remainder.
-- Also you may convert 'Nat' to Integer value.
instance Integral Nat where
  toInteger :: Nat -> Integer
  toInteger Z     = 0
  toInteger (S x) = 1 + (toInteger x)

  quotRem :: Nat -> Nat -> (Nat, Nat)
  quotRem _ Z = error "Division by zero."
  quotRem x y
    | x < y     = (Z, x)
    | otherwise = (S quotVal, remVal) where (quotVal, remVal) = quotRem (x - y) y

  divMod = quotRem  -- define divMod because default isn't suitable

-- | Function 'isEven' takes 'Nat' and returns @True@ if value is even number
-- and @False@ otherwise
isEven :: Nat -> Bool
isEven x = (x `mod` 2) == Z

-- | Function 'isOdd' takes 'Nat' and returns @True@ if value is odd number
-- and @False@ otherwise
isOdd :: Nat -> Bool
isOdd x = (x `mod` 2) == (S Z)
