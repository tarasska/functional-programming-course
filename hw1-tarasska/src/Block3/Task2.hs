{-# LANGUAGE InstanceSigs #-}

module Block3.Task2
  ( -- * Types
    Endo(..)
  , Name(..)
  , NonEmpty(..)
  , ThisOrThat(..)
  ) where

-- | 'NonEmpty' data type represent list which must contain at least one element.
data NonEmpty a = a :| [a]
  deriving (Eq, Show)

-- | 'ThisOrThat' data type represent container with one or two alternatives.
data ThisOrThat a b
  = This a    -- ^ Constructor of first alternative
  | That b    -- ^ Constructor of second alternative
  | Both a b  -- ^ Constructor of both alternatives
  deriving (Eq, Show)

-- | 'Name' data type is alias for the 'String' type.
data Name
  = Name String  -- ^ Constructor which wraps 'String' value
  | Nempty       -- ^ Constructor for empty 'String' which used as 'mempty'
  deriving (Eq, Show)

-- | 'Endo' data type is alias for functions that preserve the argument type.
newtype Endo a = Endo { getEndo :: a -> a }

-- | 'NonEmpty' is instance of 'Semigroup'.
-- Values may be concatenated with @(<>)@.
instance Semigroup (NonEmpty a) where
  (<>) :: NonEmpty a -> NonEmpty a -> NonEmpty a
  (<>) (x :| xs) (y :| ys) = x :| (xs ++ (y : ys))

-- | 'ThisOrThat' is instance of 'Semigroup'.
-- Values may be concatenated with @(<>)@.
-- The left value always replaces the right one.
instance Semigroup (ThisOrThat a b) where
  (<>) :: ThisOrThat a b -> ThisOrThat a b -> ThisOrThat a b
  (<>) (This x) (This _)   = This x
  (<>) (This x) (That y)   = Both x y
  (<>) (This x) (Both _ z) = Both x z
  (<>) (That x) (This y)   = Both y x
  (<>) (That x) (That _)   = That x
  (<>) (That x) (Both y _) = Both y x
  (<>) (Both x y) _        = Both x y

-- | 'Name' is instance of 'Semigroup'.
-- Values may be concatenated with @(<>)@.
-- The values merged through a dot.
instance Semigroup Name where
  (<>) :: Name -> Name -> Name
  (<>) Nempty name       = name
  (<>) name Nempty       = name
  (<>) (Name x) (Name y) = Name (x ++ "." ++ y)

-- | 'Endo' is instance of 'Semigroup'.
-- Values may be concatenated with @(<>)@.
-- The functions concatenated as composition.
instance Semigroup (Endo a) where
  (<>) :: Endo a -> Endo a -> Endo a
  (<>) leftEndo rightEndo = Endo (getEndo leftEndo . getEndo rightEndo)

-- | 'Name' is instance of 'Monoid'.
-- Empty String representation used as 'mempty'.
instance Monoid Name where
  mempty :: Name
  mempty = Nempty

-- | 'Name' is instance of 'Monoid'.
-- Standard function 'id' used as 'mempty'.
instance Monoid (Endo a) where
  mempty :: Endo a
  mempty = Endo id
