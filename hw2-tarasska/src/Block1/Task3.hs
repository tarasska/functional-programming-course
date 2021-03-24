{-# LANGUAGE InstanceSigs #-}

module Block1.Task3
  ( -- * Type
    NonEmpty(..)
    -- * Functions
  , fromList
  , toList
  ) where

-- | 'NonEmpty' data type represent list which must contain at least one element.
data NonEmpty a = a :| [a] deriving (Show, Eq)

-- | Make list of elements of a 'NonEmpty', from left to right.
toList :: NonEmpty a -> [a]
toList (x :| xs) = x : xs

-- | Make 'NonEmpty' from regular list. Failed with error, if list is empty.
fromList :: [a] -> NonEmpty a
fromList []       = error "Unable to build NonEmpty from empty list"
fromList (x : xs) = x :| xs

-- | 'NonEmpty' is instance of 'Functor'.
-- You may map stored values with provided function.
instance Functor NonEmpty where
  fmap :: (a -> b) -> NonEmpty a -> NonEmpty b
  fmap f (x :| xs) = (f x) :| fmap f xs

-- | 'NonEmpty' is instance of 'Applicative'.
-- You can wrap a value and map a value 'NonEmpty' using a 'NonEmpty'
-- that the function stores.
instance Applicative NonEmpty where
  pure :: a -> NonEmpty a
  pure x = x :| []

  (<*>) :: NonEmpty (a -> b) -> NonEmpty a -> NonEmpty b
  (<*>) (f :| fs) neList@(x :| xs) = y :| ys
    where
      (y : ys) = (f : fs) <*> (x : xs)

-- | 'NonEmpty' is instance of 'Foldable'.
-- You can fold values in a NonEmpty.
instance Foldable NonEmpty where
  foldr :: (a -> b -> b) -> b -> NonEmpty a -> b
  foldr f accum (x :| xs) = f x (foldr f accum xs)

-- | 'NonEmpty' is instance of 'Traversable'.
-- NonEmpty can be traversed from left to right,
-- performing an action on each element.
instance Traversable NonEmpty where
  traverse :: Applicative f => (a -> f b) -> NonEmpty a -> f (NonEmpty b)
  traverse func (x :| xs) = (:|) <$> (func x) <*> (traverse func xs)

-- | 'NonEmpty' is instance of 'Monad'.
-- You may wrap you value in 'NonEmpty' and use @>>=@ for 'NonEmpty'.
instance Monad NonEmpty where
  return :: a -> NonEmpty a
  return = pure

  (>>=) :: NonEmpty a -> (a -> NonEmpty b) -> NonEmpty b
  (>>=) (x :| xs) f = y :| (ys ++ (xs >>= \elem -> toList $ f elem))
    where
      y :| ys = f x
