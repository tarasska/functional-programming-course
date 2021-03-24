{-# LANGUAGE InstanceSigs #-}

module Block1.Task2
  ( -- * Type
    Tree(..)
    -- * Functions
  , toList
  ) where

-- | 'Tree' data type represents primitive binary full tree, where values
-- stores only in leafs.
data Tree a
  = Branch (Tree a) (Tree a)  -- ^ Inner constructor holds 2 branches, no values
  | Leaf a                    -- ^ Terminal constructor for storing values
  deriving Show

-- | 'Tree' is instance of 'Eq' if it's holds 'Eq' values.
-- You may check equality of two trees.
instance Eq a => Eq (Tree a) where
  (==) :: Tree a -> Tree a -> Bool
  (Leaf val1)    == (Leaf val2)    = val1 == val2
  (Leaf _)       == _              = False
  _              == (Leaf _)       = False
  (Branch l1 r1) == (Branch l2 r2) = (l1 == l2) && (r1 == r2)

-- | 'Tree' is instance of 'Functor'.
-- You may map stored values with provided function.
instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap f (Leaf val)   = Leaf (f val)
  fmap f (Branch l r) = Branch (fmap f l) (fmap f r)

-- | 'Tree' is instance of 'Applicative'.
-- You can wrap a value and map a value tree using a function tree.
instance Applicative Tree where
  pure :: a -> Tree a
  pure val = Leaf val

  (<*>) :: Tree (a -> b) -> Tree a -> Tree b
  (<*>) (Leaf f) tree     = fmap f tree
  (<*>) (Branch l r) tree = Branch (l <*> tree) (r <*> tree)

-- | 'Tree' is instance of 'Foldable'.
-- You can fold values in a tree.
instance Foldable Tree where
  foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr f accum (Leaf val)   = f val accum
  foldr f accum (Branch l r) = foldr f (foldr f accum r) l

-- | 'Tree' is instance of 'Traversable'.
-- Tree can be traversed from left to right,
-- performing an action on each element.
instance Traversable Tree where
  traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
  traverse func (Leaf val)   = Leaf <$> func val
  traverse func (Branch l r) = Branch <$> traverse func l <*> traverse func r

-- | Make list of elements of a tree, from left to right.
toList :: Tree a -> [a]
toList (Leaf val)   = [val]
toList (Branch l r) = (toList l) ++ (toList r)
