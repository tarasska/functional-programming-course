{-# LANGUAGE InstanceSigs #-}

module Block1.Task3
  ( -- * Types
    Tree(..)
    -- * Functions
  , empty
  , find
  , fromList
  , insert
  , remove
  , size
  ) where

import Data.List.NonEmpty (NonEmpty (..), (<|))
import qualified Data.List.NonEmpty as NonEmpty

-- | 'Tree' data type represents primitive binary search tree.
data Tree a
  -- | Inner node constructor witch holds 'NonEmpty' list of equal values
  -- and two subtrees nodes.
  = Branch (NonEmpty a) (Tree a) (Tree a)
  -- | Terminal empty node constructor.
  | Leaf
  deriving Show

-- | 'Tree' is instance of 'Eq' if it's holds 'Eq' values.
-- You may check equality of two trees.
instance Eq a => Eq (Tree a) where
  (==) :: Tree a -> Tree a -> Bool
  (==) Leaf Leaf = True
  (==) Leaf _    = False
  (==) _ Leaf    = False
  (==) (Branch keys1 left1 right1) (Branch keys2 left2 right2) =
    if (keys1 == keys2)
    then (left1 == left2) && (right1 == right2)
    else False

-- | 'Tree' is instance of 'Foldable'.
-- You may use 'foldMap', 'foldr' and other folds.
instance Foldable Tree where
  foldMap :: Monoid m => (a -> m) -> Tree a -> m
  foldMap _ Leaf = mempty
  foldMap f (Branch keys left right) =
    (foldMap f left)
    <> (mconcat (NonEmpty.toList $ NonEmpty.map f keys))
    <> (foldMap f right)

  foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr _ startVal Leaf = startVal
  foldr f startVal (Branch keys left right) =
    let
      rightResult    = foldr f startVal right
      withNodeResult = NonEmpty.head $ NonEmpty.scanr f rightResult keys
    in
      foldr f withNodeResult left

-- | Function 'empty' takes 'Tree' and returns @True@ if tree has no values
-- and @False@ otherwise.
empty :: Tree a -> Bool
empty Leaf = True
empty _    = False

-- | Function 'size' takes 'Tree' of 'Ord' values and return number of values.
size :: Ord a => Tree a -> Int
size Leaf                     = 0
size (Branch keys left right) = (length keys) + (size left) + (size right)

-- | Function 'find' takes 'Ord' @value@ and 'Tree' of 'Ord' values and returns
-- @Just value@ if tree contains provided element and @Nothing@ otherwise.
find :: Ord a => a -> Tree a -> Maybe a
find _ Leaf = Nothing
find lookUpKey (Branch keys left right)
  | branchKey == lookUpKey = Just lookUpKey
  | branchKey > lookUpKey  = find lookUpKey left
  | otherwise              = find lookUpKey right
  where
    branchKey = NonEmpty.head keys

-- | Function 'insert' takes 'Ord' @value@ and 'Tree' of 'Ord' values and
-- returns a new tree containing the given element.
insert :: Ord a => a -> Tree a -> Tree a
insert newKey Leaf = Branch (NonEmpty.fromList [newKey]) Leaf Leaf
insert newKey (Branch keys left right)
  | branchKey == newKey = Branch (newKey <| keys) left right
  | branchKey > newKey  = Branch keys (insert newKey left) right
  | otherwise           = Branch keys left (insert newKey right)
  where
    branchKey = NonEmpty.head keys

-- | Function 'mergeBranch' takes two 'Tree' of 'Ord' values and
-- returns a new valid tree made of two given trees.
mergeBranch :: Ord a => Tree a -> Tree a -> Tree a
mergeBranch Leaf right = right
mergeBranch left Leaf  = left
mergeBranch (Branch keys lLeft lRight) rRight = Branch keys lLeft newRight
  where
    newRight = mergeBranch lRight rRight

-- | Function 'checkedRemove' takes 'Tree' of 'Ord' values and
-- returns a new tree with the value removed at the root.
checkedRemove :: Ord a => Tree a -> Tree a
checkedRemove Leaf = Leaf
checkedRemove (Branch (_ :| keys) left right)
  | length keys > 0 = Branch newKeys left right
  | otherwise       = mergeBranch left right
  where
    newKeys = NonEmpty.fromList keys

-- | Function 'remove' takes 'Ord' value and 'Tree' of 'Ord' values and
-- returns a new tree without provided value.
remove :: Ord a => a -> Tree a -> Tree a
remove _ Leaf = Leaf
remove rmKey node@(Branch keys left right)
  | branchKey == rmKey = checkedRemove node
  | branchKey > rmKey  = Branch keys (remove rmKey left) right
  | otherwise          = Branch keys left (remove rmKey right)
  where
    branchKey = NonEmpty.head keys

-- | Function 'fromList' takes list of 'Ord' values and
-- returns a new tree built on the given elements.
fromList :: Ord a => [a] -> Tree a
fromList []       = Leaf
fromList (x : xs) = insert x (fromList xs)
