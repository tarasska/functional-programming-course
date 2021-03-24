{-# LANGUAGE InstanceSigs #-}

{-|
Module      : Task8.ComonadContainer

This module focuses on the data types used to simulate the spread
of a virus using comonads. All code is taken from the presentation
of the course, so I consider comments unnecessary.
-}
module Task8.ComonadContainer
  ( -- * Types
    ListZipper(..)
  , Grid(..)
    -- * Functions
  , down
  , gridRead
  , gridWrite
  , horizontal
  , iterateTail
  , left
  , listLeft
  , listRight
  , listWrite
  , mkZipper
  , neighbours
  , right
  , toList
  , up
  , vertical
  ) where

import Control.Comonad (Comonad (..))
import Control.Monad (liftM2)

data ListZipper a = LZ [a] a [a]

instance Functor ListZipper where
  fmap f (LZ ls x rs) = LZ (map f ls) (f x) (map f rs)

instance Comonad ListZipper where
  extract :: ListZipper a -> a
  extract (LZ _ x _) = x

  duplicate :: ListZipper a -> ListZipper (ListZipper a)
  duplicate = mkZipper listLeft listRight

listLeft :: ListZipper a -> ListZipper a
listLeft (LZ (l : ls) x rs) = LZ ls l (x : rs)
listLeft _                  = error "Unable to shift left"

listRight :: ListZipper a -> ListZipper a
listRight (LZ ls x (r : rs)) = LZ (x : ls) r rs
listRight _                  = error "Unable to shift right"

listWrite :: a -> ListZipper a -> ListZipper a
listWrite x (LZ ls _ rs) = LZ ls x rs

iterateTail :: (a -> a) -> a -> [a]
iterateTail f = tail . iterate f

mkZipper :: (v -> v) -> (v -> v) -> v -> ListZipper v
mkZipper genLeft genRight e = LZ (iterateTail genLeft e) e (iterateTail genRight e)

toList :: Int -> ListZipper a -> [a]
toList n (LZ ls x rs) = reverse (take n ls) ++ x : take n rs



newtype Grid a = Grid { unGrid :: ListZipper (ListZipper a) }

instance Functor Grid where
  fmap f (Grid g) = Grid (fmap (fmap f) g)

instance Comonad Grid where
  extract :: Grid a -> a
  extract = gridRead

  duplicate :: Grid a -> Grid (Grid a)
  duplicate = Grid . fmap horizontal . vertical

up :: Grid a -> Grid a
up (Grid g) = Grid (listLeft g)

down :: Grid a -> Grid a
down (Grid g) = Grid (listRight g)

left :: Grid a -> Grid a
left (Grid g) = Grid (fmap listLeft g)

right :: Grid a -> Grid a
right (Grid g) = Grid (fmap listRight g)

gridRead :: Grid a -> a
gridRead (Grid g) = extract $ extract g

gridWrite :: a -> Grid a -> Grid a
gridWrite x (Grid g) = Grid $ listWrite newLine g
  where
    oldLine = extract g
    newLine = listWrite x oldLine

horizontal :: Grid a -> ListZipper (Grid a)
horizontal = mkZipper left right

vertical :: Grid a -> ListZipper (Grid a)
vertical = mkZipper up down

neighbours :: [Grid a -> Grid a]
neighbours = horizontals ++ verticals ++ liftM2 (.) horizontals verticals
  where
    horizontals = [left, right]
    verticals   = [up, down]
