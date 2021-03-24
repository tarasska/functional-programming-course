module Block3.Task1
  ( -- * Functions
    eitherConcat
  , maybeConcat
  ) where

import Data.Maybe (fromMaybe)

-- | Function 'maybeConcat' takes list of 'Maybe' containing list
-- and returns list - concatenation of inner 'Maybe' lists.
maybeConcat :: [Maybe [a]] -> [a]
maybeConcat elems = fromMaybe [] (mconcat elems)

-- | Function 'eitherConcat' takes list of 'Either' containing 'Monoid' values
-- and returns pair - concatenation of inner 'Either' values.
eitherConcat :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
eitherConcat elems = foldl mappend (mempty, mempty) (map mapper elems)
  where
    mapper :: (Monoid a, Monoid b) => Either a b -> (a, b)
    mapper (Left x)  = (x, mempty)
    mapper (Right y) = (mempty, y)
