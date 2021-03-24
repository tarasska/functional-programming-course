module Block2.Task2
  ( -- * Functions
    joinWith
  , splitOn
  ) where

import Data.List.NonEmpty (NonEmpty (..), fromList, (<|))

-- | Function 'splitOn' takes 'Eq' value as separator and list of 'Eq' values
-- to be split. Returns 'NonEmpty' of lists - container of separate parts.
splitOn :: Eq a => a -> [a] -> NonEmpty [a]
splitOn separator elems = foldr (checker separator) (fromList [[]]) elems
  where
    checker :: Eq a => a -> a -> NonEmpty [a] -> NonEmpty [a]
    checker sep a parts@(p :| ps)
      | a == sep  = [] <| parts
      | otherwise = (a : p) :| ps

-- | Function 'joinWith' takes separating element and list of values to be join.
-- Returns 'NonEmpty' of lists - container of separate parts.
joinWith :: a -> NonEmpty [a] -> [a]
joinWith separator xs = foldr1 (\x acc -> x ++ [separator] ++ acc) xs
