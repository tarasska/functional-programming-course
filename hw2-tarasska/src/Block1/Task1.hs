module Block1.Task1
  ( -- * Functions
    stringSum
  ) where

import Text.Read (readMaybe)

-- | Parses the string as a sequence of numbers, summing it if successful.
stringSum :: String -> Maybe Int
stringSum s = sum <$> traverse readMaybe (words s)
