{-# LANGUAGE LambdaCase #-}

module Block3.Task2
  ( -- * Functions
    element
  , eof
  , ok
  , satisfy
  , stream
  ) where

import Block3.Task1 (Parser (..))

-- | Completes successfully on any data stream without modifying it
-- or returning anything.
ok :: Parser s ()
ok = Parser $ \s -> Just ((), s)

-- | It succeeds if the data stream ends, otherwise failed.
eof :: Parser s ()
eof = Parser $ \case
  [] -> Just ((), [])
  _  -> Nothing

-- | Checks if the head of a stream matches a predicate.
-- On success returns the head as result and the remaining stream.
satisfy :: (s -> Bool) -> Parser s s
satisfy cond = Parser $ \case
  (x : xs) -> if cond x then Just (x, xs) else Nothing
  _        -> Nothing

-- | Parses the head of the stream, if it is equal to the given element,
-- fail otherwise.
element :: Eq s => s -> Parser s s
element e = satisfy (== e)

-- | Parses the stream prefix, if it is equal to the given list, fail otherwise.
stream :: Eq s => [s] -> Parser s [s]
stream expectedElems = Parser $ \s ->
  (dropIfEq expectedElems s) >>= \rem -> return (expectedElems, rem)
  where
    dropIfEq :: Eq s => [s] -> [s] -> Maybe [s]
    dropIfEq [] actual         = Just actual
    dropIfEq expected []       = Nothing
    dropIfEq (e : es) (c : cs) = if e == c then dropIfEq es cs else Nothing
