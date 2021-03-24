{-# LANGUAGE InstanceSigs #-}

module Block3.Task1
  ( -- * Types
    Parser(..)
    -- * Functions
  , first
  ) where

import Control.Applicative (Alternative (..))

-- | Parser-combinator that stores a function that takes a list of some type
-- and returns 'Maybe' of the parsing result and the remaining data stream.
data Parser s a = Parser
  { runParser :: [s] -> Maybe (a, [s])  -- ^ Inner parse function
  }

-- | Applies the supplied function to the first element of the pair.
-- This feature was suggested in an additional file from the homework.
first :: (a -> b) -> (a, c) -> (b, c)
first f (x, y) = (f x, y)

-- | Parser is instance of 'Functor'.
-- Allows to apply mapping to the parsing result.
instance Functor (Parser s) where
  fmap :: (a -> b) -> Parser s a -> Parser s b
  fmap f (Parser pf) = Parser (fmap (first f) . pf)

-- | Parser is instance of 'Applicative'.
-- Allows to combine parsers with static results with parsers containing
-- some functions. Useful for building complex combinatorial parsers.
instance Applicative (Parser s) where
  pure :: a -> Parser s a
  pure res = Parser (\s -> Just (res, s))

  (<*>) :: Parser s (a -> b) -> Parser s a -> Parser s b
  pf <*> pa = Parser $ \s -> do
    (f, remLeft)    <- runParser pf s
    (val, remRight) <- runParser pa remLeft
    return (f val, remRight)

-- | Parser is instance of 'Monad'.
-- Allows you to transform the stored types of the parsing result and
-- build a new parser based on the results of the previous ones.
instance Monad (Parser s) where
  return :: a -> Parser s a
  return = pure

  (>>=) :: Parser s a -> (a -> Parser s b) -> Parser s b
  Parser pf >>= f = Parser $ \s -> (pf s >>= \(res, rem) -> runParser (f res) rem)

-- | Parser is instance of 'Alternative'.
-- Allows you to choose from several possible scenarios to get a successful result.
instance Alternative (Parser s) where
  empty :: Parser s a
  empty = Parser (\res -> Nothing)

  (<|>) :: Parser s a -> Parser s a -> Parser s a
  Parser pf <|> Parser pa = Parser (\s -> pf s <|> pa s)
