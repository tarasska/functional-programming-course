{-# LANGUAGE InstanceSigs #-}

module Block3.Task3
  ( -- * Type
    CorrectBracketSeq(..)
    -- * Functions
  , correctBracketSeqParser
  , intParser
  , intTokenParser
  , nonNegativeIntParser
  , nonNegativeIntTokenParser
  ) where

import Control.Applicative (Alternative (..), liftA2)
import Data.Char (isDigit)

import Block3.Task1 (Parser (..))
import Block3.Task2 (element, eof, ok, satisfy, stream)

-- | 'CorrectBracketSeq' is a sequence of constructors describing
-- a structure equivalent to a string representation.
data CorrectBracketSeq
  = Empty                                    -- ^ Terminal for empty sequence
  | Block CorrectBracketSeq                  -- ^ CBS wrapped with brackets
  | CorrectBracketSeq :|: CorrectBracketSeq  -- ^ Concatenation of two CBS

-- | 'CorrectBracketSeq' is instance of 'Eq'.
-- Compares sequences, ignoring empty terminal states,
-- so that the result can be written shorter.
instance Eq CorrectBracketSeq where
  (==) :: CorrectBracketSeq -> CorrectBracketSeq -> Bool
  (Empty)       == (Empty)       = True
  (Block a)     == (Block b)     = (a == b)
  (a :|: b)     == (c :|: d)     = (a == c) && (b == d)
  a             == (b :|: Empty) = (a == b)
  a             == (Empty :|: b) = (a == b)
  (a :|: Empty) == b             = (a == b)
  (Empty :|: a) == b             = (a == b)
  _             == _             = False

-- | 'CorrectBracketSeq' is instance of 'Show'.
-- You may get correct string representation using 'show'.
instance Show CorrectBracketSeq where
  show :: CorrectBracketSeq -> String
  show Empty     = ""
  show (Block b) = "(" ++ (show b) ++ ")"
  show (l :|: r) = (show l) ++ (show r)

-- | Parses a sequence of characters in 'CorrectBracketSeq' if it satisfies
-- the properties of a correct bracket sequence and does not contain any space
-- or other characters in the stream.
correctBracketSeqParser :: Parser Char CorrectBracketSeq
correctBracketSeqParser = parse <* eof
  where
    parse :: Parser Char CorrectBracketSeq
    parse = (fmap (:|:) parseBlock <*> parse) <|> (Empty <$ ok)

    parseBlock :: Parser Char CorrectBracketSeq
    parseBlock = fmap Block (element '(' *> parse <* element ')')

-- | Parses a sequence of characters into a list of characters
-- if they match the string representation of a signed integer.
-- This function can be reused to create parsing into any convenient integer type.
parseIntChars :: Parser Char [Char]
parseIntChars = parseSign <*> parseDigits
  where
    parseSign :: Parser Char ([Char] -> [Char])
    parseSign
      =   (fmap (:) (element '-'))
      <|> (fmap (:) (element '+'))
      <|> (('+' :) <$ ok)

    parseDigits :: Parser Char [Char]
    parseDigits = anyDigit (satisfy isDigit)
      where
        anyDigit :: Parser Char Char -> Parser Char [Char]
        anyDigit parser =
          let recursiveHelper = liftA2 (:) parser (recursiveHelper <|> pure [])
           in recursiveHelper

-- | Converts 'Integer' to 'Int' without overflow.
checkedIntegerToInt :: Integer -> Maybe Int
checkedIntegerToInt x = do
  if (toInteger (minBound :: Int)) <= x && x <= (toInteger (maxBound :: Int))
  then Just $ fromInteger x
  else Nothing

-- | Allow leading plus on the number, it also checks 'Int' overflow.
-- Parse 'String' to 'Int' if possible, fails otherwise.
maybeInt :: String -> Maybe Int
maybeInt s@(c : cs) | c == '+' = checkedIntegerToInt (read cs :: Integer)
maybeInt s                     = checkedIntegerToInt (read s  :: Integer)

-- | Map int candidate from 'String' view to 'Int'
mapParserResToSignedInt :: [Char] -> Parser Char Int
mapParserResToSignedInt res = Parser $ \s -> maybeInt res >>= \x -> Just (x, s)

-- | Map int candidate from 'String' view to 'Int', fails if value less than zero.
mapParserResToNonNegativeInt :: [Char] -> Parser Char Int
mapParserResToNonNegativeInt res = Parser $ \s -> maybeInt res >>= \x ->
  if x < 0
  then Nothing
  else Just (x, s)

-- | Parses a non-negative number to an 'Int'.
-- Stream should not contain spaces or extraneous characters.
nonNegativeIntParser  :: Parser Char Int
nonNegativeIntParser = (parseIntChars <* eof) >>= mapParserResToNonNegativeInt

-- | Works like 'nonNegativeIntParser', but tries to parse max prefix
-- which satisfies integer number, not all stream.
nonNegativeIntTokenParser  :: Parser Char Int
nonNegativeIntTokenParser = (parseIntChars) >>= mapParserResToNonNegativeInt

-- | Parses a number to an 'Int'.
-- A customized reading function is used, but it is very easy to
-- replace it with another. It should be noted that since the line is correct,
-- no errors will occur.
-- Stream should not contain spaces or extraneous characters.
intParser :: Parser Char Int
intParser = (parseIntChars <* eof) >>= mapParserResToSignedInt

-- | Works like 'intParser', but tries to parse max prefix
-- which satisfies integer number, not all stream.
intTokenParser :: Parser Char Int
intTokenParser = (parseIntChars) >>= mapParserResToSignedInt
