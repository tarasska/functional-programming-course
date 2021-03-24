module Block3.Task4
  ( -- * Functions
    listlistParser
  , skipSpaces
  ) where

import Control.Applicative (Alternative (..), liftA2)
import Data.Char (isSpace)

import Block3.Task1 (Parser (..))
import Block3.Task2 (element, eof, ok, satisfy, stream)
import Block3.Task3 (intTokenParser, nonNegativeIntTokenParser)

-- | Parses the maximum prefix that satisfies 'isSpace' and discards the result.
skipSpaces :: Parser Char ()
skipSpaces = (satisfy isSpace >> skipSpaces) <|> ok

-- | Parser of a list of lists of numbers separated by commas.
-- Expects that each list is preceded by its correct non-negative length,
-- otherwise the parsing is considered unsuccessful.
listlistParser :: Parser Char [[Int]]
listlistParser = parse <* eof
  where
    parse :: Parser Char [[Int]]
    parse = (fmap (:) parseIntList) <*> (allList listWithComma <|> ([] <$ ok))
      where
        allList :: Parser Char [Int] -> Parser Char [[Int]]
        allList parser =
          let recursiveHelper = liftA2 (:) parser (recursiveHelper <|> pure [])
           in recursiveHelper

        listWithComma :: Parser Char [Int]
        listWithComma = skipSpaces *> (element ',') *> parseIntList

    parseIntList :: Parser Char [Int]
    parseIntList = skipSpaces *> nonNegativeIntTokenParser >>= \res -> helper res
      where
        helper :: Int -> Parser Char [Int]
        helper 0  = return []
        helper sz = (:)
          <$> (skipSpaces *> (element ',') *> skipSpaces *> intTokenParser)
          <*> helper (sz - 1)
