{-# LANGUAGE LambdaCase #-}

module Block3.Task1Tests
  ( parserInstTestTree
  ) where

import Control.Applicative (Alternative (..))
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (Spec, describe, it, shouldBe, testSpec)
import Text.Read (readMaybe)

import Block3.Task1 (Parser (..))
import Block3.Task2 (element, eof, ok, satisfy, stream)

parserInstTestTree :: IO TestTree
parserInstTestTree = testSpec "Parser instances" parserInstSpec

-- | Here are some simple tests for the raw parser,
-- because I'm convinced that the tests in the following blocks
-- cover most of the parser use cases.
parserInstSpec :: Spec
parserInstSpec = do
  describe "Some raw parser tests" $ do
    it "Functor usage 1, fmap" $
      let parser = fmap (False ||) bitParser
       in runParser parser "1" `shouldBe` Just (True, "")
    it "Functor usage 2, <$>" $
      let parser = not <$> bitParser
       in runParser parser "0" `shouldBe` Just (True, "")
    it "Functor usage 3, O as letter, not zero)" $
      let parser = not <$> bitParser
       in runParser parser "O" `shouldBe` Nothing
    it "Simple int parser" $
      let parser = Parser $ \s -> (readMaybe s :: Maybe Int) >>= \val -> Just (val, "")
       in runParser parser "14" `shouldBe` Just (14, "")
  describe "With base parsers" $ do
    it "Alternative or" $ do
      let parser = element '0' <|> element '1' <|> element '!'
       in runParser parser "!0" `shouldBe` Just ('!', "0")
    it "Applicative map succes" $ do
      let notParser = fmap (\ignore -> not) (element '!')
       in runParser (notParser <*> bitParser) "!0" `shouldBe` Just (True, "")
    it "Applicative map fail" $ do
      let notParser = fmap (\ignore -> not) (element '!')
       in runParser (notParser <*> bitParser) "?0" `shouldBe` Nothing
  where
    bitParser :: Parser Char Bool
    bitParser = Parser $ \case
      ('0' : s) -> Just (False, s)
      ('1' : s) -> Just (True, s)
      _          -> Nothing
