module Block3.Task3Tests
  ( intParserPropertyTestTree
  , simpleParserTestTree
  ) where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty (TestTree)
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.Hspec (Spec, describe, it, shouldBe, testSpec)
import Text.Read (readMaybe)

import Block3.Task1 (Parser (..), first)
import Block3.Task2 (element, eof, ok, satisfy, stream)
import Block3.Task3 (CorrectBracketSeq (..), correctBracketSeqParser, intParser,
                     nonNegativeIntParser)

simpleParserTestTree :: IO TestTree
simpleParserTestTree = testSpec "Simple parsers" simpleParserSpec

simpleParserSpec :: Spec
simpleParserSpec = do
  describe "Correct Bracket Sequence" $ do
    it "Empty" $
      let res = runParser correctBracketSeqParser ""
       in res `shouldBe` Just (Empty, "")
    it "One block" $
      let res = runParser correctBracketSeqParser "()"
       in res `shouldBe` Just (Block Empty, "")
    it "Two block" $
      let res = runParser correctBracketSeqParser "()()"
       in res `shouldBe` Just ((Block Empty) :|: (Block Empty), "")
    it "Сorrect Seq" $
      let res = runParser correctBracketSeqParser "(()())()"
       in res `shouldBe` Just ((Block (Block Empty :|: Block Empty)) :|: (Block Empty), "")
    it "Big Example 1, show eq" $
      let res = runParser correctBracketSeqParser "(()(()))((()))()()"
       in fmap (first show) res `shouldBe` Just ("(()(()))((()))()()", "")
    it "Big Example 2, show eq" $
      let res = runParser correctBracketSeqParser "()()((()()()))()(()((())))"
       in fmap (first show) res `shouldBe` Just ("()()((()()()))()(()((())))", "")
    it "Only open '('" $
      let res = runParser correctBracketSeqParser "(((("
       in res `shouldBe` Nothing
    it "Only close ')'" $
      let res = runParser correctBracketSeqParser ")"
       in res `shouldBe` Nothing
    it "Incorrect Seq 1." $
      let res = runParser correctBracketSeqParser "(()()"
       in res `shouldBe` Nothing
    it "Incorrect Seq 2." $
      let res = runParser correctBracketSeqParser "()()))"
       in res `shouldBe` Nothing
  describe "intParser" $ do
    it "Negative" $
      let res = runParser intParser "-239"
       in res `shouldBe` Just ((-239), "")
    it "Positive" $
      let res = runParser intParser "+366"
       in res `shouldBe` Just (366, "")
    it "Unsigned" $
      let res = runParser intParser "100000000"
       in res `shouldBe` Just (100000000, "")
    it "only sign" $
      let res = runParser intParser "-"
       in res `shouldBe` Nothing
    it "Empty" $
      let res = runParser intParser ""
       in res `shouldBe` Nothing
    it "Incorrect char seq 1" $
      let res = runParser intParser "-123f"
       in res `shouldBe` Nothing
    it "Incorrect char seq 2" $
      let res = runParser intParser "-366.0000"
       in res `shouldBe` Nothing
  describe "positiveIntParser" $ do
    it "Negative" $
      let res = runParser nonNegativeIntParser "-239"
       in res `shouldBe` Nothing
    it "Positive" $
      let res = runParser nonNegativeIntParser "+366"
       in res `shouldBe` Just (366, "")
    it "Unsigned" $
      let res = runParser nonNegativeIntParser "100"
       in res `shouldBe` Just (100, "")

intParserPropertyTestTree :: IO TestTree
intParserPropertyTestTree = return $
  testProperty "Correct strings ≡ numbers" intParserProperty

genInt :: Gen Int
genInt = Gen.enumBounded

intParserProperty :: Property
intParserProperty = property $ do
  x <- forAll genInt
  runParser intParser (show x) === Just (x, "")
