module Block3.Task4Tests
  ( hardParserPropertyTestTree
  , hardParserTestTree
  ) where

import Data.List (intercalate)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty (TestTree)
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.Hspec (Spec, describe, it, shouldBe, testSpec)

import Block3.Task1 (Parser (..))
import Block3.Task2 (element, eof, ok, satisfy, stream)
import Block3.Task3 (CorrectBracketSeq (..), correctBracketSeqParser, intParser)
import Block3.Task4 (listlistParser, skipSpaces)

hardParserTestTree :: IO TestTree
hardParserTestTree = testSpec "Hard parsers" hardParserSpec

hardParserSpec :: Spec
hardParserSpec = do
  describe "Spaces" $ do
    it "Space" $
      let res = runParser skipSpaces "     "
       in res `shouldBe` Just ((), "")
    it "With Tabs" $
      let res = runParser skipSpaces "\t\t \t  \t"
       in res `shouldBe` Just ((), "")
  describe "List of lists of Int" $ do
    it "[[int]]" $
      let res = runParser listlistParser "5,1,2,3,4,5"
       in res `shouldBe` Just ([[1, 2, 3, 4, 5]], "")
    it "[[int], [int]]" $
      let res = runParser listlistParser "1,2, 2, 1, 2"
       in res `shouldBe` Just ([[2], [1, 2]], "")
    it "[[int], [int]] with tabs" $
      let res = runParser listlistParser "2,-1,\t2,  3,   6, -100, 0"
       in res `shouldBe` Just ([[-1, 2], [6, -100, 0]], "")
    it "Example" $
      let res = runParser listlistParser "2, 1,+10  , 3,5,-7, 2"
       in res `shouldBe` Just ([[1, 10], [5, -7, 2]], "")
    it "Empty list" $
      let res = runParser listlistParser "0, 1, 2"
       in res `shouldBe` Just ([[], [2]], "")
    it "Doubles" $
      let res = runParser listlistParser "2,-1.0, 3.3"
       in res `shouldBe` Nothing
    it "Missing ','" $
      let res = runParser listlistParser "2 -1, 228"
       in res `shouldBe` Nothing
    it "Extra ','" $
      let res = runParser listlistParser "2,    -1, 3,"
       in res `shouldBe` Nothing
    it "Negative len" $
      let res = runParser listlistParser "-2, 1, 2"
       in res `shouldBe` Nothing

hardParserPropertyTestTree :: IO TestTree
hardParserPropertyTestTree = return $
  testProperty "parse (str representation of int list) ≡ correct int list" hardParserProp

genInt :: Int -> Int -> Gen Int
genInt lower upper = Gen.int (Range.constantFrom 0 lower upper)

genIntList :: Gen [Int]
genIntList =
  let listLength = Range.linear 1 100
   in Gen.list listLength (genInt (-1000) 1000)

genIntListList :: Gen [[Int]]
genIntListList =
  let listLength = Range.linear 1 100
   in Gen.list listLength genIntList

chooseSpace :: Int -> Char
chooseSpace 0 = ' '
chooseSpace 1 = '\t'
chooseSpace 2 = '\n'
chooseSpace 3 = '\r'
chooseSpace 4 = '\f'
chooseSpace _ = '\v'

genSepStr :: Int -> Int -> Gen String
genSepStr 0   0        = return ","
genSepStr 0   _        = return ""
genSepStr len commaPos = do
  tailStr <- genSepStr (len - 1) commaPos
  x       <- genInt 0 5
  if len == commaPos
  then return $ ',' : (chooseSpace x) : tailStr
  else return $ (chooseSpace x) : tailStr

listListToStr :: [[Int]] -> Gen String
listListToStr []       = return ""
listListToStr (l : ls) = do
  sepLen   <- genInt 1 6
  commaPos <- genInt 0 sepLen
  sep      <- genSepStr sepLen commaPos
  tailStr  <- listListToStr ls
  case ls of
    [] -> return $ intercalate sep (map show ((length l) : l))
    _  -> return $ intercalate sep (map show ((length l) : l)) ++ (',' : tailStr)

hardParserProp :: Property
hardParserProp = property $ do
  listlist    <- forAll genIntListList
  strListList <- forAll (listListToStr listlist)
  runParser listlistParser strListList === Just (listlist, "")
