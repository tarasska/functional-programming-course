module Block1.Task1Tests
  ( stringSumPropertyTestTree
  , stringSumTestTree
  ) where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.Hspec (Spec, describe, it, shouldBe, testSpec)

import Block1.Task1 (stringSum)

stringSumTestTree :: IO TestTree
stringSumTestTree = testSpec "stringSum" stringSumSpec

stringSumSpec :: Spec
stringSumSpec = do
  describe "stringSum" $ do
    it "simple sum" $ stringSum "1 2 3" `shouldBe` Just 6
    it "all negative" $ stringSum "-1 -2 -3 -4" `shouldBe` Just (-10)
    it "multiple spaces" $ stringSum "0   14 -2" `shouldBe` Just 12
    it "trailing spaces" $ stringSum "  1   -1 " `shouldBe` Just 0
    it "simple error" $ stringSum "1 2 haha" `shouldBe` Nothing
    it "double error" $ stringSum "1. 2 3 4" `shouldBe` Nothing
    it "whitespaces" $ stringSum "1 \n 2 \t\t\t 3 \n\n\t\n\n -7" `shouldBe` Just (-1)

stringSumPropertyTestTree :: IO TestTree
stringSumPropertyTestTree = return $ testGroup "stringSum property" $
  [ testProperty "Simple correct string ≡ Simple sum" stringSumSuccessSimpleProp
  , testProperty "Hard correct string ≡ Simple sum" stringSumSuccessHardProp
  , testProperty "Incorrect string ≡ Fail" stringSumFailProp
  ]

genInt :: Int -> Int -> Gen Int
genInt lower upper = Gen.int (Range.constantFrom 0 lower upper)

genIntList :: Gen [Int]
genIntList =
  let listLength = Range.linear 0 1000
   in Gen.list listLength Gen.enumBounded

chooseSpace :: Int -> Char
chooseSpace 0 = ' '
chooseSpace 1 = '\t'
chooseSpace 2 = '\n'
chooseSpace 3 = '\r'
chooseSpace 4 = '\f'
chooseSpace _ = '\v'

chooseCrashSymbol :: Int -> Char
chooseCrashSymbol 0 = '?'
chooseCrashSymbol 1 = '!'
chooseCrashSymbol 2 = '.'
chooseCrashSymbol 3 = '0'
chooseCrashSymbol 4 = 'f'
chooseCrashSymbol _ = ';'

genSep :: Bool -> Int -> Gen String
genSep False 0 = return ""
genSep True 0  = return ""
genSep isCorrect len = do
  tailStr <- genSep isCorrect (len - 1)
  x       <- genInt 0 5
  if isCorrect
  then return $ (chooseSpace x) : tailStr
  else return $ (chooseCrashSymbol x) : tailStr

genStringAndList :: Bool -> Int -> Gen (String, [Int])
genStringAndList _ 0            = return ("", [])
genStringAndList isCorrect size = do
  sepLen              <- genInt 1 6
  sep                 <- genSep isCorrect sepLen
  curNum              <- genInt (-100000) 100000
  (tailStr, tailList) <- genStringAndList isCorrect (size - 1)
  return $ ((show curNum) ++ sep ++ tailStr, curNum : tailList)

listToString :: [Int] -> String
listToString xs = foldr (\elem res -> (show elem) ++ " " ++ res) "" xs

stringSumSuccessSimpleProp :: Property
stringSumSuccessSimpleProp = property $
  forAll genIntList >>= \xs ->
  stringSum (listToString xs) === Just (sum xs)

stringSumSuccessHardProp :: Property
stringSumSuccessHardProp = property $ do
  (correctStr, numList) <- forAll (genStringAndList True 100)
  stringSum correctStr === Just (sum numList)

stringSumFailProp :: Property
stringSumFailProp = property $ do
  (incorrectStr, _) <- forAll (genStringAndList False 100)
  stringSum incorrectStr === Nothing
