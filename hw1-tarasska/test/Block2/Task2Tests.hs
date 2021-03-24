module Block2.Task2Tests
  ( splitJoinPropertyTestTree
  , splitJoinTestTree
  ) where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Test.Tasty (TestTree)
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.Hspec (Spec, describe, it, shouldBe, testSpec)

import Data.Foldable (toList)
import Data.List (sort)
import Data.List.NonEmpty (fromList)

import Block2.Task2 (joinWith, splitOn)

splitJoinTestTree :: IO TestTree
splitJoinTestTree = testSpec "Split and Join" splitJoinSpec

splitJoinSpec :: Spec
splitJoinSpec = do
  describe "splitOn" $ do
    it "splitOn '/' path/to/file" $
      splitOn '/' "path/to/file" `shouldBe` fromList ["path", "to", "file"]
    it "splitOn 0 [1, 0, 2, 0]" $
      splitOn 0 [1, 0, 2, 0] `shouldBe` fromList [[1],[2],[]]
    it "splitOn 0 [0, 0, 0]" $
      splitOn 9 [9, 9, 9] `shouldBe` fromList [[],[],[],[]]
    it "splitOn 1.0 [-0.1, 2.0, 3.0]" $
      splitOn 1.0 [-0.1, 2.0, 3.0] `shouldBe` fromList [[-0.1, 2.0, 3.0]]
    it "splitOn '.' \"ska.taras\"" $
      splitOn '.' "ska.taras" `shouldBe` fromList ["ska", "taras"]
  describe "joinWith" $ do
    it "joinWith '/' [\"path\", \"to\", \"file\"]" $
      joinWith '/' (fromList ["path", "to", "file"]) `shouldBe` "path/to/file"
    it "joinWith 0 [[[1],[2],[]]" $
      joinWith 0 (fromList [[1],[2],[]]) `shouldBe` [1, 0, 2, 0]
    it "joinWith 9 [[],[],[],[]] " $
      joinWith 9 (fromList [[],[],[],[]])  `shouldBe` [9, 9, 9]
    it "joinWith 1.0 [[-0.1, 2.0, 3.0]] " $
      joinWith 1.0 (fromList [[-0.1, 2.0, 3.0]]) `shouldBe` [-0.1, 2.0, 3.0]
    it "joinWith '.' [\"t\", \"a\", \"r\", \"a\", \"s\"]" $
      joinWith '.' (fromList ["t", "a", "r", "a", "s"]) `shouldBe` "t.a.r.a.s"

splitJoinPropertyTestTree :: IO TestTree
splitJoinPropertyTestTree = return $
  testProperty "joinWith x . splitOn x ≡ id" splitJoinProp

genIntList :: Gen [Int]
genIntList =
  let listLength = Range.linear 0 10000
  in  Gen.list listLength Gen.enumBounded

splitJoinProp :: Property
splitJoinProp = property $ do
  xs  <- forAll genIntList
  sep <- forAll (Gen.int (Range.constantFrom 0 (-1000) 1000))
  joinWith sep (splitOn sep xs) === xs
