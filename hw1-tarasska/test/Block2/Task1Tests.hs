module Block2.Task1Tests
  ( foldableSpecTestTree,
    foldablePropertyTestTree
  ) where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Test.Tasty (TestTree)
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.Hspec (Spec, describe, it, shouldBe, testSpec)

import Data.Foldable (toList)
import Data.List (sort)

import Block2.Task1 (Tree (..), fromList)

foldableSpecTestTree :: IO TestTree
foldableSpecTestTree = testSpec "Foldable tree" foldableSpec

foldableSpec :: Spec
foldableSpec = do
  describe "Foldable" $ do
    it "sum 0..3" $
      foldr (+) 0 (fromList [0, 1, 2, 3]) `shouldBe` 6
    it "pow 1..5" $
      foldr (*) 1 (fromList [1, 2, 3, 4, 5]) `shouldBe` 120
    it "Maybe [a]" $
      foldMap (\x -> Just x) (fromList [[0], [1], [2]]) `shouldBe` Just [0, 1, 2]

foldablePropertyTestTree :: IO TestTree
foldablePropertyTestTree = return $ testProperty "toList . fromList ≡ id" foldIntProp

genIntList :: Gen [Int]
genIntList =
  let listLength = Range.linear 0 10000
  in  Gen.list listLength Gen.enumBounded

foldIntProp :: Property
foldIntProp = property $
  forAll genIntList >>= \xs ->
  toList (fromList xs) === sort xs
