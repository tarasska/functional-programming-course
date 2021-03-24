module Block3.Task1Tests
  ( concatTestTree
  ) where

import Test.Tasty (TestTree)
import Test.Tasty.Hspec (Spec, describe, it, shouldBe, testSpec)

import Data.Semigroup (Sum (..))

import Block3.Task1 (eitherConcat, maybeConcat)

concatTestTree :: IO TestTree
concatTestTree = testSpec "Monoid concat" concatSpec

concatSpec :: Spec
concatSpec = do
  describe "maybeConcat" $ do
    it "maybeConcat []" $
      maybeConcat ([] :: [Maybe [Int]]) `shouldBe` []
    it "maybeConcat [Just [1,2,3], Nothing, Just [4,5]]" $
      maybeConcat [Just [1,2,3], Nothing, Just [4,5]] `shouldBe` [1, 2, 3, 4, 5]
    it "maybeConcat [Just \"Hello\", Nothing, Nothing, Just \"World\"]" $
      maybeConcat [Just "Hello", Nothing, Nothing, Just "World"] `shouldBe` "HelloWorld"
    it "maybeConcat [Nothing, Nothing, Nothing]" $
      maybeConcat ([Nothing, Nothing, Nothing] :: [Maybe String]) `shouldBe` ""
    it "maybeConcat [Just [0]]" $
      maybeConcat ([Just [0]] :: [Maybe [Int]]) `shouldBe` [0]
  describe "eitherConcat" $ do
    it "eitherConcat [Left [1], Right [2]]" $
      eitherConcat [Left [1], Right [2]] `shouldBe` ([1], [2])
    it "eitherConcat [Left \"taras\", Left \"ska\"]" $
      eitherConcat ([Left "taras", Left "ska"] :: [Either String [Int]])
        `shouldBe` ("tarasska", [])
    it "eitherConcat [Right \"taras\", Right \"ska\"]" $
      eitherConcat ([Right "taras", Right "ska"] :: [Either [Int] String])
        `shouldBe` ([], "tarasska")
    it "Sum hw example" $
      eitherConcat ([Left (Sum 3), Right [1,2,3], Left (Sum 5), Right [4,5]])
        `shouldBe` (Sum {getSum = 8}, [1,2,3,4,5])
