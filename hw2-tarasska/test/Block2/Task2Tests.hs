module Block2.Task2Tests
  ( movingPropertyTestTree
  , movingTestTree
  ) where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty (TestTree)
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.Hspec (Spec, describe, it, shouldBe, testSpec)

import Block2.Task2 (moving)

movingTestTree :: IO TestTree
movingTestTree = testSpec "Moving" movingSpec

movingSpec :: Spec
movingSpec = do
  describe "Moving" $ do
    it "Ex. 1" $ moving 4 [1, 5, 3, 8, 7, 9, 6]
      `shouldBe` [1.0, 3.0, 3.0, 4.25, 5.75, 6.75, 7.5]
    it "Ex. 2" $ moving 2 [1, 5, 3, 8, 7, 9, 6]
      `shouldBe` [1.0, 3.0, 4.0, 5.5, 7.5, 8.0, 7.5]
    it "Empty" $ moving 3 [] `shouldBe` []
    it "Id" $ moving 1 [1, 2, 3] `shouldBe` [1, 2, 3]

movingPropertyTestTree :: IO TestTree
movingPropertyTestTree = return $
  testProperty "Moving ≡ Trivial SMA" movingProp

genInt :: Gen Int
genInt = Gen.int (Range.constantFrom 1 1 10)

genIntList :: Gen [Double]
genIntList =
  let listLength = Range.linear 0 100
   in Gen.list listLength (Gen.double (Range.constantFrom 1 1 100))

trivialSma' :: Int -> [Double] -> [Double]
trivialSma' winSz []            = []
trivialSma' winSz vals@(_ : xs) = cur : (trivialSma' winSz xs)
  where
    win = take winSz vals
    cur = (sum win) / (fromIntegral (length win))

trivialSma :: Int -> [Double] -> [Double]
trivialSma winSz xs = reverse $ trivialSma' winSz (reverse xs)

movingProp :: Property
movingProp = property $ do
  xs    <- forAll genIntList
  winSz <- forAll genInt
  let move = moving winSz xs
  let triv = trivialSma winSz xs
  let eps  = 0.001
  foldr (+) 0 (zipWith (\x y -> if abs(x - y) < eps then 0 else 1) move triv) === 0
