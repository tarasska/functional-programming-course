module Block1.Task3Tests
  ( nonEmptyPropertyTestTree
  , nonEmptyTestTree
  ) where

import Data.Char (chr)
import qualified Data.List.NonEmpty as StdNonEmpty
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.Hspec (Spec, describe, it, shouldBe, testSpec)
import Text.Read (readMaybe)

import Block1.Task3 (NonEmpty (..), fromList, toList)

nonEmptyTestTree :: IO TestTree
nonEmptyTestTree = testSpec "Tree instances" nonEmptySpec

nonEmptySpec :: Spec
nonEmptySpec = do
  describe "Functor" $ do
    it "+1" $ fmap (+ 1) (1 :| [2, 3]) `shouldBe` (2 :| [3, 4])
  describe "Applicative" $ do
    it "tree +2" $  (+ 2) :| [] <*> (10 :| [(-2), 0]) `shouldBe` (12 :| [0, 2])
  describe "Foldable" $ do
    it "tree sum" $ foldr (+) 0 (1 :| [2, 3]) `shouldBe` 6
  describe "Traversable" $ do
    it "maybe string sum" $  traverse (readMaybe :: String -> Maybe Int)
      ("0" :| ["1", "2"]) `shouldBe` Just (0 :| [1, 2])
    it "maybe string sum : fail" $  traverse (readMaybe :: String -> Maybe Int)
      ("0" :| ["1f", "2"]) `shouldBe` Nothing
  describe ">>=" $ do
    it "map" $ (1 :| [2, 3] >>= return . show) `shouldBe` "1" :|  ["2", "3"]


nonEmptyPropertyTestTree :: IO TestTree
nonEmptyPropertyTestTree = return $ testGroup "Equality to std NonEmpty"
  [ testProperty "Functor" functorProp
  , testProperty "Applicative" applicativeProp
  , testProperty "Foldable" foldableProp
  , testProperty "Traversable" traversableProp
  , testProperty ">>=" monadArrowProp
  , testProperty ">>= Eq to simple list" monadArrowSimpleListProp
  ]

genInt :: Gen Int
genInt = Gen.int (Range.constantFrom 0 0 100)

genIntList :: Gen [Int]
genIntList =
  let listLength = Range.linear 1 1000
   in Gen.list listLength genInt

genNumStr :: Bool -> Gen String
genNumStr False = genInt >>= \x -> return $ show x
genNumStr True  = genInt >>= \x -> return $ (show x) ++ [chr x, '$']

genNumStrList :: Bool -> Gen [String]
genNumStrList fail =
  let listLength = Range.linear 1 1000
   in Gen.list listLength (genNumStr fail)

chooseFunc :: Int -> Int -> (Int -> Int)
chooseFunc 0 x = (x *)
chooseFunc 1 x = (x +)
chooseFunc 2 x = (x -)
chooseFunc _ x = const x

chooseOper :: Int -> Int -> (Int -> Int)
chooseOper 0 = (*)
chooseOper 1 = (+)
chooseOper _ = (-)

genIntFuncList :: Int -> Gen [Int -> Int]
genIntFuncList 0   = return []
genIntFuncList len = do
  tail <- genIntFuncList (len - 1)
  cur  <- genInt >>= \x -> return [chooseFunc (x `mod` 4) x]
  return (cur ++ tail)

functorProp :: Property
functorProp = property $ do
  xs <- forAll genIntList
  y  <- forAll genInt
  let f = chooseFunc (y `mod` 4) y
  toList (fmap f (fromList xs)) === StdNonEmpty.toList (fmap f (StdNonEmpty.fromList xs))

applicativeProp :: Property
applicativeProp = property $ do
  fs <- forAll (genIntFuncList 5)
  xs <- forAll genIntList
  toList ((fromList fs) <*> (fromList xs))
    === StdNonEmpty.toList ((StdNonEmpty.fromList fs) <*> (StdNonEmpty.fromList xs))

foldableProp :: Property
foldableProp = property $ do
  xs <- forAll genIntList
  y  <- forAll genInt
  let op = chooseOper (y `mod` 3)
  foldr op 0 (fromList xs) === foldr op 0 (StdNonEmpty.fromList xs)
  foldl op 0 (fromList xs) === foldl op 0 (StdNonEmpty.fromList xs)

traversableProp :: Property
traversableProp = property $ do
  nums <- forAll (genNumStrList False)
  strs <- forAll (genNumStrList True)
  let f   = readMaybe :: String -> Maybe Int
  fmap toList (traverse f (fromList nums))
    === fmap StdNonEmpty.toList (traverse f (StdNonEmpty.fromList nums))
  fmap toList (traverse f (fromList strs))
    === fmap StdNonEmpty.toList (traverse f (StdNonEmpty.fromList strs))

monadArrowProp :: Property
monadArrowProp = property $ do
  xs <- forAll genIntList
  toList ((fromList xs) >>= return . show)
    === StdNonEmpty.toList ((StdNonEmpty.fromList xs) >>= return . show)

monadArrowSimpleListProp :: Property
monadArrowSimpleListProp = property $ do
  xs <- forAll genIntList
  let neMap   = \x -> x :| replicate (x `mod` 5 + 1) x
  toList (fromList xs >>= neMap) === (xs >>= \x -> replicate (x `mod` 5 + 2) x)
