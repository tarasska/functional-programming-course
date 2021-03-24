module Block1.Task2Tests
  ( treeInstPropertyTestTree
  , treeInstTestTree
  ) where

import Data.Functor.Compose (Compose (..))
import Data.Functor.Identity (Identity (..))
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.Hspec (Spec, describe, it, shouldBe, testSpec)
import Text.Read (readMaybe)

import Block1.Task2 (Tree (..), toList)

treeInstTestTree :: IO TestTree
treeInstTestTree = testSpec "Tree instances" treeInstSpec

treeInstSpec :: Spec
treeInstSpec = do
  describe "Functor" $ do
    it "+1" $ fmap (+ 1) (Branch (Leaf 0) (Leaf 1)) `shouldBe` Branch (Leaf 1) (Leaf 2)
  describe "Applicative" $ do
    it "tree +2" $ Leaf (+ 2) <*> Branch (Leaf 1) (Leaf 2)
      `shouldBe` Branch (Leaf 3) (Leaf 4)
  describe "Foldable" $ do
    it "tree sum" $ foldr (+) 0 (Branch (Leaf 3) (Leaf 4)) `shouldBe` 7
  describe "Traversable" $ do
    it "maybe string sum" $  traverse (readMaybe :: String -> Maybe Int)
      (Branch (Leaf "0") (Leaf "1")) `shouldBe` Just (Branch (Leaf 0) (Leaf 1))
    it "maybe string sum : fail" $  traverse (readMaybe :: String -> Maybe Int)
      (Branch (Leaf "0") (Leaf "haha")) `shouldBe` Nothing

treeInstPropertyTestTree :: IO TestTree
treeInstPropertyTestTree = return $ testGroup "Tree instance properties"
  [ testProperty "Functor Identity" funcIdProp
  , testProperty "Functor Composition" funcCompProp
  , testProperty "Applicative Identity" applIdProp
  , testProperty "Applicative Composition" funcCompProp
  , testProperty "Applicative Homomorphism" applHomoProp
  , testProperty "Applicative Interchange" applInterProp
  , testProperty "foldr sum tree ≡ foldr sum (toList tree)" foldrTreeProp
  , testProperty "Traversable Naturality" traverseNaturProp
  , testProperty "Traversable Identity" traverseIdProp
  , testProperty "Traversable Composition" traverseCompProp
  ]

genInt :: Gen Int
genInt = Gen.int (Range.constantFrom 0 0 100)

genIntTree :: Int -> Gen (Tree Int)
genIntTree 0     = genInt >>= \x -> return $ Leaf x
genIntTree depth = do
  left  <- genIntTree (depth - 1)
  right <- genIntTree (depth - 1)
  return $ Branch left right

chooseFunc :: Int -> Int -> (Int -> Int)
chooseFunc 0 x = (x *)
chooseFunc 1 x = (x +)
chooseFunc 2 x = (x -)
chooseFunc _ x = const x

genIntToIntTree :: Int -> Gen (Tree (Int -> Int))
genIntToIntTree 0     = genInt >>= \x -> return $ Leaf (chooseFunc (x `mod` 4) x)
genIntToIntTree depth = do
  left  <- genIntToIntTree (depth - 1)
  right <- genIntToIntTree (depth - 1)
  return $ Branch left right

funcIdProp :: Property
funcIdProp = property $ do
  tree <- forAll (genIntTree 5)
  fmap id tree === tree

funcCompProp :: Property
funcCompProp = property $ do
  tree <- forAll (genIntTree 5)
  let f = (* 239)
  let g = (+ 366)
  fmap (f . g) tree === (fmap f . fmap g) tree

applIdProp :: Property
applIdProp = property $ do
  tree <- forAll (genIntTree 5)
  (pure id <*> tree) === tree

applCompProp :: Property
applCompProp = property $ do
  u <- forAll (genIntToIntTree 2)
  v <- forAll (genIntToIntTree 2)
  w <- forAll (genIntTree 2)
  (pure (.) <*> u <*> v <*> w) === (u <*> (v <*> w))

applHomoProp :: Property
applHomoProp = property $ do
  x <- forAll genInt
  let f = (+ 366)
  (pure f <*> pure x) === (pure (f x) :: Tree Int)

applInterProp :: Property
applInterProp = property $ do
  tree <- forAll (genIntToIntTree 2)
  y    <- forAll genInt
  (tree <*> pure y) === (pure ($ y) <*> tree)

foldrTreeProp :: Property
foldrTreeProp = property $ do
  tree <- forAll (genIntTree 4)
  foldr (+) 0 tree === foldr (+) 0 (toList tree)

uselessT :: Maybe a -> Either String a
uselessT Nothing  = Left "error"
uselessT (Just x) = Right x

evenMaybe :: Int -> Maybe Int
evenMaybe x = if even x then Just x else Nothing

traverseNaturProp :: Property
traverseNaturProp = property $ do
  v <- forAll (genIntTree 4)
  (uselessT . traverse evenMaybe) v === traverse (uselessT . evenMaybe) v

traverseIdProp :: Property
traverseIdProp = property $ do
  v <- forAll (genIntTree 4)
  runIdentity (traverse Identity v) === v

traverseCompProp :: Property
traverseCompProp = property $ do
  v <- forAll (genIntTree 4)
  let f = evenMaybe
  let g = (\x -> if x >= 10 then Right x else Left "Less then 10") :: Int -> Either String Int
  traverse (Compose . fmap g . f) v === Compose (fmap (traverse g) (traverse f v))
