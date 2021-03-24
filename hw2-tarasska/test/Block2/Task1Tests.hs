module Block2.Task1Tests
  ( exprPropertyTestTree
  , exprTestTree
  ) where

import Hedgehog (Gen (..), Property (..), forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.Hspec (Spec, describe, it, shouldBe, testSpec)

import Block2.Task1 (ArithmeticError (..), Expr (..), eval)

exprTestTree :: IO TestTree
exprTestTree = testSpec "Expr Int" exprSpec

exprSpec :: Spec
exprSpec = do
  describe "Plus" $ do
    it "1 + 1" $ eval (Val 1 :+: Val 1) `shouldBe` Right (1 + 1)
  describe "Minus" $ do
    it "366 - 239" $  eval (Val 366 :-: Val 239) `shouldBe` Right (366 - 239)
  describe "Mul" $ do
    it "13 * 13" $ eval (Val 13 :*: Val 13) `shouldBe` Right (13 * 13)
  describe "Div" $ do
    it "14 / 2" $  eval (Val 14 :/: Val 2) `shouldBe` Right (14 `div` 2)
    it "Division by zero" $  eval (Val 0 :/: Val 0) `shouldBe` Left DivisionByZero
  describe "Pow" $ do
    it "2 ^ 2" $ eval (Val 2 :^: Val 2) `shouldBe` Right (2 ^ 2)
    it "9 ^ 0" $ eval (Val 9 :^: Val 0) `shouldBe` Right (2 ^ 0)
    it "negative pow" $ eval (Val 366 :^: Val (-1)) `shouldBe` Left NegativePow
  describe "Сombined examples" $ do
    it "Ex. 1" $
      let expr = (Val 2 :+: Val 2 :*: Val 2)
       in eval expr `shouldBe` Right 6
    it "Ex. 2" $
      let expr = ((Val 2 :+: Val 2) :*: Val 2)
       in eval expr `shouldBe` Right 8
    it "Ex. 3" $
      let expr = (Val 2 :^: Val 3 :+: Val 2 :*: Val 0 :-: Val 16 :/: Val 2)
       in eval expr `shouldBe` Right 0
    it "Ex. 4" $
      let expr = ((Val 2 :+: Val 3 :+: Val (-3) :*: Val 2) :^: Val 4)
       in eval expr `shouldBe` Right 1
    it "Ex. 5" $
      let expr = (Val (-2) :*: Val (-2) :+: Val 10) :^: (Val 4 :-: Val 5)
       in eval expr `shouldBe` Left NegativePow

exprPropertyTestTree :: IO TestTree
exprPropertyTestTree = return $ testGroup "Expr property" $
  [ testProperty "X :op: Y ≡ Right (X op Y)" exprProp
  , testProperty "ArithmeticError"  exprErrorProp
  ]

genInt :: Gen Int
genInt = Gen.int (Range.constantFrom 1 1 1000)

exprProp :: Property
exprProp = property $ do
  x <- forAll genInt
  y <- forAll genInt
  eval (Val x :+: Val y) === Right (x + y)
  eval (Val x :-: Val y) === Right (x - y)
  eval (Val x :*: Val y) === Right (x * y)
  eval (Val x :/: Val y) === Right (x `div` y)
  eval (Val (x `mod` 10) :^: Val (y `mod` 5)) === Right ((x `mod` 10) ^ (y `mod` 5))

exprErrorProp :: Property
exprErrorProp = property $ do
  x <- forAll genInt
  y <- forAll genInt
  eval (Val x :/: Val 0) === Left DivisionByZero
  eval (Val x :^: Val (-y)) === Left NegativePow
