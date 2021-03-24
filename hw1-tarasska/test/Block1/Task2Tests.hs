module Block1.Task2Tests
  ( natPropertyTestTree
  , natTestTree
  ) where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.Hspec (Spec, describe, it, shouldBe, testSpec)

import Block1.Task2 (Nat (..), isEven, isOdd)

natTestTree :: IO TestTree
natTestTree = testSpec "Nat data type" natSpec

natSpec :: Spec
natSpec =
  let
    zero    = Z
    one     = S Z
    two     = S $ S Z
    three   = S $ S $ S $ Z
    five    = S $ S $ S $ S $ S Z
    ten     = S $ S $ S $ S $ S $ S $ S $ S $ S $ S Z
    six     = fromInteger 6
    seven   = fromInteger 7
    fifteen = fromInteger 15
  in do
    describe "fromInteger" $ do
      it "from 0" $ fromInteger 0 `shouldBe` zero
      it "from 1" $ fromInteger 1 `shouldBe` one
      it "from 2" $ fromInteger 2 `shouldBe` two
      it "from 3" $ fromInteger 3 `shouldBe` three
      it "from 5" $ fromInteger 5 `shouldBe` five
      it "from 10" $ fromInteger 10 `shouldBe` ten
    describe "toInteger" $ do
      it "to 0" $ toInteger zero `shouldBe` 0
      it "to 1" $ toInteger one `shouldBe` 1
      it "to 2" $ toInteger two `shouldBe` 2
      it "to 3" $ toInteger three `shouldBe` 3
      it "to 5" $ toInteger five `shouldBe` 5
      it "to 10" $ toInteger ten `shouldBe` 10
    describe "x + y" $ do
      it "0 + 0 == 0" $ (zero + zero) `shouldBe` zero
      it "0 + 1 == 1" $ (zero + one) `shouldBe` one
      it "5 + 10 == 15" $ (five + ten) `shouldBe` fifteen
      it "5 + 5 == 10" $ (five + five) `shouldBe` ten
      it "1 + 2 + 2 == 5" $ (one + two + two) `shouldBe` five
      it "6 + 7 + 2 == 15" $ (six + seven + two) `shouldBe` fifteen
    describe "x * y" $ do
      it "0 * 0 == 0" $ (zero * zero) `shouldBe` zero
      it "15 * 0 == 0" $ (fifteen * zero) `shouldBe` zero
      it "1 * 2 == 2" $ (one * two) `shouldBe` two
      it "3 * 5 == 15" $ (three * five) `shouldBe` fifteen
      it "3 * 2 == 6" $ (three * two) `shouldBe` six
      it "3 * 15 * 0 == 0" $ (three * fifteen * zero) `shouldBe` zero
    describe "x - y" $ do
      it "0 - 0 == 0" $ (zero - zero) `shouldBe` zero
      it "1 - 0 == 1" $ (one - zero) `shouldBe` one
      it "10 - 5 == 5" $ (ten - five) `shouldBe` five
      it "15 - 5 == 10" $ (fifteen - five) `shouldBe` ten
      it "5 - 2 - 1 == 2" $ (five - two - one) `shouldBe` two
    describe "x == y" $ do
      it "0 == 0 - True" $ (zero == zero) `shouldBe` True
      it "5 == 5 - True" $ (five == five) `shouldBe` True
      it "1 == 10 - False" $ (one == ten) `shouldBe` False
      it "5 == 3 - False" $ (five == three) `shouldBe` False
      it "5 /= 3 - True" $ (five /= three) `shouldBe` True
      it "2 /= 10 - True" $ (two /= ten) `shouldBe` True
      it "2 /= 2 - False" $ (two /= two) `shouldBe` False
    describe "x <=> y" $ do
      it "0 <= 0 - True" $ (zero <= zero) `shouldBe` True
      it "3 < 3 - False" $ (three < three) `shouldBe` False
      it "1 > 0 - True" $ (one > zero) `shouldBe` True
      it "10 <= 2 - False" $ (ten <= two) `shouldBe` False
      it "10 < 15 - True" $ (ten < fifteen) `shouldBe` True
      it "5 >= 5 - True" $ (five >= five) `shouldBe` True
      it "6 > 15 - False" $ (six > fifteen) `shouldBe` False
    describe "isEven x" $ do
      it "isEven 0" $ isEven zero `shouldBe` True
      it "isEven 2" $ isEven two `shouldBe` True
      it "isEven 3" $ isEven three `shouldBe` False
      it "isEven 5" $ isEven five `shouldBe` False
      it "isEven 10" $ isEven ten `shouldBe` True
    describe "isOdd x" $ do
      it "isOdd 0" $ isOdd zero `shouldBe` False
      it "isOdd 2" $ isOdd two `shouldBe` False
      it "isOdd 3" $ isOdd three `shouldBe` True
      it "isOdd 5" $ isOdd five `shouldBe` True
      it "isOdd 10" $ isOdd ten `shouldBe` False
    describe "x div y" $ do
      it "10 div 2" $ (ten `div` two) `shouldBe` five
      it "15 div 2" $ (fifteen `div` two) `shouldBe` seven
      it "6 quot 3" $ (six `quot` three) `shouldBe` two
      it "5 quot 5" $ (five `quot` five) `shouldBe` one
      it "3 div 15" $ (three `div` fifteen) `shouldBe` zero
    describe "x mod y" $ do
      it "10 mod 2" $ (ten `mod` two) `shouldBe` zero
      it "15 mod 2" $ (fifteen `mod` two) `shouldBe` one
      it "6 mod 3" $ (six `mod` three) `shouldBe` zero
      it "5 mod 5" $ (five `mod` five) `shouldBe` zero
      it "3 rem 15" $ (three `rem` fifteen) `shouldBe` three
      it "1 rem 2" $ (one `rem` two) `shouldBe` one

natPropertyTestTree :: IO TestTree
natPropertyTestTree = return $ testGroup "Nat property tests"
  [ testProperty "toEnum . fromEnum ≡ id" natEnumProp
  , testProperty "(a + b) - b ≡ a" natPlusMinusProp
  , testProperty "(a * b) `div` b ≡ a" natMulDivProp
  , testProperty "(a `quot` b) * b + (a `rem` b) ≡ a" natQuotRemProp
  , testProperty "(a + b) + c ≡ a + (b + c)" associativePlusProp
  , testProperty "(a * b) * c ≡ a * (b * c)" associativeMulProp
  , testProperty "isEven a ≡ isOdd (a + 1)" natEvenOddProp
  ]

genNat :: Gen Nat
genNat = (Gen.int (Range.constantFrom 0 0 100)) >>= (return . toEnum)

natEnumProp :: Property
natEnumProp = property $ do
  a <- forAll genNat
  toEnum (fromEnum a) === a

natPlusMinusProp :: Property
natPlusMinusProp = property $ do
  a <- forAll genNat
  b <- forAll genNat
  (a + b) - b === a

natMulDivProp :: Property
natMulDivProp = property $ do
  a <- forAll genNat
  b <- forAll genNat
  (a * (b + 1)) `div` (b + 1) === a

natQuotRemProp :: Property
natQuotRemProp = property $ do
  a <- forAll genNat
  b <- forAll genNat
  (a `quot` (b + 1)) * (b + 1) + (a `rem` (b + 1)) === a

associativePlusProp :: Property
associativePlusProp = property $ do
  a <- forAll genNat
  b <- forAll genNat
  c <- forAll genNat
  (a + b) + c  === a + (b + c)

associativeMulProp :: Property
associativeMulProp = property $ do
  a <- forAll genNat
  b <- forAll genNat
  c <- forAll genNat
  (a * b) * c  === a * (b * c)

natEvenOddProp :: Property
natEvenOddProp = property $ do
  a <- forAll genNat
  isEven a === isOdd (a + 1)
