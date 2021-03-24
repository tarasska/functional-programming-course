module Block3.Task2Tests
  ( monoidTestTree
  ) where

import Test.Tasty (TestTree)
import Test.Tasty.Hspec (Spec, describe, it, shouldBe, testSpec)

import Block3.Task2 (Endo (..), Name (..), NonEmpty (..), ThisOrThat (..))

monoidTestTree :: IO TestTree
monoidTestTree = testSpec "Monoid" monoidSpec

monoidSpec :: Spec
monoidSpec =
  let
    isAssociative :: (Eq a, Semigroup a) => a -> a -> a -> Bool
    isAssociative a b c = (a <> (b <> c)) == ((a <> b) <> c)
  in do
    describe "NonEmpty" $ do
      it "366 :| [] <> 228 :| [0]" $
        (366 :| []) <> (228 :| [0]) `shouldBe` 366 :| [228, 0]
      it "'x' :| [] <> 'y' : []" $
        ('x' :| []) <> ('y' :| []) `shouldBe` 'x' :| ['y']
      it "1 :| [] <> 2 :| [3] <> 4 :| [] <> 5 :| [6]" $
        (1 :| []) <> (2 :| [3]) <> (4 :| []) <> (5 :| [6])
          `shouldBe` 1 :| [2, 3, 4, 5, 6]
      it "x <> (y <> z) = (x <> y) <> z" $
        isAssociative ("taras" :| ["love"]) ("haskell" :| []) (":3" :| [])
          `shouldBe` True
      it "x <> (y <> z) = (x <> y) <> z" $
        isAssociative (1 :| [1, 2, 3]) (2 :| [10, 5]) (3 :| [2])
          `shouldBe` True
      it "x <> (y <> z) = (x <> y) <> z" $
        isAssociative ('a' :| []) ('b' :| []) ('c' :| [])
           `shouldBe` True
    describe "ThisOrThat" $ do
      it "This This" $
        ((This 0) <> (This 1) :: ThisOrThat Int Int) `shouldBe` (This 0)
      it "This This That" $
        This (1) <> (This 0) <> (That 366) `shouldBe` (Both 1 366)
      it "That This That" $
        (That (-1)) <> (This 0) <> (That 1) `shouldBe` (Both 0 (-1))
      it "That That" $
        ((That 'a') <> (That 'b') :: ThisOrThat Int Char) `shouldBe` (That 'a')
      it "That This This" $
        (That 1) <> (This 0) <> (This 366) `shouldBe` (Both 0 1)
      it "Both That This" $
        (Both 'a' 3) <> (That 0) <> (This 'z') `shouldBe` (Both 'a' 3)
      it "That Both This" $
        (That 0) <> (Both 'a' 3) <> (This 'z') `shouldBe` (Both 'a' 0)
      it "x <> (y <> z) = (x <> y) <> z" $
        isAssociative (That 0) (Both 'a' 3) (This 'z') `shouldBe` True
      it "x <> (y <> z) = (x <> y) <> z" $
        isAssociative (That 0) (That 3) (This 'z') `shouldBe` True
      it "x <> (y <> z) = (x <> y) <> z" $
        isAssociative (This 'c') (That 'a') (Both 'a' 'c') `shouldBe` True
    describe "Name" $ do
      it "hw1 example" $
        Name "root" <> Name "server" `shouldBe` Name "root.server"
      it "3 names" $
        Name "a" <> Name "b" <> Name "c" `shouldBe` Name "a.b.c"
      it "mempty name" $
        mempty <> Name "b" `shouldBe` Name "b"
      it "name mempty" $
        Name "a" <> mempty `shouldBe` Name "a"
      it "x <> (y <> z) = (x <> y) <> z" $
        isAssociative (Name "a") (Name "b") (Name "c") `shouldBe` True
      it "x <> (y <> z) = (x <> y) <> z" $
        isAssociative (Name "Taras") (Name "love") (Name "haskell") `shouldBe` True
      it "x <> (y <> z) = (x <> y) <> z" $
        isAssociative (Name "Skaz") mempty (Name "ell") `shouldBe` True
    describe "Endo" $ do
      it "x <> (y <> z) = (x <> y) <> z" $
        getEndo (Endo (* 2) <> (Endo (* 4) <> Endo (* 8))) 2
          `shouldBe` getEndo ((Endo (* 2) <> Endo (* 4)) <> Endo (* 8)) 2
      it "x <> (y <> z) = (x <> y) <> z" $
        getEndo (Endo (+ 2) <> (mempty <> Endo (* 8))) 10
          `shouldBe` getEndo ((Endo (+ 2) <> mempty) <> Endo (* 8)) 10
