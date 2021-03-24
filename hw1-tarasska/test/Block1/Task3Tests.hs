module Block1.Task3Tests
  ( binSearchTreeTestTree
  ) where

import Test.Tasty (TestTree)
import Test.Tasty.Hspec (Spec, describe, it, shouldBe, testSpec)

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty

import Block1.Task2 (Nat (..))
import Block1.Task3 (Tree (..), empty, find, fromList, insert, remove, size)

binSearchTreeTestTree :: IO TestTree
binSearchTreeTestTree = testSpec "BinSearch tree data type" binSearchTreeSpec

binSearchTreeSpec :: Spec
binSearchTreeSpec =
  let
    singleTree :: Ord a => a -> Tree a
    singleTree x = Branch (NonEmpty.fromList [x]) Leaf Leaf

    emptyTree = Leaf :: Tree Int
    zeroTree  = singleTree 0
    twoTree   = singleTree 2
    fullTree  = Branch (NonEmpty.fromList [1, 1]) zeroTree twoTree

    fiveNat   = S $ S $ S $ S $ S Z
    emptyNatT = Leaf :: Tree Nat
    zeroNatT  = singleTree Z
    fiveNatT  = singleTree fiveNat
    fullNatT  = Branch (NonEmpty.fromList [S Z, S Z]) zeroNatT fiveNatT
  in do
    describe "empty" $ do
      it "empty leaf Int" $
        empty emptyTree `shouldBe` True
      it "empty leaf Nat" $
        empty emptyNatT `shouldBe` True
      it "empty non-emtpy tree" $
        empty fullTree `shouldBe` False
    describe "size" $ do
      it "size of empty tree" $
        size emptyTree `shouldBe` 0
      it "size of singleton" $
        size zeroTree `shouldBe` 1
      it "size of 4 keys in 3 nodes" $
        size fullTree `shouldBe` 4
      it "size of 4 keys in 3 nodes with Nat type" $
        size fullNatT `shouldBe` 4
      it "size after remove with Nat type" $
        size (remove (S Z) fullNatT) `shouldBe` 3
    describe "find" $ do
      it "find 366 in empty" $
        find 366 emptyTree `shouldBe` Nothing
      it "find 0 in zero singletone" $
        find 0 zeroTree `shouldBe` Just 0
      it "find 1 in zero singletone" $
        find 1 zeroTree `shouldBe` Nothing
      it "find 2 in 0-(1,1)-2 tree" $
        find 2 fullTree `shouldBe` Just 2
      it "find 3 in [0, 1, 2, 4, 5]" $
        find 3 (fromList [0, 1, 2, 4, 5]) `shouldBe` Nothing
      it "find 0 in [-1, 0, 0, 0, 1]" $
        find 0 (fromList [-1, 0, 0, 0, 1]) `shouldBe` Just 0
      it "find Z in Z-(S Z, S Z)-5SZ with Nat type" $
        find Z fullNatT `shouldBe` Just Z
      it "find SZ in Z-(S Z, S Z)-5SZ with Nat type" $
        find (S Z) fullNatT `shouldBe` Just (S Z)
      it "find SZ in 5SZ with Nat type" $
        find (S Z) fiveNatT `shouldBe` Nothing
    describe "insert" $ do
      it "insert 0 in empty" $
        insert 0 emptyTree `shouldBe` zeroTree
      it "insert 2 in empty" $
        insert 2 emptyTree `shouldBe` twoTree
      it "insert 0->2->1 in 1-tree" $
        insert 1 (insert 2 (insert 0 $ singleTree 1)) `shouldBe` fullTree
      it "insert 5SZ->Z->SZ in SZ-tree with Nat type" $
        insert (S Z) (insert Z (insert fiveNat $ singleTree (S Z)))
          `shouldBe` fullNatT
      it "insert 5SZ to Leaf :: Nat" $
        insert fiveNat Leaf `shouldBe` fiveNatT
    describe "fromList" $ do
      it "fromList [0]" $
        fromList [0] `shouldBe` zeroTree
      it "fromList [2]" $
        fromList [2] `shouldBe` twoTree
      it "fromList [2, 1, 0, 1]" $
        fromList [2, 1, 0, 1] `shouldBe` fullTree
      it "fromList [Z] with Nat type" $
        fromList [Z] `shouldBe` zeroNatT
      it "fromList [5SZ] with Nat type" $
        fromList [fiveNat] `shouldBe` fiveNatT
      it "fromList [SZ, Z, 5SZ, SZ] with Nat type" $
        fromList [S Z, Z, fiveNat, S Z] `shouldBe` fullNatT
    describe "remove" $ do
      it "remove 366 from empty" $
        remove 366 emptyTree `shouldBe` Leaf
      it "remove Z from empty with Nat type" $
        remove Z emptyNatT `shouldBe` Leaf
      it "remove 2 from 2-tree" $
        remove 2 twoTree `shouldBe` Leaf
      it "remove 0 from 2-tree" $
        remove 0 twoTree `shouldBe` twoTree
      it "remove 1 from 1-1-tree" $
        remove 1 (fromList [1, 1]) `shouldBe` singleTree 1
      it "remove 5SZ from 5SZ-tree" $
        remove fiveNat fiveNatT `shouldBe` Leaf
      it "remove SZ->SZ from Z-(S Z, S Z)-5SZ with Nat type" $
        remove (S Z) (remove (S Z) fullNatT) `shouldBe` fromList [fiveNat, Z]
      it "remove SZ->5SZ->SZ from Z-(S Z, S Z)-5SZ with Nat type" $
        remove (S Z) (remove fiveNat (remove (S Z) fullNatT))
          `shouldBe` zeroNatT
