module Block3.Task2Tests
  ( baseCombinatorTestTree
  ) where

import Test.Tasty (TestTree)
import Test.Tasty.Hspec (Spec, describe, it, shouldBe, testSpec)
import Text.Read (readMaybe)

import Block3.Task1 (Parser (..))
import Block3.Task2 (element, eof, ok, satisfy, stream)

baseCombinatorTestTree :: IO TestTree
baseCombinatorTestTree = testSpec "Base combinator" baseCombinatorSpec

baseCombinatorSpec :: Spec
baseCombinatorSpec = do
  describe "element" $ do
    it "Presentation.Ex. Just" $
      let res = runParser (element 'a') "aba"
       in res `shouldBe` Just ('a',"ba")
    it "elem from int stream" $
      let res = runParser (element 1) [1, 2, 3]
       in res `shouldBe` Just (1, [2, 3])
    it "Presentation.Ex. Nothing" $
      let res = runParser (element 'x') "aba"
       in res `shouldBe` Nothing
    it "Wrong elem from double stream" $
      let res = runParser (element 0.5) [1.0, -1.0]
       in res `shouldBe` Nothing
    it "Empty stream" $
      let res = runParser (element 'a') []
       in res `shouldBe` Nothing
  describe "stream" $ do
    it "elems from char stream" $
      let res = runParser (stream "ab") "aba"
       in res `shouldBe` Just ("ab","a")
    it "elems from int stream" $
      let res = runParser (stream [1, 2]) [1, 2]
       in res `shouldBe` Just ([1, 2], [])
    it "Wrong elems from char stream" $
      let res = runParser (stream "ax") "aba"
       in res `shouldBe` Nothing
    it "Wrong elems from double stream" $
      let res = runParser (stream [0.5, 1.0]) [1.0, -1.0]
       in res `shouldBe` Nothing
    it "Query len > stream len" $
      let res = runParser (stream "ab") "a"
       in res `shouldBe` Nothing
  describe "eof" $ do
    it "Empty stream" $ runParser eof "" `shouldBe` Just((), "")
    it "Non-empty stream" $ runParser eof "aba" `shouldBe` Nothing
  describe "ok" $ do
    it "Emtpty ok" $ runParser ok "" `shouldBe` Just((), "")
    it "Non-empty ok" $ runParser ok [1, 2, 3] `shouldBe` Just((), [1, 2, 3])
  describe "satisfy" $ do
    it "Even predicate" $
      let res = runParser (satisfy even) [2, 4, 6]
       in res `shouldBe` Just (2, [4, 6])
    it "Predicate failed" $
      let res = runParser (satisfy even) [3, 4]
       in res `shouldBe` Nothing
