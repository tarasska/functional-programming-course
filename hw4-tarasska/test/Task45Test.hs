{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Task45Test
  ( halyavaScriptTestTree
  ) where

import Control.Monad.ST (ST, runST)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.Hspec (Spec, describe, it, shouldBe, testSpec)

import Task45 (HalyavaScript (..), HalyavaScriptEx (..), runHS, transToJS)

halyavaScriptTestTree :: IO TestTree
halyavaScriptTestTree = testSpec "HalyavaScript calc test" hsSpec

log2 :: HalyavaScriptEx m => Int -> m Int
log2 = sFun1 0 $ \a logCnt ->
         sWithVar 0 $ \accum ->
           accum @= 1 #
           logCnt @= 0 #

           sWhile (a @@> accum)
             -- the following lines use different syntax
             -- with the same meaning to show variability
             (accum  @@= (eRead accum $ \accVal -> vLift $ accVal + accVal) #
              logCnt @@= (logCnt @+ 1)
             )

log2ToTrans :: HalyavaScript m => Int -> m Int
log2ToTrans = sFun1 0 $ \a logCnt ->
                sWithVar 0 $ \accum ->
                  accum @= 1 #
                  logCnt @= 0 #

                  sWhile (a @@> accum)
                    (accum  @@= (accum @+@ accum) #
                     logCnt @@= (logCnt @+ 1)
                    )

helloSample :: HalyavaScriptEx m => String -> m String
helloSample = sFun1 "Hello" $ \name hello ->
                hello @@= (eRead2 hello name $ \hv nv -> vLift $ hv ++ " " ++ nv)

diffTypeSample :: HalyavaScript m => Bool -> Int -> m String
diffTypeSample = sFun2 "" $ \isNigth age verdict ->
  sIf (isNigth @== True)
    (verdict @= "We don't sell alcohol at night.")
    (sIf (age @>= 18)
      (verdict @= "Ok. Here is your vodka.")
      (verdict @= "Selling alcohol to children is prohibited.")
    )

hsSpec :: Spec
hsSpec = do
  describe "Simple calculation" $ do
    it "log2 1" $ do
      runHS (log2 1) `shouldBe` 0
    it "log2 8" $ do
      runHS (log2 8) `shouldBe` 3
    it "log2 31" $ do
      runHS (log2 31) `shouldBe` 5
    it "Hello world" $ do
      runHS (helloSample "world") `shouldBe` "Hello world"
  describe "Different type" $ do
    it "Vodka ok" $ do
      runHS (diffTypeSample False 20) `shouldBe` "Ok. Here is your vodka."
    it "No vodka :( " $ do
      runHS (diffTypeSample True 69) `shouldBe` "We don't sell alcohol at night."
  describe "Translation to JS" $ do
    it "HW example" $ do
      let actualCode   = transToJS (log2ToTrans 2)
      let expectedCode = "function(var_0){\n\
                         \  var var_1 = 0;\n\
                         \  var var_2 = 0;\n\
                         \  var_2 = 1;\n\
                         \  var_1 = 0;\n\
                         \  while (var_0 > var_2) {\n\
                         \    var_2 = var_2 + var_2;\n\
                         \    var_1 = var_1 + 1;\n\
                         \  }\n\
                         \}\n"
      actualCode `shouldBe` expectedCode
    it "Vodka again!" $ do
      let actualCode   = transToJS (diffTypeSample False 17)

      let expectedCode = "function(var_0, var_1){\n\
                         \  var var_2 = \"\";\n\
                         \  if (var_0 == True) {\n\
                         \    var_2 = \"We don't sell alcohol at night.\";\n\
                         \  } else {\n\
                         \    if (var_1 >= 18) {\n\
                         \      var_2 = \"Ok. Here is your vodka.\";\n\
                         \    } else {\n\
                         \      var_2 = \"Selling alcohol to children is prohibited.\";\n\
                         \    }\n\
                         \  }\n\
                         \}\n"
      actualCode `shouldBe` expectedCode
