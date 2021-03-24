module Main
  ( main
  ) where

import Test.Tasty (defaultMain, testGroup)

import Block1.Task1Tests (stringSumPropertyTestTree, stringSumTestTree)
import Block1.Task2Tests (treeInstPropertyTestTree, treeInstTestTree)
import Block1.Task3Tests (nonEmptyPropertyTestTree, nonEmptyTestTree)
import Block2.Task1Tests (exprPropertyTestTree, exprTestTree)
import Block2.Task2Tests (movingPropertyTestTree, movingTestTree)
import Block3.Task1Tests (parserInstTestTree)
import Block3.Task2Tests (baseCombinatorTestTree)
import Block3.Task3Tests (intParserPropertyTestTree, simpleParserTestTree)
import Block3.Task4Tests (hardParserPropertyTestTree, hardParserTestTree)

main :: IO ()
main = do
  block1Task1Unit <- stringSumTestTree
  block1Task1Prop <- stringSumPropertyTestTree
  block1Task2Unit <- treeInstTestTree
  block1Task2Prop <- treeInstPropertyTestTree
  block1Task3Unit <- nonEmptyTestTree
  block1Task3Prop <- nonEmptyPropertyTestTree
  block2Task1Unit <- exprTestTree
  block2Task1Prop <- exprPropertyTestTree
  block2Task2Unit <- movingTestTree
  block2Task2Prop <- movingPropertyTestTree
  block3Task1Unit <- parserInstTestTree
  block3Task2Unit <- baseCombinatorTestTree
  block3Task3Unit <- simpleParserTestTree
  block3Task3Prop <- intParserPropertyTestTree
  block3Task4Unit <- hardParserTestTree
  block3Task4Prop <- hardParserPropertyTestTree
  defaultMain (testGroup "All Tests" [
      block1Task1Unit, block1Task1Prop
    , block1Task2Unit, block1Task2Prop
    , block1Task3Unit, block1Task3Prop
    , block2Task1Unit, block2Task1Prop
    , block2Task2Unit, block2Task2Prop
    , block3Task1Unit
    , block3Task2Unit
    , block3Task3Unit, block3Task3Prop
    , block3Task4Unit, block3Task4Prop
    ])
