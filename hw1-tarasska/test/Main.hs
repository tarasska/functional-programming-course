module Main
  ( main
  ) where

import Test.Tasty (defaultMain, testGroup)

import Block1.Task1Tests (daysOfTheWeekTestTree)
import Block1.Task2Tests (natPropertyTestTree, natTestTree)
import Block1.Task3Tests (binSearchTreeTestTree)
import Block2.Task1Tests (foldablePropertyTestTree, foldableSpecTestTree)
import Block2.Task2Tests (splitJoinPropertyTestTree, splitJoinTestTree)
import Block3.Task1Tests (concatTestTree)
import Block3.Task2Tests (monoidTestTree)

main :: IO ()
main = do
  block1Task1Unit <- daysOfTheWeekTestTree
  block1Task2Unit <- natTestTree
  block1Task2Prop <- natPropertyTestTree
  block1Task3Unit <- binSearchTreeTestTree
  block2Task1Unit <- foldableSpecTestTree
  block2Task1Prop <- foldablePropertyTestTree
  block2Task2Unit <- splitJoinTestTree
  block2Task2Prop <- splitJoinPropertyTestTree
  block3Task1Unit <- concatTestTree
  block3Task2Unit <- monoidTestTree
  defaultMain (testGroup "All Tests"
    [ block1Task1Unit
    , block1Task2Unit, block1Task2Prop
    , block1Task3Unit
    , block2Task1Unit, block2Task1Prop
    , block2Task2Unit, block2Task2Prop
    , block3Task1Unit
    , block3Task2Unit
    ])
