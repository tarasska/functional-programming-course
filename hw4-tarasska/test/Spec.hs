module Main
  ( main
  ) where

import Test.Tasty (defaultMain, testGroup)

import Task1Test (geometryPerfTestTree, geometryTestTree)
import Task2Test (monteCarloPerfTestTree)
import Task3Test (chtPerfTestTree, chtTestTree)
import Task45Test (halyavaScriptTestTree)
import Task67Test (fsLensRunTestTree)

main :: IO ()
main = do
  putStrLn $ "Please note that performance tests are duplicated both in\
    \tests and in a separate benchmark, because\
    \I was not sure in which format they should be provided."
  task1Unit <- geometryTestTree
  task1Perf <- geometryPerfTestTree
  task2Perf <- monteCarloPerfTestTree
  task3Unit <- chtTestTree
  task3Perf <- chtPerfTestTree
  task4Unit <- halyavaScriptTestTree
  task67Unit <- fsLensRunTestTree
  defaultMain (testGroup "All test"
    [ task1Unit, task1Perf
    , task2Perf
    , task3Unit, task3Perf
    , task4Unit
    , task67Unit
    ])
