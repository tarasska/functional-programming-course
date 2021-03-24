module Main where

import Criterion.Main (defaultMain, defaultMainWith, defaultConfig)
import Criterion.Types (Config(..))
import System.Directory (createDirectoryIfMissing, getCurrentDirectory)
import System.FilePath ((</>))

import Task1Test (doubleAreaBench, perimeterBench)
import Task2Test (monteCarloBench)
import Task3Test (chtIterOpBench, chtMultiGet, chtMultiPut)

main :: IO ()
main = do
  cht1 <- chtIterOpBench
  cht2 <- chtMultiGet
  cht3 <- chtMultiPut
  let task1 = [ perimeterBench, doubleAreaBench ]
  let task2 = [ monteCarloBench ]
  let task3 = [ cht1, cht2, cht3 ]

  curDir <- getCurrentDirectory
  let path = curDir </> "benchRes"
  createDirectoryIfMissing True path
  defaultMainWith defaultConfig{ reportFile = Just (path </> "task1.html") } task1
  defaultMainWith defaultConfig{ reportFile = Just (path </> "task2.html") } task2
  defaultMainWith defaultConfig{ reportFile = Just (path </> "task3.html") } task3
