module Main
  ( main
  ) where

import           Test.Tasty          (defaultMain, testGroup)

import           ManagerPropertyTest (managerPropertyTestTree)
import           ManagerUnitTest     (cmdTestTree)

main :: IO ()
main = do
  cmdUnit <- cmdTestTree
  cmdProp <- managerPropertyTestTree
  defaultMain (testGroup "All Tests" [cmdUnit, cmdProp])
