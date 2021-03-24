{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Main
Description : Contains the main body of the application
Copyright   : (c) Skazhenik Taras 2020
License     : BSD-3-Clause
Maintainer  : taras.skazhenik@yandex.ru

This is the FIRST(old) version of the functional programming homework.
The file manager uploads the file system into RAM at the specified path, be careful!
The manager provides all the commands indicated in the example for
the description of the homework. Additionally, the ability to output the
system tree to the console is provided. The implementation of the manager
is completely decoupled from the real world. The manager is launched if the
download has occurred without errors. The upload tries to add whatever it can
and tries to work even with errors.
-}
module Main where

import           Control.Exception               (SomeException (..), catch)
import           Control.Monad.State             (runState)
import           Control.Monad.Trans.Except      (runExceptT)
import qualified Data.ByteString.UTF8            as BS8
import qualified Data.Time.Clock                 as Time
import           Options.Applicative             (ParserResult (..),
                                                  defaultPrefs, execCompletion,
                                                  execParserPure, renderFailure)
import           System.FilePath                 ((</>))
import           System.IO                       (hFlush, stdout)

import           ArgParser                       (cmdParser, mainArgParser,
                                                  splitToCmdArgs)
import           FileManager                     (execCat, execCd,
                                                  execCreateFile,
                                                  execCreateFolder,
                                                  execFindFile, execInformation,
                                                  execLs, execRemove, execTree,
                                                  execWriteFile)
import           RealFileSystem.FileSystemReader (readFileSystem)
import           RealFileSystem.FileSystemWriter (writeNewFileSystem)
import           Structure.ControlType           (Cmd (..), RootPath (..))
import           Structure.Error                 (CustomPrintableEither (..),
                                                  exceptionHandler)
import           Structure.FileSystemType        (FileSysRoot (..),
                                                  ManagerState (..), SafeState)

-- | Main initializes file manager with path provided as command line argument.
main :: IO ()
main = initManager =<< mainArgParser

-- | Try to load file system. If an error occurs during loading,
-- it is displayed on the screen, the application is closed.
initManager :: RootPath -> IO ()
initManager (RootPath path) =
  (readFileSystem path >>= \fs -> mainLoop (ManagerState fs "")) `catch` handler
  where
    msg = "Manager stopped. System information: "

    handler :: SomeException -> IO ()
    handler e = putStrLn $ msg ++ (show e)

-- | The function accepts the current state, waits for a new command
-- and starts processing it.
mainLoop :: ManagerState -> IO ()
mainLoop ms@ManagerState{fsRoot = root, curPath = path} = do
  printLocationBar (rootPath root </> path)
  input <- getLine
  case (splitToCmdArgs input) of
    Just parts -> case (execParserPure defaultPrefs cmdParser parts) of
      Failure err         -> (putStrLn $ fst $ renderFailure err "")    >> mainLoop ms
      CompletionInvoked c -> (execCompletion c "" >>= \s -> putStrLn s) >> mainLoop ms
      Success cmd         -> commandDispatcher ms cmd

    Nothing    -> (putStrLn $ "Unable to parse command, use --help") >> mainLoop ms

-- | Takes the current state and command, selects the desired function
-- and sends it for execution.
commandDispatcher :: ManagerState -> Cmd -> IO ()
commandDispatcher ms = \case
  Cat path            -> changeState (execCat path) ms
  Cd  path            -> changeState (execCd path) ms
  CreateFile name     -> Time.getCurrentTime >>= \time ->
    changeState (execCreateFile name time) ms
  CreateFolder name   -> changeState (execCreateFolder name) ms
  DirCmd              -> commandDispatcher ms (Ls "./")
  FindFile name       -> changeState (execFindFile name) ms
  Information path    -> changeState (execInformation path) ms
  Ls path             -> changeState (execLs path) ms
  Quit                -> writeNewFileSystem (fsRoot ms)
  Remove path         -> changeState (execRemove path) ms
  Tree path           -> changeState (execTree path) ms
  WriteFile path text -> Time.getCurrentTime >>= \time ->
    changeState (execWriteFile path (BS8.fromString text) time) ms

-- | Executes the given command, processes the result, and display it on the screen.
-- Then it transfers control to the 'mainLoop'.
changeState :: CustomPrintableEither a => SafeState a -> ManagerState -> IO ()
changeState cmdRes ms =
  let (result, newSt) = runState (runExceptT cmdRes) ms
   in exceptionHandler result >> mainLoop newSt

-- | Outputs the current path in green to contrast with the rest of the white text.
printLocationBar :: FilePath -> IO ()
printLocationBar path = do
  putStr $ path ++ " |> "
  hFlush stdout
