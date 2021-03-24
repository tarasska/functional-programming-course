{-# LANGUAGE ScopedTypeVariables #-}

module RealFileSystem.FileSystemWriter
  ( -- * Function
    writeNewFileSystem
  ) where

import           Control.Exception        (SomeException, catch)
import           Control.Monad            (filterM, forM_)
import qualified Data.ByteString          as BS
import           Data.Map.Strict          ((!))
import qualified Data.Map.Strict          as SMap
import           Data.Maybe               (isJust)
import           System.Directory         (createDirectoryIfMissing,
                                           doesDirectoryExist, doesFileExist,
                                           getModificationTime, listDirectory,
                                           removeDirectoryRecursive, removeFile,
                                           setModificationTime, setPermissions,
                                           writable)
import           System.FilePath.Posix    (dropTrailingPathSeparator,
                                           splitExtension, splitFileName, (</>))

import           Structure.FileSystemType (Dir (..), DirProperty (..),
                                           File (..), FileProperty (..),
                                           FileSysRoot (..))

-- | Writes changes from the file manager to the file system.
-- Errors do not stop the download, but are logged so that the user
-- knows what went wrong and can fix something by hand.
writeNewFileSystem :: FileSysRoot -> IO ()
writeNewFileSystem (FileSysRoot path dir) = writeDir path dir

-- | Write directory changes to the file system.
writeDir :: FilePath -> Dir -> IO ()
writeDir path dir = do
  let dirPath          = (dropTrailingPathSeparator path)
  dirContent <- safeListDirectory dirPath
  updateDirIfPossible dirPath dir dirContent
  forM_ (SMap.toList (dirDirs dir)) (\(name, subDir) -> writeDir (path </> name) subDir)

-- | Write file changes to the file system. Checks that writing is allowed.
uploadFile :: FilePath -> File -> IO ()
uploadFile path file | writable $ filePermission $ fileProperty file = do
  BS.writeFile path (fileData file)
  setModificationTime path (fileModTime $ fileProperty file)
  setPermissions path (filePermission $ fileProperty file)
uploadFile _ _ = return ()

-- | Takes a possible list of directory contents and writes it.
updateDirIfPossible :: FilePath -> Dir -> Maybe [FilePath] -> IO ()
updateDirIfPossible _    _   Nothing     = return ()
updateDirIfPossible path dir (Just cont) = do
  contDirs  <- filterM (\name -> doesDirectoryExist $ path </> name) cont
  contFiles <- filterM (\name -> doesFileExist      $ path </> name) cont

  let myDirs      = dirDirs  dir
  let myFiles     = dirFiles dir
  let rmDirNames  = filter (\name -> SMap.notMember name myDirs)  contDirs
  let rmFileNames = filter (\name -> SMap.notMember name myFiles) contFiles
  let mkDirNames  = SMap.keys myDirs
  let mkFileNames = SMap.keys myFiles

  forM_ rmDirNames  (\name -> safeRemoveDir  $ path </> name)
  forM_ rmFileNames (\name -> safeRemoveFile $ path </> name)
  forM_ mkDirNames  (\name -> safeCreateDir  $ path </> name)
  forM_ mkFileNames (\name -> safeCreateFile  (path </> name) (myFiles ! name))

-- | Creates a directory and parents if it doesn't exist.
-- Logs an error if something went wrong.
safeCreateDir :: FilePath -> IO ()
safeCreateDir path =
  let msg = "Unabel to create dir: " ++ path
   in createDirectoryIfMissing True path `catch` logHandler msg

-- | Creates a file if it did not exist and writes an updated
-- one if it has not been changed after changes in the file manager.
-- Logs an error if something went wrong.
safeCreateFile :: FilePath -> File -> IO ()
safeCreateFile path file = do
  isExist    <- doesFileExist path
  isModified <- checkModified path file `catch` (\(ignored :: SomeException) -> return False)
  if isExist && isModified
  then
    putStrLn $ "The file was changed after actions in the application,\
      \the current changes will not be applied. File: " ++ path
  else
    uploadFile path file `catch` \e ->
      logHandler ("Unable to write file: " ++ path ++ ". With info: " ++ show e) e

  where
    msg = "ModTime won't be checked for " ++ path

    checkModified :: FilePath -> File -> IO Bool
    checkModified path file = do
      modTime <- getModificationTime path
      return $ (fileModTime $ fileProperty file) < modTime

-- | Deletes a folder and all its contents recursively.
-- Logs an error if something went wrong.
safeRemoveDir :: FilePath -> IO ()
safeRemoveDir path =
  let msg = "Unable to remove dir: " ++ path
   in removeDirectoryRecursive path `catch` logHandler msg

-- | Deletes a file. Logs an error if something went wrong.
safeRemoveFile :: FilePath -> IO ()
safeRemoveFile path =
  let msg = "Unable to remove file: " ++ path
   in removeFile path `catch` logHandler msg

-- | Tries to get the contents of the directory. And returns the result
-- inside 'Maybe'. Logs an error if something went wrong.
safeListDirectory :: FilePath -> IO (Maybe [FilePath])
safeListDirectory path = (Just <$> (listDirectory path)) `catch` handler
  where
    msg     = "Unable to get dir content. Changes would be ignored."

    handler :: SomeException -> IO (Maybe [FilePath])
    handler ex = logHandler msg ex >> return Nothing

-- | Accepts any 'IO' error and prints it to the console with a custom message.
logHandler :: String -> SomeException -> IO ()
logHandler msg e = putStrLn $ msg ++ "\n System message: " ++ (show e)
