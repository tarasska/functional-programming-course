module Util.FileManagerUtils
  ( -- * Functions
    -- | Useful functions for file system abstraction operations
    buildPath
  , cutPath
  , createEmptyDir
  , createEmptyFile
  , findDir
  , findMaybeDir
  , findFilesPathByName
  , isDirExist
  , isSysName
  , pathTailDecompose
  , pathToNames
  , readFileOrThrow
  , rewriteFileOrThrow
  , showDir
  , updateDirInFileSys
  ) where

import           Control.Monad.Trans.Except      (throwE)
import qualified Data.ByteString                 as BS
import           Data.List                       (intercalate)
import qualified Data.Map.Strict                 as SMap
import qualified Data.Time.Clock                 as Time
import           System.Directory.Internal       (Permissions (..))
import           System.FilePath.Posix           ((</>))
import           System.FilePath.Posix           (dropTrailingPathSeparator,
                                                  isAbsolute, joinPath,
                                                  splitPath, (</>))

import           RealFileSystem.FileSystemReader (collectExtension,
                                                  updateDirSize)
import           Structure.Error                 (ManagerException (..))
import           Structure.FileSystemType        (Dir (..), DirProperty (..),
                                                  File (..), FileProperty (..),
                                                  FileSysRoot (..), SafeState)

-- | If the path starts with '/' then it is interpreted as an absolute path
-- relative to the root of the file manager. Otherwise, it is considered
-- relative to the current position.
buildPath :: FilePath -> FilePath -> FilePath
buildPath curPath argPath = case isAbsolute argPath of
  True  -> argPath
  False -> curPath </> argPath

-- | Takes the path as a list of names. Throws out useless names like ".".
-- Delete directories that are symmetric to "..". And it calculates the balance
-- of the ascent upwards along "..", to check the root out of bounds.
cutPath :: [FilePath] -> (Int, [FilePath])
cutPath []   = (0, [])
cutPath (p : ps)
  | p == "."  = cutPath ps
  | p == ".." = let (ddot, parts) = cutPath ps in (ddot + 1, parts)
  | otherwise = case (cutPath ps) of
    (0   , parts) -> (0       , p : parts)
    (ddot, parts) -> (ddot - 1, parts)

-- | Takes in a path and returns a list of names for that path.
-- Throws 'OutOfRootBound' if the path rises above the root.
pathToNames :: FilePath -> SafeState [FilePath]
pathToNames path = if ddots > 0 then throwE OutOfRootBound else return remParts
  where
    parts = case (splitPath path) of
      ps@(x : xs) -> if x == "/" then xs else ps
      ps          -> ps
    (ddots, remParts) = cutPath $ map dropTrailingPathSeparator parts

-- * CorrectPath
--
-- $corrPath
-- Correct path is a path or list of name, which represents the path, without
-- ".", "..". For name list "/" also forbidden $corrPath.

-- | Checks if a directory exists at the given $corrPath.
isDirExist :: [FilePath] -> FileSysRoot -> Bool
isDirExist paths fs = case (findMaybeDir paths fs) of
  Just _  -> True
  Nothing -> False

-- | Takes the $corrPath, the root of the manager and tries to find a directory
-- along the given path.
findMaybeDir :: [FilePath] -> FileSysRoot -> Maybe Dir
findMaybeDir paths fs = findMaybeDir' paths (rootDir fs)
  where
    findMaybeDir' :: [FilePath] -> Dir -> Maybe Dir
    findMaybeDir' []          dir = Just dir
    findMaybeDir' (name : ps) dir = do
      subDir <- SMap.lookup name (dirDirs dir)
      findMaybeDir' ps subDir

-- | Wrapper for the 'findMaybeDir' function.
-- Throws 'NoSuchDirectory' if the directory is not found.
findDir :: [FilePath] -> FileSysRoot -> SafeState Dir
findDir []    fs = return $ rootDir fs
findDir paths fs = case (findMaybeDir paths fs) of
  Just dir -> return dir
  Nothing  -> throwE $ NoSuchDirectory (joinPath paths)

-- | Collects a list of directories and files in the current directory for display.
showDir :: Dir -> String
showDir dir = intercalate "\n" [dirs, files]
  where
    dirs  = intercalate "\n" (map fst (SMap.toAscList (dirDirs  dir)))
    files = intercalate "\n" (map fst (SMap.toAscList (dirFiles dir)))

-- | Creates empty directory with provided name and default permissions.
createEmptyDir :: FilePath -> FilePath -> Dir
createEmptyDir path name = Dir SMap.empty SMap.empty name prop
  where
    prop = DirProperty 0 (path </> name) (Permissions True True False True) 0

-- | Creates empty file with provided name, creation time and default permissions.
createEmptyFile :: FilePath -> FilePath -> Time.UTCTime -> File
createEmptyFile path name time = File BS.empty name prop
  where
    filePath = path </> name
    prop = FileProperty
      { fileType        = collectExtension name
      , fileModTime     = time
      , filePath        = filePath
      , filePermission  = Permissions True True False True
      , fileSizeInBytes = 0
      }

-- | Includes new or updated directories in the file system along its path.
-- Throws 'PathNotExist' if some name on the path does not exist.
updateDirInFileSys :: Dir -> FilePath -> FileSysRoot -> SafeState FileSysRoot
updateDirInFileSys newDir path fs = pathToNames path >>= \splitedPath ->
  updateDir' splitedPath (rootDir fs) >>= \root -> return fs{ rootDir = root }
  where
    updateDir' :: [FilePath] -> Dir -> SafeState Dir
    updateDir' []             curDir = return $ updateDirSize newDir
    updateDir' (curName : ns) curDir =
      case (SMap.lookup curName (dirDirs curDir)) of
        Just subDir -> do
          upDir <- updateDir' ns subDir
          return $ updateDirSize curDir{ dirDirs = SMap.insert curName upDir (dirDirs curDir) }
        Nothing     -> throwE $ PathNotExist curName

-- | Retrieves data from a file.
-- Throws 'PermissionDenied' if reading the file is prohibited.
readFileOrThrow :: File -> SafeState BS.ByteString
readFileOrThrow file =
  if readable (filePermission $ fileProperty file)
  then return $ fileData file
  else throwE $ PermissionDenied

-- | Rewrites file data.
-- Throws 'PermissionDenied' if writing the file is prohibited.
rewriteFileOrThrow :: File -> BS.ByteString -> Time.UTCTime -> SafeState File
rewriteFileOrThrow file text modTime = let prop = fileProperty file in
  if writable (filePermission prop)
  then return file{ fileData = text, fileProperty = prop{ fileModTime = modTime } }
  else throwE PermissionDenied

-- | Finds absolute files path in real file system by name.
findFilesPathByName :: FilePath -> Dir -> [FilePath]
findFilesPathByName name dir = (concat subFiles) ++ file
  where
    subFiles = map (\subDir -> findFilesPathByName name subDir) (SMap.elems $ dirDirs dir)
    file     =
      case (SMap.lookup name (dirFiles dir)) of
        Just f  -> [filePath $ fileProperty f]
        Nothing -> []

-- | Checks that the name is "." or "..".
-- Used to emulate the existence of these directories.
isSysName :: FilePath -> Bool
isSysName name | name == "." || name == ".." = True
isSysName _    = False

-- | Separates a path into a list of names and a last name.
pathTailDecompose :: FilePath -> SafeState ([FilePath], FilePath)
pathTailDecompose ""   = return ([], "")
pathTailDecompose path = do
  splitedPath <- pathToNames path
  return (init splitedPath, last splitedPath)
