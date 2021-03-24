module FileManager
  ( -- * Functions
    -- | All operations provided by the file manager
    execCat
  , execCd
  , execCreateFile
  , execCreateFolder
  , execFindFile
  , execInformation
  , execLs
  , execRemove
  , execTree
  , execWriteFile
  ) where

import           Control.Monad.State        (get, put)
import           Control.Monad.Trans.Except (throwE)
import qualified Data.ByteString            as BS
import           Data.Char                  (chr)
import           Data.List                  (intercalate)
import           Data.Map.Strict            ((!))
import qualified Data.Map.Strict            as SMap
import qualified Data.Time.Clock            as Time
import           System.Directory.Internal  (Permissions (..))
import           System.FilePath.Posix      (dropTrailingPathSeparator,
                                             joinPath, takeDirectory,
                                             takeFileName, (</>))


import           Structure.Error            (ManagerException (..))
import           Structure.FileSystemType   (Dir (..), DirProperty (..),
                                             File (..), FileProperty (..),
                                             FileSysRoot (..),
                                             ManagerState (..), SafeState (..))
import           Util.FileManagerUtils      (buildPath, createEmptyDir,
                                             createEmptyFile, cutPath, findDir,
                                             findFilesPathByName, findMaybeDir,
                                             isDirExist, isSysName,
                                             pathTailDecompose, pathToNames,
                                             readFileOrThrow,
                                             rewriteFileOrThrow, showDir,
                                             updateDirInFileSys)

-- | Moves the current position along the specified path.
-- Throws 'NoSuchDirectory' if the desired directory or her parent doesn't exist.
execCd :: FilePath -> SafeState ()
execCd argPath = do
  managerSt@ManagerState{ fsRoot = fsr, curPath = stPath } <- get
  splitedPath <- pathToNames $ buildPath stPath argPath
  if isDirExist splitedPath fsr
  then put managerSt{curPath = joinPath splitedPath}
  else throwE (NoSuchDirectory argPath)

-- | Collects the provided directory content into a 'String'.
-- Throws 'NoSuchDirectory' if the desired directory doesn't exist.
execLs :: FilePath -> SafeState String
execLs argPath = do
  managerSt@ManagerState{ fsRoot = fsr, curPath = stPath } <- get
  splitedPath <- pathToNames $ buildPath stPath argPath
  lsDir       <- findDir splitedPath fsr
  return $ showDir lsDir

-- | Extracts file data by the path.
-- Throws 'NoSuchDirectory' if the file parent directory doesn't exist.
-- Throws 'NoSuchFile' if the file doesn't exist.
-- Throws 'PermissionDenied' if file not readable.
execCat :: FilePath -> SafeState BS.ByteString
execCat filePath = do
  managerSt@ManagerState{ fsRoot = fsr, curPath = stPath } <- get
  splitedPath <- pathToNames $ buildPath stPath (takeDirectory filePath)
  dir         <- findDir splitedPath fsr
  case SMap.lookup (takeFileName filePath) (dirFiles dir) of
    Just file -> readFileOrThrow file
    Nothing   -> throwE $ NoSuchFile filePath

-- | Creates directory by the name in the current location.
-- Throws 'DirExist' if directory with provided name already exist in current location.
execCreateFolder :: FilePath -> SafeState ()
execCreateFolder name = do
  if isSysName name then throwE $ DirExist name else return ()
  managerSt@ManagerState{ fsRoot = fsr, curPath = stPath } <- get
  let path = stPath </> name
  dir <- pathToNames stPath >>= \splitedPath -> findDir splitedPath fsr
  if SMap.member name (dirDirs dir)
  then throwE $ DirExist path
  else do
    let realPath = (rootPath fsr) </> stPath
    let newDir = dir{ dirDirs = SMap.insert name (createEmptyDir realPath name) (dirDirs dir) }
    newRoot <- updateDirInFileSys newDir stPath fsr
    put managerSt{ fsRoot = newRoot }

-- | Creates file by the name in the current location.
-- Throws 'FileExist' if file with provided name already exist in current location.
execCreateFile :: FilePath -> Time.UTCTime -> SafeState ()
execCreateFile name time = do
  if isSysName name then throwE $ DirExist name else return ()
  managerSt@ManagerState{ fsRoot = fsr, curPath = stPath } <- get
  let path = stPath </> name
  dir <- pathToNames stPath >>= \splitedPath -> findDir splitedPath fsr
  if SMap.member name (dirFiles dir)
  then throwE $ FileExist path
  else do
    let realPath = (rootPath fsr) </> stPath
    let newDir = dir{
      dirFiles = SMap.insert name (createEmptyFile realPath name time) (dirFiles dir)
    }
    newRoot <- updateDirInFileSys newDir stPath fsr
    put managerSt{ fsRoot = newRoot }

-- | Removes file or directory by their path.
-- The directory is deleted with all its contents.
-- Throws 'PermissionDenied' if provided directory is root.
-- Throws 'NoSuchDirectory' if parent directory of removing entity not exist.
-- Throws 'PathNotExist' if provided path not exist.
execRemove :: FilePath -> SafeState ()
execRemove rmPath = do
  managerSt@ManagerState{ fsRoot = fsr, curPath = stPath } <- get
  normPath <- (pathToNames $ buildPath stPath rmPath) >>= return . joinPath
  (splitedPath, name) <- pathTailDecompose normPath
  rmRootCheck splitedPath name
  dir <- findDir splitedPath fsr
  if SMap.member name (dirFiles dir)
  then do
    let newDir = dir{ dirFiles = SMap.delete name (dirFiles dir) }
    newRoot <- updateDirInFileSys newDir (joinPath splitedPath) fsr
    put managerSt{ fsRoot = newRoot }
  else if SMap.member name (dirDirs dir)
  then do
    let newDir = dir{ dirDirs = SMap.delete name (dirDirs dir) }
    newRoot <- updateDirInFileSys newDir (joinPath splitedPath) fsr
    put managerSt{ fsRoot = newRoot }
  else throwE $ PathNotExist rmPath

  where
    rmRootCheck :: [FilePath] -> FilePath -> SafeState ()
    rmRootCheck [] "" = throwE PermissionDenied
    rmRootCheck _  _  = return ()

-- | Collects file or directory information by their path.
-- Throws 'NoSuchDirectory' if parent directory of entity not exist.
-- Throws 'PathNotExist' if provided path not exist.
execInformation :: FilePath -> SafeState String
execInformation infoPath = do
  managerSt@ManagerState{ fsRoot = fsr, curPath = stPath } <- get
  let path = dropTrailingPathSeparator infoPath
  normPath <- (pathToNames $ buildPath stPath path) >>= return . joinPath
  (splitedPath, name) <- pathTailDecompose normPath
  parentDir <- findDir splitedPath fsr
  if SMap.member name (dirFiles parentDir)
  then do
    let file = (dirFiles parentDir) ! name
    return $ show file
  else if SMap.member name (dirDirs parentDir)
  then do
    let dir = (dirDirs parentDir) ! name
    return $ show dir
  else if null name
  then return $ show parentDir
  else throwE $ PathNotExist infoPath

-- | Searches all files by name in the current directory and all subdirectories.
-- Returns the absolute paths to the found files, concatenated to a 'String'.
execFindFile :: FilePath -> SafeState String
execFindFile name = do
  managerSt@ManagerState{ fsRoot = fsr, curPath = stPath } <- get
  dir <- pathToNames stPath >>= \splitedPath -> findDir splitedPath fsr
  return $ intercalate "\n" $ findFilesPathByName name dir

-- | Overwrites the new information in the specified file
-- and setup the provided modification time.
-- Throws 'NoSuchDirectory' if parent directory of entity not exist.
-- Throws 'NoSuchFile' if provided file not exist.
-- Throws 'PermissionDenied' if file not writable.
execWriteFile :: FilePath -> BS.ByteString -> Time.UTCTime -> SafeState ()
execWriteFile filePath text modTime = do
  managerSt@ManagerState{ fsRoot = fsr, curPath = stPath } <- get
  let fileName = takeFileName filePath
  splitedPath <- pathToNames $ buildPath stPath (takeDirectory filePath)
  dir         <- findDir splitedPath fsr
  let files = dirFiles dir
  if SMap.member fileName files
  then do
    updatedFile <- rewriteFileOrThrow (files ! fileName) text modTime
    let newDir = dir{ dirFiles = SMap.insert fileName updatedFile files }
    newRoot <- updateDirInFileSys newDir (joinPath splitedPath) fsr
    put managerSt{ fsRoot = newRoot }
  else throwE $ NoSuchFile filePath

-- | Shows the filesystem tree starting from a passed directory.
-- Throws 'NoSuchDirectory' if provided directory not exist.
execTree :: FilePath -> SafeState String
execTree path = do
  managerSt@ManagerState{ fsRoot = fsr, curPath = stPath } <- get
  splitedPath <- pathToNames $ buildPath stPath path
  dir         <- findDir splitedPath fsr
  return $ showTree "" dir
  where
    horizTreeElem = chr 9500
    lastTreeElem  = chr 9492
    innerBlock    = [chr 9500, chr 9472, chr 9472, chr 9472]
    lastBlock     = [chr 9492, chr 9472, chr 9472, chr 9472]
    spaceBlock    = (chr 9474) : ("    ")
    fsign         = "[f]:"
    dsign         = "[d]:"

    showFilesTree :: String -> [String] -> String -> String
    showFilesTree pref []          _    = ""
    showFilesTree pref [name]      last = intercalate "" [pref, last, fsign, name, "\n"]
    showFilesTree pref (name : ns) last =
      intercalate "" [pref, innerBlock, fsign, name, "\n", showFilesTree pref ns last]

    showDirsTree :: String -> [(String, Dir)] -> String
    showDirsTree _    []                 = ""
    showDirsTree pref ((name, dir) : ns) = intercalate ""
      [ pref
      , if null ns then lastBlock else innerBlock
      , dsign
      , name
      , "\n"
      , let block = if null ns then tail spaceBlock else spaceBlock
         in showTree (pref ++ block) dir
      , showDirsTree pref ns
      ]

    showTree :: String -> Dir -> String
    showTree pref Dir{ dirDirs = subDirs, dirFiles = files, dirName = name } =
      if null subDirs
      then showFilesTree pref (SMap.keys files) lastBlock
      else intercalate ""
        [ showFilesTree pref (SMap.keys files) innerBlock
        , showDirsTree  pref (SMap.toList subDirs)
        ]
