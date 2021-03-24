module RealFileSystem.FileSystemReader
  ( -- * Functions
    collectExtension
  , readFileSystem
  , updateDirSize
  ) where

import           Control.Monad            (filterM)
import qualified Data.ByteString          as BS
import qualified Data.Map.Strict          as SMap
import           System.Directory         (doesDirectoryExist, doesFileExist,
                                           getFileSize, getModificationTime,
                                           getPermissions, listDirectory,
                                           makeAbsolute, pathIsSymbolicLink)
import           System.FilePath.Posix    (dropTrailingPathSeparator,
                                           splitExtension, splitFileName, (</>))

import           Structure.FileSystemType (Dir (..), DirProperty (..),
                                           File (..), FileProperty (..),
                                           FileSysRoot (..))

-- | Takes a path to a directory and loads the entire file system into RAM,
-- rooted in that directory. It does not catch errors in the 'IO',
-- because I think that it is not worth starting the manager,
-- if an error occurs during the loading process, their processing is carried
-- out by the one who calls this function.
readFileSystem :: FilePath -> IO FileSysRoot
readFileSystem rootPath = do
  absRootPath <- makeAbsolute rootPath
  dir         <- readDir rootPath
  return $ FileSysRoot rootPath dir

-- | Takes a path to a folder and a list of directory names in it.
-- As a result, it returns a list of pairs from name and directory.
processDirs :: FilePath -> [FilePath] -> IO [(String, Dir)]
processDirs path []          = return []
processDirs path (name : ns) = do
  dir  <- readDir (path </> name)
  dirs <- processDirs path ns
  return $ (name, dir) : dirs

-- | Reads directory by the current path.
readDir :: FilePath -> IO Dir
readDir path = do
  let (prefPath, name) = splitFileName (dropTrailingPathSeparator path)
  content   <- listDirectory path >>= \ct -> filterM (\x -> notSymbolLink (path </> x)) ct
  dirProp   <- readDirProperty path (length content)
  fileNames <- (filterM (\f -> doesFileExist (path </> f)) content)
  files     <- processFiles path fileNames
  dirNames  <- (filterM (\d -> doesDirectoryExist (path </> d)) content)
  dirs      <- processDirs path dirNames
  let dir   = Dir (SMap.fromList dirs) (SMap.fromList files) name dirProp
  return $ updateDirSize dir

-- | Reads directory property by the path.
readDirProperty :: FilePath -> Int -> IO DirProperty
readDirProperty path itemsCnt = do
  permissons <- getPermissions path
  dirSize    <- getFileSize path
  return $ DirProperty (toInteger itemsCnt) path permissons dirSize

-- | Takes a path to a folder and a list of files names in it.
-- As a result, it returns a list of pairs from name and file.
processFiles :: FilePath -> [FilePath] -> IO [(String, File)]
processFiles path []          = return []
processFiles path (name : ns) = do
  file <- loadFile path name
  fs   <- processFiles path ns
  return $ (name, file) : fs

-- | Loads file by the path to the parent folder and name.
loadFile :: FilePath -> FilePath-> IO File
loadFile prefPath name = do
  let path = prefPath </> name
  fileProperty <- readFileProperty path
  fileData     <- BS.readFile path
  return $ File fileData name fileProperty

-- | Reads file property by the path.
readFileProperty :: FilePath -> IO FileProperty
readFileProperty path = do
  let fileType = collectExtension path
  time        <- getModificationTime path
  permissions <- getPermissions path
  size        <- getFileSize path
  return $ FileProperty fileType time path permissions size

-- | Takes name and extract an extension from it.
collectExtension :: FilePath -> [String]
collectExtension path = if ext == "" then [] else ext : (collectExtension remPath)
  where
    (remPath, ext) = splitExtension path

-- | Calculates directory size as the sum of subdirectories and files.
updateDirSize :: Dir -> Dir
updateDirSize dir@Dir{ dirDirs = subDirs, dirFiles = files, dirProperty = prop }
  = dir{ dirProperty = prop{ dirSizeInBytes = sz } }
  where
    subDirsSize = sum $ map (dirSizeInBytes  . dirProperty)  (SMap.elems subDirs)
    filesSize   = sum $ map (fileSizeInBytes . fileProperty) (SMap.elems files)
    sz = subDirsSize + filesSize

-- | Checks that the path is not a symbolic link
notSymbolLink :: FilePath -> IO Bool
notSymbolLink path = pathIsSymbolicLink path >>= return . not
