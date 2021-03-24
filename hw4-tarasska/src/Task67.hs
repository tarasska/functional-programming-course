{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Task67
  ( -- * Types
    FS(..)
    -- * Functions
  , cd
  , contents
  , dirContents
  , dirContList
  , dirName
  , file
  , fileName
  , ls
  , name
  , scanFileSystem
  ) where

import Control.Exception (throwIO)
import Control.Monad (mapM)
import Lens.Micro (Lens', Traversal', filtered, lens, traversed, (^.))
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (FilePath)
import System.FilePath.Posix (takeFileName, (</>))

-- | A type to display a file system object (file or dir).
data FS
  = Dir
    { _name     :: FilePath  -- ^ Dir name, not absolute path
    , _contents :: [FS]      -- ^ Dir contents (files or dirs)
    }
  | File
    { _name     :: FilePath  -- ^ File name, not absolute path
    }
  deriving (Eq, Show)

-- | Name lens.
name :: Lens' FS FilePath
name = lens getter setter
  where
    getter = \fs -> _name fs
    setter = \fs newName -> fs{ _name = newName }

-- | Contents lens.
contents :: Lens' FS [FS]
contents = lens getter setter
  where
    getter = \fs -> _contents fs
    setter = \fs newContent -> fs{ _contents = newContent }

-- | File name traversal.
fileName :: Traversal' FS FilePath
fileName f (File fName) = File <$> (f fName)
fileName _ dirObj       = pure dirObj

-- | Directory name traversal.
dirName :: Traversal' FS FilePath
dirName f dir@Dir{ _name = dName } =
  let renamer = \newName -> dir{ _name = newName}
   in renamer <$> (f dName)
dirName _ fsFile = pure fsFile

-- | Traversal for directory contents (as a list).
dirContList :: Traversal' FS [FS]
dirContList f dir@Dir { _contents = cont } =
  let updater = \updCont -> dir{ _contents = updCont }
   in updater <$> (f cont)
dirContList _ fsFile = pure fsFile

-- | Traversal for directory contents (for each element).
dirContents :: Traversal' FS FS
dirContents f dir@Dir { _contents = cont } =
  let updater = \updCont -> dir{ _contents = updCont }
   in updater <$> (traverse f cont)
dirContents _ fsFile = pure fsFile

-- | Scans the filesystem at the given path. And builds a 'FS'-tree.
scanFileSystem :: FilePath -> IO FS
scanFileSystem initPath = scanElement initPath
  where
    scanElement :: FilePath -> IO FS
    scanElement path = do
      isDir <- doesDirectoryExist path
      if isDir
      then do
        subElems <- listDirectory path
        dContent <- mapM (\eName -> scanElement (path </> eName)) subElems
        return $ Dir (takeFileName path) dContent
      else do
        isFile <- doesFileExist path
        if isFile
        then return $ File (takeFileName path)
        else throwIO $ userError ("No such file or directory: " ++ path)

-- | Cd for 'FS' implemented through traversal.
cd :: FilePath -> Traversal' FS FS
cd targetName = dirContents.(filtered $ isTargetDir targetName)
  where
    isTargetDir :: FilePath -> FS -> Bool
    isTargetDir dName fs = (fs ^. dirName) == dName

-- | Ls for 'FS' implemented through traversal.
ls :: Traversal' FS FilePath
ls = dirContList.traversed.name

-- | Gets the name of the file if it exists.
file :: FilePath -> Traversal' FS FilePath
file targetName
  = trDir.dirContList.traversed.trFile.(filtered $ isTargetFile targetName).name
  where
    trFile f = \case
      file@(File name) -> f file
      dir              -> pure dir

    trDir f = \case
      dir@(Dir _ _) -> f dir
      file          -> pure file

    isTargetFile :: FilePath -> FS -> Bool
    isTargetFile fName fs = (fs ^. fileName) == fName
