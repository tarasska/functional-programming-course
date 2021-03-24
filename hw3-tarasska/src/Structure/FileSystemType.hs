module Structure.FileSystemType
  ( -- * Types
    -- | Types for working with an isolated file system
    Dir(..)
  , DirProperty(..)
  , File(..)
  , FileProperty(..)
  , FileSysRoot(..)
  , ManagerState(..)
  , SafeState(..)
  ) where

import           Control.Monad.State        (State)
import           Control.Monad.Trans.Except (ExceptT)
import qualified Data.ByteString            as BS
import           Data.List                  (intercalate)
import qualified Data.Map.Strict            as SMap
import qualified Data.Time.Clock            as Time
import qualified System.Directory           as SysDir

import           Structure.Error            (ManagerException)

-- | Directory storage type.
data Dir = Dir
  { dirDirs     :: SMap.Map String Dir   -- ^ Subdirectories
  , dirFiles    :: SMap.Map String File  -- ^ Files
  , dirName     :: String                -- ^ Directory name
  , dirProperty :: DirProperty           -- ^ Different properties
  }

-- | Directory property type.
data DirProperty = DirProperty
  { dirItemCount   :: Integer             -- ^ The number of directories and files
  , dirPath        :: FilePath            -- ^ Path in real file system
  , dirPermission  :: SysDir.Permissions  -- ^ Permissions
  , dirSizeInBytes :: Integer             -- ^ Size in bytes (subdirectories sizes included)
  }

-- | Type represents file.
data File = File
  { fileData     :: BS.ByteString  -- ^ File content as a sequence of bytes
  , fileName     :: String         -- ^ File name
  , fileProperty :: FileProperty   -- ^ Different properties
  }

-- | File property type.
data FileProperty = FileProperty
  { fileType        :: [String]            -- ^ File types or extensions
  , fileModTime     :: Time.UTCTime        -- ^ Modification time in UTC
  , filePath        :: FilePath            -- ^ Path in real file system
  , filePermission  :: SysDir.Permissions  -- ^ Permissions
  , fileSizeInBytes :: Integer             -- ^ Size in bytes
  }

-- | The root of the application file system.
-- Stores the current file system and the path to its root in the real world.
data FileSysRoot = FileSysRoot
  { rootPath :: FilePath  -- ^ Path to root of application FS in the real world
  , rootDir  :: Dir       -- ^ Root directory with current application data
  }

-- | Main state used in application. Stores 'FileSysRoot' and path to the
-- current location in manager file system.
data ManagerState = ManagerState
  { fsRoot  :: FileSysRoot  -- ^ current state of manager file system
  , curPath :: FilePath     -- ^ current location
  }

-- | A type representing the state of our manager with the ability
-- to use custom exceptions.
-- I hope this is the correct order of combination, as indicated in the lectures.
type SafeState a = ExceptT ManagerException (State ManagerState) a



-- | 'Eq' instances
-- The next block of code defines the instances to compare
-- most of the entered entities. This is mainly used in tests.

instance Eq FileProperty where
  (FileProperty typeL timeL pathL permL sizeL) == (FileProperty typeR timeR pathR permR sizeR)
    =  typeL == typeR
    && timeL == timeR
    && pathL == pathR
    && permL == permR
    && sizeL == sizeR

instance Eq File where
  (File dataL nameL propL) == (File dataR nameR propR)
    =  dataL == dataR
    && nameL == nameR
    && propL == propR

instance Eq DirProperty where
  (DirProperty cntL pathL permL sizeL) == (DirProperty cntR pathR permR sizeR)
    =  cntL  == cntR
    && pathL == pathR
    && permL == permR
    && sizeL == sizeR

instance Eq Dir where
  (Dir dirsL filesL nameL propL) == (Dir dirsR filesR nameR propR)
    =  dirsL  == dirsR
    && filesL == filesR
    && nameL  == nameR
    && propL  == propR

instance Eq FileSysRoot where
  (FileSysRoot pathL dirL) == (FileSysRoot pathR dirR) = (pathL == pathR) && (dirL == dirR)

instance Eq ManagerState where
  (ManagerState fsL pathL) == (ManagerState fsR pathR) = (fsL == fsR) && (pathL == pathR)

-- | Readable 'ManagerState' display
instance Show ManagerState where
  show (ManagerState fsRoot path) = intercalate "\n"
    [ "State path: " ++ (show path)
    , show fsRoot
    ]

-- | Readable 'FileSysRoot' display
instance Show FileSysRoot where
  show (FileSysRoot path dir) = intercalate "\n"
    [ "Root: " ++ (show path)
    , show dir
    ]

-- | Shows directory properties with formatting for console output.
instance Show DirProperty where
  show (DirProperty items path perm sz) = intercalate "\n"
    [ "- Items:           " ++ (show items)
    , "- Path in real FS: " ++ path
    , "- Permisssions:    " ++ (show perm)
    , "- Size in bytes:   " ++ (show sz)
    ]

-- | Shows directory information with formatting for console output.
instance Show Dir where
  show (Dir _ _ name prop) = intercalate "\n"
    [ "Directory name: " ++ name
    , "Properties:\n"    ++ (show prop)
    ]

-- | Shows file properties with formatting for console output.
instance Show FileProperty where
  show (FileProperty ftype modTime path perm sz) = intercalate "\n"
    [ "- Type:              " ++ (intercalate ", " ftype)
    , "- Modification time: " ++ (show modTime)
    , "- Path in real FS:   " ++ path
    , "- Permisssions:      " ++ (show perm)
    , "- Size in bytes:     " ++ (show sz)
    ]

-- | Shows file information with formatting for console output.
instance Show File where
  show (File _ name prop) = intercalate "\n"
    [ "File name:  "  ++ name
    , "Properties:\n" ++ (show prop)
    ]
