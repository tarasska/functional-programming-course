module Structure.ControlType
  ( -- * Control types
    -- Types are used to represent custom manager control requests
    Cmd(..)
  , RootPath(..)
  ) where

-- | Holder for initial path argument.
data RootPath = RootPath
  { getPath :: String  -- ^ root path of file manager
  }

-- | Types representing commands for the file manager.
data Cmd
  = Cat FilePath               -- ^ Display contents of a file by path
  | Cd FilePath                -- ^ Moving to a directory at a given path
  | CreateFolder FilePath      -- ^ Create directory by name in current location
  | CreateFile FilePath        -- ^ Create directory by name in current location
  | DirCmd                     -- ^ Show directory content
  | FindFile FilePath          -- ^ Recursively find file by provided name
  | Information FilePath       -- ^ Show information about file or dir by path
  | Ls FilePath                -- ^ Show dir content by path
  | Quit                       -- ^ Load file system and exit
  | Remove FilePath            -- ^ Remove file or dir by path
  | Tree FilePath              -- ^ Show file system structure in tree view
  | WriteFile FilePath String  -- ^ Write text to file by path
