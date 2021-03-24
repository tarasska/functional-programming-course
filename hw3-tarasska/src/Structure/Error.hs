{-# LANGUAGE FlexibleInstances #-}

module Structure.Error
  ( -- * Classes
    CustomPrintableEither
    -- * Types
  , ManagerException(..)
    -- * Functions
  , exceptionHandler
  ) where

import qualified Data.ByteString.Char8 as BS8

-- | Type represents exception, which would be occurred during execution.
data ManagerException
  -- | For handling unexpected parameters
  = IncorrectUsage String
  -- | If the path rises above the root
  | OutOfRootBound
  -- | If path not exist
  | PathNotExist FilePath
  -- | If directory not exist
  | NoSuchDirectory FilePath
  -- | If file not exist
  | NoSuchFile FilePath
  -- | If actions contradict the existence of the directory
  | DirExist FilePath
  -- | If actions contradict the existence of the file
  | FileExist FilePath
  -- | If it is impossible to perform actions due to their prohibition
  | PermissionDenied
  deriving (Eq)

-- | 'Show' instance for readable error representation
instance Show ManagerException where
  show (PathNotExist path)    = "Path not exist: " ++ path
  show (NoSuchDirectory path) = "Directory not found: " ++ path
  show (NoSuchFile path)      = "Directory not found: " ++ path
  show (DirExist path)        = "Directory already exist: " ++ path
  show (FileExist path)       = "File already exist: " ++ path
  show PermissionDenied       = "Permisson denied"
  show OutOfRootBound         = "Path upper then root dir is prohibited"
  show (IncorrectUsage msg)   = "Incorrect usage: " ++ msg

-- | Class for printing 'Either' which is the key essence of this application.
-- Avoids copy-paste and functions with different names, but the same meaning.
class Show a => CustomPrintableEither a where
  exceptionHandler :: Either ManagerException a -> IO ()
  exceptionHandler (Left ex)   = putStrLn $ show ex
  exceptionHandler (Right res) = putStrLn $ show res

-- | This instance is used to display messages created inside the program.
instance CustomPrintableEither [Char] where
  exceptionHandler (Left ex)   = putStrLn $ show ex
  exceptionHandler (Right res) = putStrLn $ res

-- | This instance is used to display the contents of files.
instance CustomPrintableEither BS8.ByteString where
  exceptionHandler (Left ex)   = putStrLn $ show ex
  exceptionHandler (Right res) = BS8.putStrLn res

-- | This instance is used to display errors when the return value is always empty.
instance CustomPrintableEither () where
  exceptionHandler (Left ex)   = putStrLn $ show ex
  exceptionHandler (Right res) = return ()
