module ManagerUnitTest
  ( cmdTestTree
  ) where

import           Control.Monad.State            (runState)
import           Control.Monad.Trans.Except     (runExceptT)
import qualified Data.ByteString                as BS
import qualified Data.ByteString.UTF8           as BS8
import           Data.Either                    (isRight)
import qualified Data.Map.Strict                as SMap
import qualified Data.Text                      as T
import           Data.Time.Calendar.OrdinalDate (fromOrdinalDate)
import           Data.Time.Clock                (UTCTime (..),
                                                 secondsToDiffTime)
import           System.Directory.Internal      (Permissions (..))
import           System.FilePath.Posix          ((</>))
import           Test.Tasty                     (TestTree)
import           Test.Tasty.Hspec               (Spec, describe, it, shouldBe,
                                                 testSpec)

import           FileManager                    (execCat, execCd,
                                                 execCreateFile,
                                                 execCreateFolder, execFindFile,
                                                 execInformation, execLs,
                                                 execRemove, execWriteFile)
import           Structure.Error                (ManagerException (..))
import           Structure.FileSystemType       (Dir (..), DirProperty (..),
                                                 File (..), FileProperty (..),
                                                 FileSysRoot (..),
                                                 ManagerState (..))

cmdTestTree :: IO TestTree
cmdTestTree = testSpec "File Manager command" cmdSpec

cmdSpec :: Spec
cmdSpec = do
  describe "cd" $ do
    it "one dir down" $ do
      let (res, newState) = runState (runExceptT $ execCd "dir1") staticSt
      emptyOk res >> (curPath newState `shouldBe` "dir1")
    it "two dir down" $ do
      let (res, newState) = runState (runExceptT $ execCd "dir2/dir22") staticSt
      emptyOk res >> (curPath newState `shouldBe` "dir2/dir22")
    it "Path start with '/' (from root ok)" $ do
      let (res, newState) = runState (runExceptT $ execCd "/dir2/dir22") staticSt
      emptyOk res >> (curPath newState `shouldBe` "dir2/dir22")
    it "Path start with '/' (subdirs - error)" $ do
      let (res1, newState1) = runState (runExceptT $ execCd "/dir2") staticSt
      emptyOk res1
      let (res2, _) = runState (runExceptT $ execCd "/dir22") newState1
      res2 `shouldBe` Left (NoSuchDirectory "/dir22")
    it "cur dir" $ do
      let (res, newState) = runState (runExceptT $ execCd ".") staticSt
      emptyOk res >> (curPath newState `shouldBe` "")
    it "long path with .." $ do
      let path            = "dir1/../dir2/dir22/../../dir1"
      let (res, newState) = runState (runExceptT $ execCd path) staticSt
      emptyOk res >> (curPath newState `shouldBe` "dir1")
    it "out of root error .." $ do
      let path     = "dir1/../dir2/dir22/../../../../dir1"
      let (res, _) = runState (runExceptT $ execCd path) staticSt
      res `shouldBe` Left OutOfRootBound
    it "cd <file_path> error" $ do
      let path     = "dir1/file1.txt"
      let (res, _) = runState (runExceptT $ execCd path) staticSt
      res `shouldBe` Left (NoSuchDirectory path)
    it "cd <not_exist_dir>" $ do
      let path     = "dir1/xaxa_error"
      let (res, _) = runState (runExceptT $ execCd path) staticSt
      res `shouldBe` Left (NoSuchDirectory path)
  describe "ls" $ do
    it "one dir down" $ do
      let (res, _) = runState (runExceptT $ execLs "dir1") staticSt
      shouldBeRight res
      strsInRes ["file1.txt"] (resToText res) `shouldBe` True
    it "two dir down" $ do
      let (res, _) = runState (runExceptT $ execLs "dir2/dir22") staticSt
      shouldBeRight res
      strsInRes ["my.bin"] (resToText res) `shouldBe` True
    it "cur dir" $ do
      let (res, _) = runState (runExceptT $ execLs ".") staticSt
      shouldBeRight res
      strsInRes ["dir1", "dir2", "notRead"] (resToText res) `shouldBe` True
    it "out of root error .." $ do
      let path     = "dir1/../dir2/dir22/../../../../dir1"
      let (res, _) = runState (runExceptT $ execLs path) staticSt
      res `shouldBe` Left OutOfRootBound
    it "ls <file_path> error" $ do
      let path     = "dir1/file1.txt"
      let (res, _) = runState (runExceptT $ execLs path) staticSt
      res `shouldBe` Left (NoSuchDirectory path)
    it "ls <not_exist_dir>" $ do
      let path     = "dir1/xaxa_error"
      let (res, _) = runState (runExceptT $ execLs path) staticSt
      res `shouldBe` Left (NoSuchDirectory path)
  describe "cat" $ do
    it "file1.txt" $ do
      let (res, _) = runState (runExceptT $ execCat "dir1/file1.txt") staticSt
      res `shouldBe` Right (BS8.fromString "File data")
    it "non-text data" $ do
      let (res, _) = runState (runExceptT $ execCat "dir2/dir22/my.bin") staticSt
      res `shouldBe` Right (BS.pack [0, 1, 2, 3])
    it "Permisson denied" $ do
      let (res, _) = runState (runExceptT $ execCat "notRead") staticSt
      res `shouldBe` Left PermissionDenied
    it "cat dir" $ do
      let (res, _) = runState (runExceptT $ execCat "dir1") staticSt
      res `shouldBe` Left (NoSuchFile "dir1")
    it "incorrect path" $ do
      let path     = "dir1/not_exist.txt"
      let (res, _) = runState (runExceptT $ execCat path) staticSt
      res `shouldBe` Left (NoSuchFile path)
  describe "create-folder" $ do
    it "in cur dir" $ do
      let name            = "newDir"
      let (res, newState) = runState (runExceptT $ execCreateFolder name) staticSt
      emptyOk res
      SMap.member name (dirDirs $ rootDir $ fsRoot newState) `shouldBe` True
    it "dir exist" $ do
      let name     = "dir2"
      let (res, _) = runState (runExceptT $ execCreateFolder name) staticSt
      res `shouldBe` Left (DirExist name)
  describe "create-file" $ do
    it "in cur dir" $ do
      let name            = "bubusiki"
      let (res, newState) = runState (runExceptT $ execCreateFile name defTime) staticSt
      emptyOk res
      SMap.member name (dirFiles $ rootDir $ fsRoot newState) `shouldBe` True
    it "file exist" $ do
      let name     = "notRead"
      let (res, _) = runState (runExceptT $ execCreateFile name defTime) staticSt
      res `shouldBe` Left (FileExist name)
  describe "remove" $ do
    it "rm dir in cur dir" $ do
      let name            = "dir1"
      let (res, newState) = runState (runExceptT $ execRemove name) staticSt
      emptyOk res
      SMap.member name (dirDirs $ rootDir $ fsRoot newState) `shouldBe` False
    it "rm file in cur dir" $ do
      let name            = "notRead"
      let (res, newState) = runState (runExceptT $ execRemove name) staticSt
      emptyOk res
      SMap.member name (dirFiles $ rootDir $ fsRoot newState) `shouldBe` False
    it "rm file in subdirs" $ do
      let path = "dir2/dir22/my.bin"
      let (res1, newState1) = runState (runExceptT $ execRemove path) staticSt
      emptyOk res1
      let (res2, _) = runState (runExceptT $ execCat path) newState1
      res2 `shouldBe` Left (NoSuchFile path)
    it "rm dir in subdirs" $ do
      let path              = "dir2/dir22"
      let (res1, newState1) = runState (runExceptT $ execRemove path) staticSt
      emptyOk res1
      let (res2, _) = runState (runExceptT $ execCd path) newState1
      res2 `shouldBe` Left (NoSuchDirectory path)
  describe "find-file" $ do
    it "Ex.1" $ do
      let (res, _) = runState (runExceptT $ execFindFile "file1.txt") staticSt
      res `shouldBe` Right "/home/tarasska/tmp/dir1/file1.txt"
    it "Ex.2" $ do
      let (res, _) = runState (runExceptT $ execFindFile "my.bin") staticSt
      res `shouldBe` Right "/home/tarasska/tmp/dir2/dir22/my.bin"
  describe "write-file" $ do
    it "Ok" $ do
      let path              = "/dir1/file1.txt"
      let text              = "You are breathtaking."
      let newData           = BS8.fromString text
      let (res1, newState1) = runState (runExceptT $ execWriteFile path newData defTime) staticSt
      emptyOk res1
      let (res2, _) = runState (runExceptT $ execCat path) newState1
      res2 `shouldBe` Right newData
    it "Permission error" $ do
      let path     = "/dir2/dir22/my.bin"
      let text     = "Error"
      let newData  = BS8.fromString text
      let (res, _) = runState (runExceptT $ execWriteFile path newData defTime) staticSt
      res `shouldBe` Left PermissionDenied
  describe "information" $ do
    it "File" $ do
      let path          = "/dir1/file1.txt"
      let fsPath        = "/home/tarasska/tmp/dir1/file1.txt"
      let partsExpected = ["file1.txt", "9", show $ Permissions True True False True, fsPath]
      let (res, _)      = runState (runExceptT $ execInformation path) staticSt
      shouldBeRight res
      strsInRes partsExpected (resToText res) `shouldBe` True
    it "Dir error" $ do
      let path          = "/dir2/dir22"
      let fsPath        = "/home/tarasska/tmp/dir2/dir22"
      let partsExpected = ["dir22", "1", show $ Permissions True True False True, fsPath]
      let (res, _)      = runState (runExceptT $ execInformation path) staticSt
      shouldBeRight res
      strsInRes partsExpected (resToText res) `shouldBe` True
    it "Not exist" $ do
      let path     = "/dir1/error"
      let (res, _) = runState (runExceptT $ execInformation path) staticSt
      res `shouldBe` Left (PathNotExist path)
  where
    emptyOk res       = res `shouldBe` (Right ())
    shouldBeRight res = isRight res `shouldBe` True

    resToText :: Either ManagerException String -> T.Text
    resToText (Right res) = T.pack res
    resToText (Left _)    = error "Unreachable statement. Use sholdBeRight before."

    strsInRes :: [String] -> T.Text -> Bool
    strsInRes []   _       = True
    strsInRes (s : ss) res =
      let isIn = T.pack s `T.isInfixOf` res
       in isIn && strsInRes ss res

-- test
--   dir1: file1.txt
--   dir2:
--     dir22:
--       my.bin
--   notRead
staticSt :: ManagerState
staticSt = ManagerState (FileSysRoot testRoot rootDirectory) ""
  where
    perm     = Permissions True True False True
    modeTime = defTime

    testRoot = "/home/tarasska/tmp"
    dir1Ph   = testRoot </> "dir1"
    dir2Ph   = testRoot </> "dir2"
    dir22Ph  = dir2Ph   </> "dir22"

    file1    = dir1Ph   </> "file1.txt"
    myBin    = dir22Ph  </> "my.bin"
    rdFile   = testRoot </> "notRead"

    fprop  = FileProperty [""] modeTime ""  perm 0

    file1Data = File
      { fileData     = BS8.fromString "File data"
      , fileName     = "file1.txt"
      , fileProperty = fprop { fileType = [".txt"], filePath = file1, fileSizeInBytes = 9 }
      }
    binData   = File
      { fileData     = BS.pack [0, 1, 2, 3]
      , fileName     = "my.bin"
      , fileProperty = fprop
        { fileType = [".bin"]
        , filePath = myBin
        , filePermission = perm { writable = False }
        , fileSizeInBytes = 4
        }
      }
    rdData    = File
      { fileData     = BS.empty
      , fileName     = "notRead"
      , fileProperty = fprop
        { filePath = rdFile
        , filePermission = perm { readable = False }
        }
      }

    dprop  = DirProperty 0 "" perm 0

    dir1  = Dir
      { dirDirs     = SMap.empty
      , dirFiles    = SMap.singleton "file1.txt" file1Data
      , dirName     = "dir1"
      , dirProperty = dprop { dirItemCount = 1, dirPath = dir1Ph }
      }
    dir22 = Dir
      { dirDirs     = SMap.empty
      , dirFiles    = SMap.singleton "my.bin" binData
      , dirName     = "dir22"
      , dirProperty = dprop { dirItemCount = 1, dirPath = dir22Ph }
      }
    dir2 = Dir
      { dirDirs     = SMap.singleton "dir22" dir22
      , dirFiles    = SMap.empty
      , dirName     = "dir2"
      , dirProperty = dprop { dirItemCount = 1, dirPath = dir2Ph }
      }
    rootDirectory = Dir
      { dirDirs     = SMap.fromList [("dir1", dir1), ("dir2", dir2)]
      , dirFiles    = SMap.singleton "notRead" rdData
      , dirName     = "tmp"
      , dirProperty = dprop { dirItemCount = 3, dirPath = testRoot}
      }

defTime :: UTCTime
defTime = UTCTime (fromOrdinalDate 2020 345) (secondsToDiffTime 0)
