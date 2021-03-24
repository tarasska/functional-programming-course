module ManagerPropertyTest
  ( managerPropertyTestTree
  ) where

import           Control.Monad.State             (runState)
import           Control.Monad.Trans.Except      (runExceptT)
import qualified Data.ByteString                 as BS
import qualified Data.ByteString.UTF8            as BS8
import qualified Data.Map.Strict                 as SMap
import           Data.Time.Calendar.OrdinalDate  (fromOrdinalDate)
import           Data.Time.Clock                 (UTCTime (..),
                                                  secondsToDiffTime)
import           Hedgehog                        (Gen, Property, PropertyT,
                                                  Range, discard, forAll,
                                                  property, (===))
import qualified Hedgehog.Gen                    as Gen
import qualified Hedgehog.Range                  as Range
import           System.FilePath.Posix           (splitPath, (</>))
import           Test.Tasty                      (TestTree, testGroup)
import           Test.Tasty.Hedgehog             (testProperty)

import           FileManager                     (execCat, execCd,
                                                  execCreateFile,
                                                  execCreateFolder, execLs,
                                                  execRemove)
import           RealFileSystem.FileSystemReader (updateDirSize)
import           Structure.Error                 (ManagerException (..))
import           Structure.FileSystemType        (Dir (..), File (..),
                                                  FileSysRoot (..),
                                                  ManagerState (..))
import           Util.FileManagerUtils           (createEmptyDir,
                                                  createEmptyFile)

managerPropertyTestTree :: IO TestTree
managerPropertyTestTree = return $ testGroup "Manager property with random FS" $
  [ testProperty "cd never change file system"     cdFileSystemUnmodifiedProp
  , testProperty "create + remove      ≡ id"       createRemoveProp
  , testProperty "cd + dir             ≡ ls"       cdDirLsProp
  , testProperty "cd + (cat . )        ≡ cat path" cdCatCatProp
  , testProperty "double file creation ≡ error"    doubleFileCreationProp
  , testProperty "double dir  creation ≡ error"    doubleDirCreationProp
  , testProperty "cd upper than root   ≡ error"    cdUpperThanRoot
  ]

defTime :: UTCTime
defTime = UTCTime (fromOrdinalDate 2020 350) (secondsToDiffTime 0)

genInt :: Int -> Int -> Gen Int
genInt lower upper = Gen.int (Range.constant lower upper)

genStringContent :: Range Int -> Gen BS.ByteString
genStringContent range = do
  str <- Gen.string range Gen.alphaNum
  return $ BS8.fromString str

genByteContent :: Range Int -> Gen BS.ByteString
genByteContent = Gen.bytes

genFile :: FilePath -> FilePath -> Gen File
genFile path name = do
  let range = (Range.constant 0 366)
  let file  = createEmptyFile path name defTime
  content <- (Gen.choice [genStringContent range, genByteContent range])
  return file{ fileData = content }

genName :: String -> Gen String
genName pref = do
  name <- (Gen.string (Range.constant 1 10) Gen.alphaNum)
  return $ pref ++ name

genNames :: String -> Int -> Gen [String]
genNames _    0 = return []
genNames pref i = do
  names <- genNames pref (i - 1)
  suff  <- genName ""
  return $ (pref ++ (show i) ++ suff) : names

genDir :: Int -> FilePath -> FilePath -> Gen Dir
genDir 0     path directoryName = return $ createEmptyDir path directoryName
genDir depth path directoryName = do
  let emptyDir = createEmptyDir path directoryName
  dirNames  <- genInt 1 3 >>= \cnt -> genNames "dir_" cnt
  fileNames <- genInt 0 4 >>= \cnt -> genNames "file_" cnt
  dirs      <- mapM (\name -> genDir (depth - 1) (path </> name) name) dirNames
  files     <- mapM (\name -> genFile (path </> name) name) fileNames
  let dir   = emptyDir{
      dirDirs  = SMap.fromList (zip dirNames  dirs)
    , dirFiles = SMap.fromList (zip fileNames files)
    }
  return $ updateDirSize dir

genFileSystem :: Int -> Gen FileSysRoot
genFileSystem depth =
  (genDir depth "/" "home") >>= \dir -> return $ FileSysRoot "/" dir

genInitState :: Gen ManagerState
genInitState = genFileSystem 4 >>= \fs -> return (ManagerState fs "")

findRandomPathDir' :: Dir -> Gen (FilePath, Dir)
findRandomPathDir' dir@Dir{ dirName = name } | null $ dirDirs dir = return (name, dir)
findRandomPathDir' dir@Dir{ dirName = name }                      = do
  stop <- Gen.bool
  if stop
  then
    return (name, dir)
  else do
    (path, rdir) <- (Gen.element $ SMap.elems $ dirDirs dir) >>= \sd -> findRandomPathDir' sd
    return (name </> path, rdir)

findRandomPathDir :: ManagerState -> Gen (FilePath, Dir)
findRandomPathDir st = do
  let root  =  rootDir $ fsRoot st
  (Gen.element $ SMap.elems $ dirDirs root) >>= \sd -> findRandomPathDir' sd

findRandomPathToDir :: ManagerState -> Gen FilePath
findRandomPathToDir st = fst <$> (findRandomPathDir st)

insertDoubleDots :: FilePath -> Gen FilePath
insertDoubleDots path = do
  ddotCnt <- genInt (splitedLen + 1) (2 * splitedLen)
  insertDoubleDots' (splitPath path) ddotCnt
  where
    splitedPath = splitPath path
    splitedLen  = length splitedPath
    ddot        = ".."

    insertDoubleDots' :: [FilePath] -> Int -> Gen FilePath
    insertDoubleDots' [] cnt = return $ foldl (</>) "" (replicate cnt ddot)
    insertDoubleDots' parts@(part : ps) cnt = do
      putDdot <- Gen.bool
      if cnt > 0 && putDdot
      then do
        newPath <- (insertDoubleDots' parts (cnt - 1))
        return $ ddot </> newPath
      else do
        newPath <- (insertDoubleDots' ps cnt)
        return $ part </> newPath

emptyOk :: Either ManagerException () -> PropertyT IO ()
emptyOk res = res === Right ()

createRemoveProp :: Property
createRemoveProp = property $ do
  st          <- forAll genInitState
  path        <- forAll $ findRandomPathToDir st
  newDirName  <- forAll $ genName "new_dir_"
  newFileName <- forAll $ genName "new_file_"
  let (res1, st1) = runState (runExceptT $ execCd path) st
  emptyOk res1
  let (res2, st2) = runState (runExceptT $ execCreateFolder newDirName) st1
  emptyOk res2
  let (res3, st3) = runState (runExceptT $ execCreateFile newFileName defTime) st2
  emptyOk res3
  let (res4, st4) = runState (runExceptT $ execCd "/") st3
  emptyOk res4
  let (res5, st5) = runState (runExceptT $ execRemove (path </> newDirName)) st4
  emptyOk res5
  let (res6, st6) = runState (runExceptT $ execRemove (path </> newFileName)) st5
  emptyOk res6
  st6 === st

cdDirLsProp :: Property
cdDirLsProp = property $ do
  st   <- forAll genInitState
  path <- forAll $ findRandomPathToDir st
  let (resLs, st1) = runState (runExceptT $ execLs path) st
  st1 === st
  let (resCd, st2) = runState (runExceptT $ execCd path) st1
  emptyOk resCd
  let (resDir, _) = runState (runExceptT $ execLs ".") st2
  resDir === resLs

cdCatCatProp :: Property
cdCatCatProp = property $ do
  st          <- forAll genInitState
  (path, dir) <- forAll $ findRandomPathDir st
  if null $ dirFiles dir
  then
    discard
  else do
    fileName' <- forAll $ Gen.element $ SMap.keys $ dirFiles dir
    let (resCatPath, st1) = runState (runExceptT $ execCat (path </> fileName')) st
    st1 === st
    let (resCd, st2) = runState (runExceptT $ execCd path) st1
    emptyOk resCd
    let (resCatCur, _) = runState (runExceptT $ execCat fileName') st2
    resCatPath === resCatCur

doubleCreation
  :: (FilePath -> ManagerState -> (Either ManagerException (), ManagerState))
  -> (FilePath -> ManagerException)
  -> Property
doubleCreation createFunc ex = property $ do
  st   <- forAll genInitState
  path <- forAll $ findRandomPathToDir st
  name <- forAll $ genName "new_entity_"
  let (resCd, st1) = runState (runExceptT $ execCd path) st
  emptyOk resCd
  let (resCr1, st2) = createFunc name st1
  emptyOk resCr1
  let (resCr2, _) = createFunc name st2
  resCr2 === (Left $ ex (path </> name))

doubleFileCreationProp :: Property
doubleFileCreationProp = doubleCreation
  (\name st -> runState (runExceptT $ execCreateFile name defTime) st)
  FileExist

doubleDirCreationProp :: Property
doubleDirCreationProp = doubleCreation
  (\name st -> runState (runExceptT $ execCreateFolder name) st)
  DirExist

cdFileSystemUnmodifiedProp :: Property
cdFileSystemUnmodifiedProp = property $ do
  st   <- forAll genInitState
  path <- forAll $ findRandomPathToDir st
  let (resCd, st1) = runState (runExceptT $ execCd path) st
  emptyOk resCd
  fsRoot st === fsRoot st1

cdUpperThanRoot :: Property
cdUpperThanRoot = property $ do
  st      <- forAll genInitState
  path    <- forAll $ findRandomPathToDir st
  badPath <- forAll $ insertDoubleDots path
  let (resCd, _) = runState (runExceptT $ execCd badPath) st
  resCd === Left OutOfRootBound
