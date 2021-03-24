{-# LANGUAGE ScopedTypeVariables #-}

module Task67Test
  ( fsLensRunTestTree
  ) where

import Control.Exception (SomeException, catch)
import qualified Data.ByteString as BS
import Lens.Micro (filtered, has, (^.), (^..), (^?))
import System.Directory (createDirectory, getTemporaryDirectory)
import System.FilePath (FilePath)
import System.FilePath.Posix ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.Hspec (Spec, describe, it, shouldBe, testSpec)

import Task67 (FS (..), cd, contents, dirContList, dirContents, dirName, file, fileName, ls, name,
               scanFileSystem)

fsLensRunTestTree :: IO TestTree
fsLensRunTestTree = testSpec "File system lens" fsSpec

fsSpec :: Spec
fsSpec = do
  describe "Static FS generation" $ do
    let exceptSafeGen = genStaticFileSystem `catch` \(e :: SomeException) -> return (False, "")
    ioFs <- return exceptSafeGen
    it "Lens/Traversal test pack" $ do
      (isGenOk, tmpRoot) <- ioFs
      if isGenOk
      then do
        fs <- scanFileSystem tmpRoot
        let expFs = pureFileSystem
        fs `shouldBe` expFs
        fs^.contents `shouldBe` (_contents expFs)
        fs^..dirContents.filtered (has dirName) `shouldBe` (filter isDir (_contents expFs))
        fs^.name `shouldBe` (_name expFs)
      else
        return ()
  describe "Pure FS" $ do
    it "Example 1" $ do
      let fs = pureFileSystem
      fs^?cd "tmp2". cd "tmp22".file "tmp22_File" `shouldBe` Just "tmp22_File"
      fs^?cd "tmp2". cd "tmp22".file "notFound" `shouldBe` Nothing
    it "Example 2" $ do
      let fs = pureFileSystem
      fs^..cd "tmp1".ls `shouldBe` ["tmp11", "tmp1_File"]
      fs^..cd "tmp2".cd "tmp22".ls `shouldBe` ["tmp22_File"]
  where
    isDir :: FS -> Bool
    isDir (Dir _ _) = True
    isDir _         = False

    isFile :: FS -> Bool
    isFile = not . isDir

genStaticFileSystem :: IO (Bool, FilePath)
genStaticFileSystem = do
  tmpRoot <- getTemporaryDirectory >>= (\path -> return $ path </> "_tarasska_tmp")
  createDirectory tmpRoot
  createDirectory (tmpRoot </> "tmp1")
  createDirectory (tmpRoot </> "tmp1" </> "tmp11")
  createDirectory (tmpRoot </> "tmp2" </> "tmp22")
  createDirectory (tmpRoot </> "tmp3")
  BS.writeFile (tmpRoot </> "rootFile") (BS.pack [0, 1])
  BS.writeFile (tmpRoot </> "tmp1" </> "tmp1_File") (BS.pack [0, 1, 1])
  BS.writeFile (tmpRoot </> "tmp2" </> "tmp22" </> "tmp22_File") (BS.pack [0, 1, 2])
  return (True, tmpRoot)

pureFileSystem :: FS
pureFileSystem = Dir
  "_tarasska_tmp"
  [ Dir "tmp1" [Dir "tmp11" [], File "tmp1_File"]
  , Dir "tmp2" [Dir "tmp22" [File "tmp22_File"]]
  , Dir "tmp3" []
  , File "rootFile"
  ]
