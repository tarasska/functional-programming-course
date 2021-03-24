{-# LANGUAGE ScopedTypeVariables #-}

module Task3Test
  ( chtIterOpBench
  , chtMultiGet
  , chtMultiPut
  , chtPerfTestTree
  , chtTestTree
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, cancel, concurrently_, forConcurrently_)
import Control.Exception (SomeException, catch)
import Control.Monad (forM_)
import Criterion.Main (Benchmark, bench, bgroup, defaultMain, nfIO, whnf)
import System.Random (randomRIO)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.Hspec (Spec, describe, it, shouldBe, testSpec)

import Task3 (ConcurrentHashTable, getCHT, newCHT, putCHT, sizeCHT)

chtTestTree :: IO TestTree
chtTestTree = testSpec "ConcurrentHashTable test" chtSpec

chtPerfTestTree :: IO TestTree
chtPerfTestTree = testSpec "ConcurrentHashTable banchmark" chtPerf

data Box = Box Int deriving (Eq, Show)

chtSpec :: Spec
chtSpec = do
  describe "Simple test" $ do
    it "Empty size" $ do
      cht <- newCHT :: IO (ConcurrentHashTable Int String)
      sz  <- sizeCHT cht
      sz `shouldBe` 0
    it "Empty get" $ do
      cht <- newCHT :: IO (ConcurrentHashTable Int String)
      nothing <- getCHT 0 cht
      nothing `shouldBe` Nothing
    it "Put <-> Get :: Int,Str" $ do
      cht <- newCHT :: IO (ConcurrentHashTable Int String)
      putCHT 0     "Taras"   cht
      putCHT 1     "love"    cht
      putCHT (-10) "haskell" cht
      loveIs <- getCHT (-10) cht
      who    <- getCHT 0     cht
      who    `shouldBe` Just "Taras"
      loveIs `shouldBe` Just "haskell"
    it "Rehash :: Int, Int" $ do
      cht <- newIntCHT
      forM_ [0..31] $ \i -> putCHT i (i*i) cht
      curSize <- sizeCHT cht
      sq0  <- getCHT 0  cht
      sq13 <- getCHT 13 cht
      sq31 <- getCHT 31 cht
      sq99 <- getCHT 99 cht
      curSize `shouldBe` 32
      sq0     `shouldBe` Just 0
      sq13    `shouldBe` Just 169
      sq31    `shouldBe` Just 961
      sq99    `shouldBe` Nothing
    it "Non trivial key value" $ do
      cht <- newCHT :: IO (ConcurrentHashTable String Box)
      forM_ [1..10] $ \i -> putCHT (show i) (Box i) cht
      box0 <- getCHT "0" cht
      box1 <- getCHT "1" cht
      box5 <- getCHT "5" cht
      box1 `shouldBe` Just (Box 1)
      box5 `shouldBe` Just (Box 5)
      box0 `shouldBe` Nothing
    it "Duplicate concurrent put" $ do
      cht <- newIntCHT
      let elemCnt = 10^3
      let cPut = forConcurrently_ [1..elemCnt] $ \i -> putCHT i i cht
      concurrently_ cPut cPut
      curSize <- sizeCHT cht
      curSize `shouldBe` elemCnt
    it "Unexpected cancel test" $ do
      cht <- newCHT :: IO (ConcurrentHashTable Int String)
      let action = forM_ [1..1000000000] $ \i -> putCHT (i `mod` 10000) (show i) cht
      asyncObj <- async action
      ms <- randomRIO (0, 10000 :: Int)
      threadDelay ms
      cancel asyncObj
      -- checking the operability of operations
      let v = "isOk"
      let k = 100000
      putCHT k v cht
      val <- getCHT k cht
      val `shouldBe` Just v

chtPerf :: Spec
chtPerf = do
  describe "Performance" $ do
    it "10^5 iter op" $ chtIterOpBench >>= \res -> defaultMain [res]
    it "1000 concurrent get" $ chtMultiGet >>= \res -> defaultMain [res]
    it "1000 concurrent put" $ chtMultiPut >>= \res -> defaultMain [res]

chtIterOpBench :: IO Benchmark
chtIterOpBench = do
  cht <- newIntCHT
  forM_ [123..9876] $ \i -> putCHT i i cht
  let sz = 10^5
  let putAll table = forM_ [0..sz] $ \i -> putCHT i i table
  let getAll table = forM_ [0..sz] $ \i -> getCHT i table
  return $ bgroup "iterative op"
    [ bench "put 10^5" $ whnf putAll cht
    , bench "get 10^5" $ whnf getAll cht
    ]

chtMultiGet :: IO Benchmark
chtMultiGet = do
  cht <- newIntCHT
  multiIntPut 1000 1000 cht
  return $ bgroup "Concurrent get" [bench "" $ nfIO (multiIntGet 100000 100 cht)]

chtMultiPut :: IO Benchmark
chtMultiPut = do
  cht <- newIntCHT
  return $ bgroup "Concurrent put" [bench "" $ nfIO (multiIntPut 100000 100 cht)]

multiIntPut :: Int -> Int -> ConcurrentHashTable Int Int -> IO ()
multiIntPut multi iter table = forConcurrently_ [0..multi] $ \_ ->
  forM_ [0..iter] $ \i -> putCHT i i table

multiIntGet :: Int -> Int -> ConcurrentHashTable Int Int -> IO ()
multiIntGet multi iter table = forConcurrently_ [0..multi] $ \_ ->
  forM_ [0..iter] $ \i -> getCHT i table

newIntCHT :: IO (ConcurrentHashTable Int Int)
newIntCHT = newCHT
