module Task1Test
  ( doubleAreaBench
  , geometryPerfTestTree
  , geometryTestTree
  , perimeterBench
  ) where

import Criterion.Main (Benchmark, bench, bgroup, defaultMain, whnf)
import System.Random (mkStdGen, randomRs)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.Hspec (Spec, describe, it, shouldBe, testSpec)

import Task1 (Point (..), doubleArea, doubleAreaSlow, perimeter, perimeterSlow)

geometryTestTree :: IO TestTree
geometryTestTree = testSpec "Geometry simple test" geometrySpec

geometrySpec :: Spec
geometrySpec = do
  describe "Perimeter" $ do
    it "0 points" $ perimeter [] `shouldBe` 0
    it "1 points" $ perimeter [Point 1 2] `shouldBe` 0
    it "2 points" $ perimeter [Point 0 0, Point 3 4] `shouldBe` (2 * 5.0)
    it "3 points" $ perimeter [Point 1 1, Point 1 13, Point (-4) 1] `shouldBe` 30.0
    it "Slow == Fast" $
      let pts = genPointList (-200) 200 1000
       in perimeter pts `shouldBe` perimeterSlow pts
  describe "Double area" $ do
    it "2 points" $ doubleArea [Point (-11) 1, Point 336 (-228)] `shouldBe` 0
    it "3 points" $ doubleArea [Point 1 1, Point 1 13, Point (-4) 1] `shouldBe` 60
    it "4 points" $
      let area = doubleArea [Point 3 (-3), Point 5 3, Point (-3) 3, Point (-5) (-3)]
       in area `shouldBe` 96
    it "Slow == Fast" $
      let pts = genPointList (-123) 169 1000
       in doubleArea pts `shouldBe` doubleAreaSlow pts


geometryPerfTestTree :: IO TestTree
geometryPerfTestTree = testSpec "Geometry banchmark" geometryPerf

geometryPerf :: Spec
geometryPerf = do
  describe "Performance" $ do
    it "Perimeter" $
      let pts = genPointList (-100) 100 (10000000)
       in defaultMain [ perimeterBench ]
    it "Double Area" $
      let pts = genPointList (-100) 100 (10000000)
       in defaultMain [ doubleAreaBench ]

doubleAreaBench :: Benchmark
doubleAreaBench = bgroup "doubleArea"
  [ bench "slow: 10^7" $ whnf doubleAreaSlow pts
  , bench "fast: 10^7" $ whnf doubleArea     pts
  ]
  where
    pts = genPointList (-100) 100 (10000000)

perimeterBench :: Benchmark
perimeterBench = bgroup "perimeter"
  [ bench "slow: 10^7" $ whnf perimeterSlow pts
  , bench "fast: 10^7" $ whnf perimeter     pts
  ]
  where
    pts = genPointList (-100) 100 (10000000)

genIntList :: Int -> Int -> Int -> [Int]
genIntList lowerB upperB cnt = take cnt (randomRs (lowerB, upperB) (mkStdGen 13))

genPointList :: Int -> Int -> Int -> [Point]
genPointList lowerB uppderB cnt = zipWith Point xs ys
  where
    xs = genIntList lowerB uppderB cnt
    ys = genIntList lowerB uppderB cnt
