module Task2Test
  ( monteCarloBench
  , monteCarloPerfTestTree
  ) where

import Criterion.Main (Benchmark, bench, bgroup, defaultMain, whnf)
import System.Random (mkStdGen, randomRs)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.Hspec (Spec, describe, it, shouldBe, testSpec)

import Task2 (monteCarloPar, monteCarloSeq)

monteCarloPerfTestTree :: IO TestTree
monteCarloPerfTestTree = testSpec "Monte Carlo banchmark" monteCarloPerf

monteCarloPerf :: Spec
monteCarloPerf = do
  describe "Seq == Par" $ do
    it "Simple eq" $
      let
        seqRes = monteCarloSeq simpleFunc 2 7 100000
        parRes = monteCarloPar simpleFunc 2 7 50 100000
       in (seqRes - parRes) <= eps `shouldBe` True
    it "Hard eq" $
      let
        seqRes = monteCarloSeq hardFunc 0.3 1.7 100000
        parRes = monteCarloPar hardFunc 0.3 1.7 50 100000
       in (seqRes - parRes) <= eps `shouldBe` True
  describe "Performance" $ do
    it "Ex1" $ defaultMain [monteCarloBench]

  where
    eps        = 1e-2
    simpleFunc = \x -> 1 / x
    hardFunc   = \x -> 1 / tan(x*x) - cos(x)

monteCarloBench :: Benchmark
monteCarloBench = bgroup "Monte Carlo"
  [ bench "slow: 10^6" $ whnf (monteCarloSeq hardFunc from to) iter
  , bench "fast: 10^6" $ whnf (monteCarloPar hardFunc from to batch) iter
  ]
  where
    from  = 0.3 :: Double
    to    = 1.7 :: Double
    iter  = 1000000 :: Int
    batch = 100 :: Int
    simpleFunc = \x -> 1 / x
    hardFunc   = \x -> 1 / tan(x*x) - cos(x)
