module Task2
  ( -- * Functions
    monteCarloPar
  , monteCarloSeq
  ) where

import Control.Monad.Par (runPar)
import Control.Monad.Par.Combinator (InclusiveRange (..), parMapReduceRange)
import System.Random (RandomGen, mkStdGen, randomRs)

-- | Sequentially calculates the integral by the usual Monte Carlo method.
integrateSeq :: (Double -> Double) -> Int -> [Double] -> Double -> Double -> Double
integrateSeq f n xs from to = (to - from) / (fromIntegral n) * (sum $ map f xs)

-- | Calculates the integral in parallel using the usual Monte Carlo method.
-- The points at which the function is evaluated are divided into batches
-- that will be executed sequentially. The rest depends on the state of the system.
integratePar :: (Double -> Double) -> Int -> Int -> Double -> Double -> Double
integratePar f n batch from to = (runPar parCalc) / (fromIntegral bsCnt)
  where
    bsCnt   = (n `div` batch)
    range   = InclusiveRange 1 bsCnt
    iterF   = \i -> integrateSeq f batch (genPoints from to batch (mkStdGen i)) from to
    parCalc = parMapReduceRange range (\i -> return $ iterF i) (\x y -> return (x+y)) 0

-- | Generates a list of random points uniformly distributed in (from, to).
genPoints :: RandomGen g => Double -> Double -> Int -> g -> [Double]
genPoints from to cnt g = take cnt (randomRs (from, to) g)

-- | Function for using the sequential Monte Carlo method.
-- Takes a function to integrate, interval boundaries and number of points.
monteCarloSeq :: (Double -> Double) -> Double -> Double -> Int -> Double
monteCarloSeq f from to n
  | n <= 0     = error "Points cnt must be positive"
  | otherwise  = integrateSeq f n (genPoints from to n (mkStdGen 1)) from to

-- | Function to use the parallel Monte Carlo method.
-- Accepts a function to integrate, interval boundaries,
-- packet size for sequential computation, and number of points.
monteCarloPar :: (Double -> Double) -> Double -> Double -> Int -> Int -> Double
monteCarloPar f from to batch n
  | n <= 0 || batch <= 0 = error "Points cnt and batch size must be positive"
  | otherwise            = integratePar f n batch from to
