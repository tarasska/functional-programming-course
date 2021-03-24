module Main where

import Control.Concurrent (threadDelay)
import Control.Monad ()
import System.Console.ANSI (Color (..), ColorIntensity (..), ConsoleLayer (..), SGR (..),
                            clearScreen, setSGR)
import Task8.ComonadContainer (Grid (..), toList, unGrid)
import Task8.CovidCore (Cell (..), HealthState (..), mkInitGrid, step)

-- | Displays the cell in color for a better perception of the symbol meanings.
cellColorPrint :: Cell -> IO ()
cellColorPrint cell@(Cell Healthy _) = do
  setSGR [SetColor Foreground Vivid Green]
  putStr (show cell)
cellColorPrint cell@(Cell (Incubation _) _) = do
  setSGR [SetColor Foreground Vivid Yellow]
  putStr (show cell)
cellColorPrint cell@(Cell (Sick _) _) = do
  setSGR [SetColor Foreground Vivid Red]
  putStr (show cell)
cellColorPrint cell@(Cell (Immunity _) _) = do
  setSGR [SetColor Foreground Vivid Blue]
  putStr (show cell)

-- | Prints a multi-colored field.
gridColorPrint :: Int -> Grid Cell -> IO ()
gridColorPrint sideSize g = do
  let fixList = toList sideSize
  let rows = fmap fixList $ fixList (unGrid g)
  mapM_ (\row -> mapM_ cellColorPrint row >> putStrLn "") rows

-- | Displays n days of the spread of Covid.
runSimulation :: (Grid Cell -> Grid Cell) -> Int -> Int -> Grid Cell -> IO ()
runSimulation _          _        0        _    = return ()
runSimulation customStep sideSize daysLeft grid = do
  clearScreen
  gridColorPrint sideSize grid
  threadDelay 500000
  runSimulation customStep sideSize (daysLeft - 1) (customStep grid)

-- | The main method is to request configuration parameters for the spread of the virus.
main :: IO ()
main = do
  putStrLn "Enter probability of infection: "
  p <- readLn :: IO Double
  putStrLn "Enter incubation period (in days): "
  incubPer <- readLn :: IO Int
  putStrLn "Enter duration of illness (in days): "
  durOfIllness <- readLn :: IO Int
  putStrLn "Enter duration of immunity (in days): "
  durOfImmunity <- readLn :: IO Int
  putStrLn "Enter the length of the visible side of the field: "
  sideLen <- readLn :: IO Int
  putStrLn "Enter days to show: "
  days <- readLn :: IO Int
  initGrid <- (mkInitGrid sideLen incubPer)
  runSimulation (step p incubPer durOfIllness durOfImmunity) sideLen days initGrid
