module Task8.CovidCore where

import Control.Comonad (extend, extract)
import System.Random (StdGen, mkStdGen, random, randomIO)

import Task8.ComonadContainer (Grid (..), ListZipper (..), down, gridRead, gridWrite, horizontal,
                               iterateTail, left, listLeft, listRight, listWrite, mkZipper,
                               neighbours, right, toList, up, vertical)

-- | Type describes a person's health condition.
data HealthState
  = Healthy         -- ^ Person is healthy
  | Sick Int        -- ^ Person will be sick for n days
  | Incubation Int  -- ^ Person will be secretly ill for n days
  | Immunity Int    -- ^ Person will be protected by immunity for n days

-- | Displaying health status on the field.
instance Show HealthState where
  show Healthy        = "*"
  show (Sick _)       = "#"
  show (Incubation _) = "?"
  show (Immunity _)   = "@"

-- | The type of reflective cell of the Covid spreading field.
-- Contains a health condition and an infection probability generator
data Cell = Cell
  { hState :: HealthState
  , rand   :: StdGen
  }

-- | 'Cell' is displayed as 'HealthState'.
instance Show Cell where
  show (Cell hs _) = show hs

-- | Tells if a cell is infectious.
isDangerCell :: Cell -> Bool
isDangerCell (Cell (Sick _) _)       = True
isDangerCell (Cell (Incubation _) _) = True
isDangerCell _                       = False

-- | Tells if there is an infected neighbor.
isDangerNearby :: Grid Cell -> Bool
isDangerNearby g = any isDangerCell $
  (map (\direction -> extract $ direction g) neighbours)

-- | Creates an initial field with randomly initialized generators and provided size.
-- The virus infects the central cell.
mkInitGrid :: Int -> Int -> IO (Grid Cell)
mkInitGrid visibleSz incubDays = do
  hiddenSeed <- randomIO :: IO Int
  initSeed   <- randomIO :: IO Int
  let genSeed = mapM (\_ -> randomIO) [1..visibleSz]
  lSeed <- genSeed
  rSeed <- genSeed
  uSeed <- genSeed
  dSeed <- genSeed
  let initCell   = mkHealthyCell initSeed
  let firstCell  = initCell { hState = Incubation incubDays }

  return $ gridWrite firstCell $ Grid
    ( mkZipper
      (fmap $ cellGen uSeed hiddenSeed)
      (fmap $ cellGen dSeed hiddenSeed)
      (mkZipper (cellGen lSeed hiddenSeed) (cellGen rSeed hiddenSeed) initCell)
    )

  where
    mkHealthyCell :: Int -> Cell
    mkHealthyCell seed = Cell Healthy (mkStdGen seed)

    cellGen :: [Int] -> Int -> Cell -> Cell
    cellGen []          hSeed _    = mkHealthyCell hSeed
    cellGen (seed : ss) _     cell = cell{ rand = mkStdGen seed }

-- | Simulates one day of the spread of the Covid.
-- It takes the parameters of the disease: the probability of infection,
-- the duration of the incubation period, the duration of the disease and
-- the duration of the immune defense, as well as the old state of the field.
step :: Double -> Int -> Int -> Int -> Grid Cell -> Grid Cell
step infProb incubPeriod durOfIllness durOfImmunity grid = extend rule grid
  where
    rule :: Grid Cell -> Cell
    rule g = case (extract g) of
      cell@(Cell Healthy _) | not $ isDangerNearby g -> cell
                            | otherwise              -> genCovidAttack cell
      other@(Cell hs rnd) -> Cell (healthLoop hs) rnd

    healthLoop :: HealthState -> HealthState
    healthLoop (Sick days)
      | days > 1  = Sick (days - 1)
      | otherwise = Immunity durOfImmunity
    healthLoop (Incubation days)
      | days > 1  = Incubation (days - 1)
      | otherwise = Sick durOfIllness
    healthLoop (Immunity days)
      | days > 1  = Immunity (days - 1)
      | otherwise = Healthy

    genCovidAttack :: Cell -> Cell
    genCovidAttack (Cell _ rnd) =
      let
        (curInfProb, nextRnd) = random rnd
        newCell = \hs -> Cell hs nextRnd
       in newCell $ if curInfProb < infProb then Healthy else Incubation incubPeriod
