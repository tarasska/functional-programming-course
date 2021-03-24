module Block2.Task2
  ( -- * Functions
    moving
  ) where

import Control.Monad.State.Lazy (State, evalState, get, put)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty

-- | 'Queue' implementation on two lists, which uses as stacks.
-- This implementation provides amortization O(1) for push and pop.
data Queue a = Queue
  { headList :: [a]  -- ^ Stack for popping values
  , tailList :: [a]  -- ^ Stack for pushing values
  , qSize    :: Int  -- ^ Size of 'Queue'
  }

-- | 'MovingWindow' holds current window in 'Queue' and future values in list,
-- it also provides max window size as @winCapacity@.
data MovingWindow a = MovingWindow
  { window      :: Queue a  -- ^ Window of some values
  , fVals       :: [a]      -- ^ Remain values used to calculate next step
  , winCapacity :: Int      -- ^ Maximum window size
  }

-- | 'SmaState' is structure which holds 'MovingWindow' and current sum of
-- values in window to calculate state in simple moving average algorithm.
data SmaState a = SmaState
  { moveWin :: MovingWindow a  -- ^ Window of values to calculate SMA step
  , winSum  :: a               -- ^ Sum of values in window
  }

-- | Push value at the end of 'Queue'.
push :: a -> Queue a -> Queue a
push x q@Queue { tailList = tl, qSize = sz } = q { tailList = x : tl, qSize = sz + 1 }

-- | Pop value from the top of 'Queue'.
-- Returns pair of pop value and updated 'Queue'.
-- Accepts @defVal@ and returns it if queue is empty.
pop :: a -> Queue a -> (a, Queue a)
pop defVal q@Queue { qSize = 0 } = (defVal, q)
pop _ q@Queue { headList = [], tailList = tailToMove, qSize = sz } =
  let (x : newHead) = reverse tailToMove
   in (x, Queue newHead [] (sz - 1))
pop _ q@Queue { headList = (x : xs), qSize = sz } =
  (x, q { headList = xs, qSize = sz - 1 })

-- | Checks whether queue is empty.
empty :: Queue a -> Bool
empty Queue { qSize = 0 } = True
empty _                   = False

-- | Checks if new values to add to the queue have run out.
isEnded :: MovingWindow a -> Bool
isEnded MovingWindow { fVals = [] } = True
isEnded _                           = False

-- | Returns actual element count in window.
windowSize :: MovingWindow a -> Int
windowSize win = qSize (window win)

-- | Checks whether actual size equal to capacity.
isFullWindow :: MovingWindow a -> Bool
isFullWindow win = qSize (window win) == winCapacity win

-- | If the window is full, it removes the value from the beginning
-- and adds a new one to the end, returning a pair of replaced values
-- and new window. Otherwise adds a new value without deleting anything.
-- If deleting or added value doesn't exist, returns @defVal@.
shiftWindow :: MovingWindow a -> a -> (MovingWindow a, (a, a))
shiftWindow win@MovingWindow { fVals = [] } defVal    = (win, (defVal, defVal))
shiftWindow win@MovingWindow { window = oldWin, fVals = (fv : fvs) } defVal
  | qSize oldWin < winCapacity win =
    (win { window = push fv oldWin, fVals = fvs } , (defVal, fv))
  | otherwise = (win { window = push fv midWin, fVals = fvs }, (old, fv))
    where (old, midWin) = pop defVal oldWin

-- | Calculates list of simple moving average using State monad.
-- Using the queue and the calculation formula, the repeated summing
-- of the same values in the window doesn't occur.
calcSma :: State (SmaState Double) [Double]
calcSma = do
  smaState@SmaState{ moveWin = win, winSum = winSum } <- get

  let (newWin, (popVal, pushVal)) = shiftWindow win 0
  let curWinLen = fromIntegral $ windowSize newWin
  let newWinSum = winSum + pushVal - popVal
  let sma       = newWinSum / curWinLen

  if isEnded newWin
  then return [sma]
  else do
    put (SmaState newWin newWinSum)
    res <- calcSma
    return $ (sma : res)

-- | Takes window size and list of function 'Double' values and returns
-- list of 'Double' - result of simple moving average calculation.
-- Failed with error if size of window less then zero.
moving :: Int -> [Double] -> [Double]
moving windowSize []
  | windowSize < 0 = error "Negative window size is prohibited."
  | otherwise      = []
moving windowSize baseVals
  | windowSize < 0 = error "Negative window size is prohibited."
  | otherwise      =
    evalState calcSma (SmaState (MovingWindow (Queue [] [] 0) baseVals windowSize) 0)
