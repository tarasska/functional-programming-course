{-# LANGUAGE InstanceSigs #-}

module Block1.Task1
  ( -- * Types
    DaysOfTheWeek(..)
    -- * Functions
  , afterDays
  , daysToParty
  , isWeekend
  , nextDay
  ) where

-- | 'DaysOfTheWeek' data type represents days of the week
data DaysOfTheWeek
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday
  deriving Show

-- | 'DaysOfTheWeek' is instance of 'Enum'
-- You may use range 0..6 instead of days names
instance Enum DaysOfTheWeek where
  fromEnum :: DaysOfTheWeek -> Int
  fromEnum Monday    = 0
  fromEnum Tuesday   = 1
  fromEnum Wednesday = 2
  fromEnum Thursday  = 3
  fromEnum Friday    = 4
  fromEnum Saturday  = 5
  fromEnum Sunday    = 6

  toEnum :: Int -> DaysOfTheWeek
  toEnum 0 = Monday
  toEnum 1 = Tuesday
  toEnum 2 = Wednesday
  toEnum 3 = Thursday
  toEnum 4 = Friday
  toEnum 5 = Saturday
  toEnum 6 = Sunday
  toEnum _ = error "There is no day of the week for this number"

-- | 'DaysOfTheWeek' is instance of 'Eq'
-- You may check equality of two days
instance Eq DaysOfTheWeek where
  (==) :: DaysOfTheWeek -> DaysOfTheWeek -> Bool
  (==) a b = (fromEnum a) == (fromEnum b)

-- | Function 'sizeOfWeek' returns 7
sizeOfWeek :: Int
sizeOfWeek = 7

-- | Function 'nextDay' takes 'DaysOfTheWeek' object and
-- returns 'DaysOfTheWeek' representation of the next day
nextDay :: DaysOfTheWeek -> DaysOfTheWeek
nextDay day = toEnum $ mod ((fromEnum day) + 1) sizeOfWeek

-- | Function 'afterDays' takes 'DaysOfTheWeek' object and 'Int' number and
-- returns returns the day after a given number of days
afterDays :: DaysOfTheWeek -> Int -> DaysOfTheWeek
afterDays day daysToSkip = toEnum $ mod ((fromEnum day) + daysToSkip) sizeOfWeek

-- | Function 'isWeekend' takes 'DaysOfTheWeek' object and
-- returns @True@ if day is @Saturday@ or @Sunday@ and @False@ otherwise
isWeekend :: DaysOfTheWeek -> Bool
isWeekend Saturday = True
isWeekend Sunday   = True
isWeekend _        = False

-- | Function 'daysToParty' takes 'DaysOfTheWeek' object and
-- returns the number of days remaining until Friday
daysToParty :: DaysOfTheWeek -> Int
daysToParty day =
  if isWeekend day
  then (fromEnum Friday) + (sizeOfWeek - (fromEnum day))
  else (fromEnum Friday) - (fromEnum day)
