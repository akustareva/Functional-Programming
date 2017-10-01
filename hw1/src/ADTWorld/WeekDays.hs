module ADTWorld.WeekDays
       ( WeekDay (..)
       , nextDay
       , afterDays
       , isWeekend
       , daysToParty
       ) where

import           Data.Ix (Ix (..))

data WeekDay = Mon | Tue | Wed | Thu | Fri | Sat | Sun
    deriving  (Eq, Ord, Enum, Bounded, Show, Ix)

nextDay :: WeekDay -> WeekDay
nextDay day
  | day == maxBound = minBound
  | otherwise       = succ day

afterDays :: WeekDay -> Int -> WeekDay
afterDays day n = iterate nextDay day !! n

isWeekend :: WeekDay -> Bool
isWeekend day = day == Sat || day == Sun

daysToParty :: WeekDay -> Int
daysToParty day
  | day <= Fri = rangeSize (day, Fri) - 1
  | otherwise  = rangeSize (day, maxBound) + rangeSize (minBound, Fri) - 1
