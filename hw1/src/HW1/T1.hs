module HW1.T1
  ( Day (..),
    nextDay,
    afterDays,
    isWeekend,
    daysToParty,
  )
where

import Numeric.Natural

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday

-- | Returns the day that follows the day of the week given as input.
nextDay :: Day -> Day
nextDay Sunday    = Monday
nextDay Monday    = Tuesday
nextDay Tuesday   = Wednesday
nextDay Wednesday = Thursday
nextDay Thursday  = Friday
nextDay Friday    = Saturday
nextDay Saturday  = Sunday

-- | Returns the day of the week after a given number of days has passed.
afterDays :: Natural -> Day -> Day
afterDays 0 day = day
afterDays n day = afterDays (n - 1) (nextDay day)

-- | Checks if the day is on the weekend.
isWeekend :: Day -> Bool
isWeekend Sunday   = True
isWeekend Saturday = True
isWeekend _        = False

-- | Computes the number of days until Friday.
daysToParty :: Day -> Natural
daysToParty Friday = 0
daysToParty day    = 1 + daysToParty (nextDay day)
