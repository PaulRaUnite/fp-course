module Week where

import           Prelude

data Week
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday
  deriving (Show, Enum)

-- map isWorkingDay [Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday]
isWorkingDay :: Week -> Bool
isWorkingDay day
  | n < 5 = True
  | otherwise = False
  where
    n = fromEnum day

-- map nextWorkingDay [Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday]
nextWorkingDay :: Week -> Week
nextWorkingDay day
  | n < 4 = toEnum $ n + 1
  | otherwise = Monday
  where
    n = fromEnum day
