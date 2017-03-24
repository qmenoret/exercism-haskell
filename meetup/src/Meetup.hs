module Meetup (Weekday(..), Schedule(..), meetupDay) where

import Data.Time.Calendar (Day, fromGregorian, addDays, addGregorianMonthsClip)
import Data.Time.Calendar.WeekDate (toWeekDate)

data Weekday = Monday
             | Tuesday
             | Wednesday
             | Thursday
             | Friday
             | Saturday
             | Sunday
    deriving (Enum)

data Schedule = First
              | Second
              | Third
              | Fourth
              | Last
              | Teenth

incWeek :: Schedule -> Int
incWeek First = 1
incWeek Second = 8
incWeek Third = 15
incWeek Fourth = 22
incWeek Teenth = 13

meetupDay :: Schedule -> Weekday -> Integer -> Int -> Day
meetupDay sc wd y m = actualDay
    where
        baseD Last = addDays (-7) $ addGregorianMonthsClip 1 $ fromGregorian y m 1
        baseD _    = fromGregorian y m (incWeek sc)
        actualDay = head [x | x <- [(baseD sc)..], (ok . toWeekDate) x]
        ok (_,_,xwd) = xwd == (fromEnum wd + 1)
