module Extensions.Time 
  ( YearMonth(..)
  , dayOfZonedTime
  , timeOfZonedTime
  , previousYearMonth
  , nextYearMonth
  , yearMonthFromDay
  , yearMonthFromUTCTime
  , firstDayOfTheMonth
  , formatShortWeekday
  , formatLongMonth
  , daysOfMonth
  , currentUTCTime
  , defaultUTCTime
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import Data.Time.Clock (UTCTime(..), DiffTime, getCurrentTime)
import Data.Time.Calendar (Year, MonthOfYear, DayOfWeek(..), Day, toGregorian, fromGregorian, gregorianMonthLength)
import Data.Time.LocalTime (ZonedTime(..), LocalTime(..), TimeOfDay(..))


dayOfZonedTime :: ZonedTime -> Day
dayOfZonedTime = localDay . zonedTimeToLocalTime


timeOfZonedTime :: ZonedTime -> TimeOfDay
timeOfZonedTime = localTimeOfDay . zonedTimeToLocalTime


data YearMonth = YearMonth Year MonthOfYear
  deriving (Eq)


previousYearMonth :: YearMonth -> YearMonth
previousYearMonth (YearMonth year 1) = YearMonth (year - 1) 12
previousYearMonth (YearMonth year month) = YearMonth year (month - 1)

nextYearMonth :: YearMonth -> YearMonth
nextYearMonth (YearMonth year 12) = YearMonth (year + 1) 1
nextYearMonth (YearMonth year month) = YearMonth year (month + 1)


yearMonthFromUTCTime :: UTCTime -> YearMonth
yearMonthFromUTCTime (UTCTime day _) = 
  YearMonth year month
    where 
      (year, month, _) = toGregorian day


yearMonthFromDay :: Day -> YearMonth
yearMonthFromDay day = 
  YearMonth year month
    where 
      (year, month, _) = toGregorian day


firstDayOfTheMonth :: Day -> Day
firstDayOfTheMonth day = 
  fromGregorian year month 1
    where 
      (year, month, _) = toGregorian day


formatShortWeekday :: DayOfWeek -> Text
formatShortWeekday Monday = "Mon" 
formatShortWeekday Tuesday = "Tue" 
formatShortWeekday Wednesday = "Wed" 
formatShortWeekday Thursday = "Thu" 
formatShortWeekday Friday = "Fri" 
formatShortWeekday Saturday = "Sat" 
formatShortWeekday Sunday = "Sun" 


formatLongMonth :: Int -> Text
formatLongMonth 1 = "January"
formatLongMonth 2 = "February"
formatLongMonth 3 = "March"
formatLongMonth 4 = "April"
formatLongMonth 5 = "May"
formatLongMonth 6 = "June"
formatLongMonth 7 = "July"
formatLongMonth 8 = "August"
formatLongMonth 9 = "September"
formatLongMonth 10 = "October"
formatLongMonth 11 = "November"
formatLongMonth 12 = "December"
formatLongMonth _ = ""


daysOfMonth :: YearMonth -> [Day]
daysOfMonth (YearMonth year month) =
  fromGregorian year month <$> [1..daysInMonth]
    where
      daysInMonth = gregorianMonthLength year month


currentUTCTime :: MonadIO m => m UTCTime
currentUTCTime = liftIO getCurrentTime

defaultUTCTime :: UTCTime
defaultUTCTime = UTCTime (fromGregorian 2025 1 1) (0 :: DiffTime)
