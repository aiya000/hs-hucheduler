{-# LANGUAGE OverloadedStrings #-}

module Data.Hucheduler
  ( ScheduleOfDay (..)
  , Schedules (..)
  , emptySchedules
  , scheduleOf
  , DayOfWeek (..)
  , getDayOfWeekOfToday
  ) where

import Control.Exception.Safe (Exception, throwM, MonadCatch)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.List (foldl1')
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import TextShow (TextShow, showb, showt)
import qualified TextShow as TS

-- | An exception for calculation of date and time
data DateTimeCalcException = DateTimeCalcException String
  deriving (Show)

instance Exception DateTimeCalcException


-- | A schedule of a day
data ScheduleOfDay = ScheduleOfDay [Text]
  deriving (Show, Read)

instance TextShow ScheduleOfDay where
  showb (ScheduleOfDay []) = TS.fromText ""
  showb (ScheduleOfDay xs) = TS.fromText $ foldl1' encloseWithSpace xs
    where
      encloseWithSpace :: Text -> Text -> Text
      encloseWithSpace x y = x <> ", " <> y


-- | The schedules of a week
data Schedules = Schedules
  { mondaySchedule    :: ScheduleOfDay
  , thuesdaySchedule  :: ScheduleOfDay
  , wednesdaySchedule :: ScheduleOfDay
  , thursdaySchedule  :: ScheduleOfDay
  , fridaySchedule    :: ScheduleOfDay
  , saturdaySchedule  :: ScheduleOfDay
  , sundaySchedule    :: ScheduleOfDay
  } deriving (Show, Read)

instance TextShow Schedules where
  showb (Schedules mon thue wed thur fri sat sun) =
    TS.fromText $ "月: " <> showt mon  <>  "\n" <>
                  "火: " <> showt thue <>  "\n" <>
                  "水: " <> showt wed  <>  "\n" <>
                  "木: " <> showt thur <>  "\n" <>
                  "金: " <> showt fri  <>  "\n" <>
                  "土: " <> showt sat  <>  "\n" <>
                  "日: " <> showt sun

-- | The default value of Schedules
emptySchedules :: Schedules
emptySchedules = Schedules
  { mondaySchedule    = ScheduleOfDay []
  , thuesdaySchedule  = ScheduleOfDay []
  , wednesdaySchedule = ScheduleOfDay []
  , thursdaySchedule  = ScheduleOfDay []
  , fridaySchedule    = ScheduleOfDay []
  , saturdaySchedule  = ScheduleOfDay []
  , sundaySchedule    = ScheduleOfDay []
  }

-- | Give a @ScheduleOfDay@ of @DayOfWeek@ in @Schedules@
scheduleOf :: Schedules -> DayOfWeek -> ScheduleOfDay
scheduleOf schedules Monday    = mondaySchedule schedules
scheduleOf schedules ThuesDay  = thuesdaySchedule schedules
scheduleOf schedules WednesDay = wednesdaySchedule schedules
scheduleOf schedules ThursDay  = thursdaySchedule schedules
scheduleOf schedules Friday    = fridaySchedule schedules
scheduleOf schedules Saturday  = saturdaySchedule schedules
scheduleOf schedules Sunday    = sundaySchedule schedules


data DayOfWeek = Monday | ThuesDay | WednesDay | ThursDay | Friday | Saturday | Sunday
  deriving (Show)

-- | Get @DayOfWeek@ of today
getDayOfWeekOfToday :: (MonadIO m, MonadCatch m) => m DayOfWeek
getDayOfWeekOfToday = do
  asInt <- liftIO (read . formatTime defaultTimeLocale "%u" <$> getCurrentTime)
  case toDayOfWeek asInt of
    Nothing -> throwM . DateTimeCalcException $ "Getting today's information is failed"
    Just a  -> return a
  where
    -- |
    -- Convert Int to @DayOfWeek@ .
    -- See @Data.Time.Format.formatTime@
    toDayOfWeek :: Int -> Maybe DayOfWeek
    toDayOfWeek 1 = Just Monday
    toDayOfWeek 2 = Just ThuesDay
    toDayOfWeek 3 = Just WednesDay
    toDayOfWeek 4 = Just ThursDay
    toDayOfWeek 5 = Just Friday
    toDayOfWeek 6 = Just Saturday
    toDayOfWeek 7 = Just Sunday
    toDayOfWeek _ = Nothing
