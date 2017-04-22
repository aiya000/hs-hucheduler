{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception.Safe (SomeException, Exception, throwM, MonadCatch)
import Control.Monad (when, forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Either (runEitherT)
import Data.List (foldl1')
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Time.Calendar (Day)
import Data.Time.Clock (UTCTime(..), getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Safe (readMay)
import System.EasyFile (doesFileExist)
import System.Environment (getEnv)
import TextShow (TextShow, showb, showt)
import qualified Data.Text.IO as TIO
import qualified TextShow as TS

-- | A schedule of a day
data ScheduleOfDay = ScheduleOfDay [Text]
  deriving (Show, Read)
instance TextShow ScheduleOfDay where
  showb (ScheduleOfDay []) = TS.fromText ""
  showb (ScheduleOfDay xs) = TS.fromText $ foldl1' encloseSpaceCon xs
    where
      encloseSpaceCon :: Text -> Text -> Text
      encloseSpaceCon x y = x <> ", " <> y

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

-- |
-- Convert Int to @DayOfWeek@ .
-- The range is 1-7.
toDayOfWeek :: Int -> Maybe DayOfWeek
toDayOfWeek 1 = Just Monday
toDayOfWeek 2 = Just ThuesDay
toDayOfWeek 3 = Just WednesDay
toDayOfWeek 4 = Just ThursDay
toDayOfWeek 5 = Just Friday
toDayOfWeek 6 = Just Saturday
toDayOfWeek 7 = Just Sunday
toDayOfWeek _ = Nothing


data DayOfWeek = Monday | ThuesDay | WednesDay | ThursDay | Friday | Saturday | Sunday
  deriving (Show)

getDayOfWeekOfToday :: (MonadIO m, MonadCatch m) => m DayOfWeek
getDayOfWeekOfToday = do
  asInt <- liftIO (read . formatTime defaultTimeLocale "%u" <$> getCurrentTime)
  case toDayOfWeek asInt of
    Nothing -> throwM . DateTimeCalcException $ "Getting today's information is failed"
    Just a  -> return a


-- | An exception for reading config file
data ConfigurationException = ConfigurationException String
  deriving (Show)
instance Exception ConfigurationException

-- | An exception for calculation of date and time
data DateTimeCalcException = DateTimeCalcException String
  deriving (Show)
instance Exception DateTimeCalcException


main :: IO ()
main = do
  schedulesOrNot <- runEitherT readHuchedulerConfig
  case schedulesOrNot of
    Left  e         -> print (e :: SomeException)
    Right schedules -> do
      x <- todayIsConfirmed
      when (not x) $ viewSchedulesOfToday schedules
  where
    viewSchedulesOfToday :: Schedules -> IO ()
    viewSchedulesOfToday schedules = do
      dayOfWeekOfTodayOrNot <- runEitherT getDayOfWeekOfToday
      case dayOfWeekOfTodayOrNot of
        Left e                 -> print (e :: SomeException)
        Right dayOfWeekOfToday -> do
          let scheduleOfToday = schedules `scheduleOf` dayOfWeekOfToday
          printSchedule scheduleOfToday

    printSchedule :: ScheduleOfDay -> IO ()
    printSchedule (ScheduleOfDay []) = putStrLn "Today's schedule is nothing !"
    printSchedule (ScheduleOfDay xs) = do
      putStrLn "+ Today's schedule"
      forM_ xs $ TIO.putStrLn . ("\t- " <>)



-- |
-- Read config file detail as @Schedules@ .
-- It is possibility to be thrown an exception about the config file.
readHuchedulerConfig :: (MonadIO m, MonadCatch m) => m Schedules
readHuchedulerConfig = do
  configFilePath <- liftIO getConfigFilePath
  x              <- liftIO $ doesFileExist configFilePath
  if not x then throwM . ConfigurationException $ configFilePath ++ " cannot be found"
           else do
    maybeConfig <- liftIO (readMay <$> readFile configFilePath)
    case maybeConfig of
      Nothing     -> throwM . ConfigurationException $ "Invalid format of " ++ configFilePath
      Just config -> return config
  where
    relativeConfigFilePath :: FilePath
    relativeConfigFilePath = "/.config/hucheduler/schedules"

    getConfigFilePath :: IO FilePath
    getConfigFilePath = (++ relativeConfigFilePath) <$> getEnv "HOME"


-- |
-- Return absolutely path of daily checked file of @day@.
-- If this file is exist, regard daily checking is already finished.
getMarkerFilePath :: Day -> IO FilePath
getMarkerFilePath day = do
  homeDir <- getEnv "HOME"
  let fileName = formatTime defaultTimeLocale "%F" day ++ "_is-cheched"
  return $ homeDir ++ "/.cache/hucheduler/daily/" ++ fileName

-- |
-- Return True if today's checking is already finished.
-- Please see @getMarkerFilePath@ .
todayIsConfirmed :: (MonadIO m, MonadCatch m) => m Bool
todayIsConfirmed = do
  today      <- liftIO (utctDay <$> getCurrentTime)
  markerFile <- liftIO $ getMarkerFilePath today
  liftIO $ doesFileExist markerFile
