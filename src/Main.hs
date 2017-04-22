{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception.Safe (SomeException, Exception, throwM, MonadCatch)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Either (runEitherT)
import Data.Default (Default, def)
import Data.List (foldl1')
import Data.Monoid ((<>))
import Data.Text (Text)
import Safe (readMay)
import System.EasyFile (doesFileExist)
import System.Environment (getEnv)
import TextShow (TextShow, showb, showt)
import qualified Data.Text.IO as TIO
import qualified TextShow as TS

data ScheduleOfDay = ScheduleOfDay [Text]
  deriving (Show, Read)
instance TextShow ScheduleOfDay where
  showb (ScheduleOfDay []) = TS.fromText ""
  showb (ScheduleOfDay xs) = TS.fromText $ foldl1' encloseSpaceCon xs
    where
      encloseSpaceCon :: Text -> Text -> Text
      encloseSpaceCon x y = x <> ", " <> y

-- | A schedule of day of week
data Schedules = Schedules
  { mondaySchedule    :: ScheduleOfDay
  , thuesdaySchedule  :: ScheduleOfDay
  , wednesdaySchedule :: ScheduleOfDay
  , thursdaySchedule  :: ScheduleOfDay
  , fridaySchedule    :: ScheduleOfDay
  , saturdaySchedule  :: ScheduleOfDay
  , sundaySchedule    :: ScheduleOfDay
  } deriving (Show, Read)
instance Default Schedules where
  def = emptySchedules
instance TextShow Schedules where
  showb (Schedules mon thue wed thur fri sat sun) =
    TS.fromText $ "月: " <> showt mon  <>  "\n" <>
                  "火: " <> showt thue <>  "\n" <>
                  "水: " <> showt wed  <>  "\n" <>
                  "木: " <> showt thur <>  "\n" <>
                  "金: " <> showt fri  <>  "\n" <>
                  "土: " <> showt sat  <>  "\n" <>
                  "日: " <> showt sun

data ConfigurationException = ConfigurationException String
instance Exception ConfigurationException
instance Show ConfigurationException where
  show (ConfigurationException e) = "config: " ++ e


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


main :: IO ()
main = do
  schedulesOrNot <- runEitherT readHuchedulerConfig
  case schedulesOrNot of
    Left  e         -> putStrLn $ show (e :: SomeException)
    Right schedules -> TIO.putStrLn $ showt schedules


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
