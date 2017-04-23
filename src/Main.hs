{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception.Safe (SomeException, Exception, throwM, MonadCatch)
import Control.Monad (when, forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Either (runEitherT)
import Data.Hucheduler
import Data.Monoid ((<>))
import Data.Time.Calendar (Day)
import Data.Time.Clock (UTCTime(..), getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Safe (readMay)
import System.EasyFile (doesFileExist)
import System.Environment (getEnv)
import System.IO (hFlush, stdout)
import qualified Data.Text.IO as TIO


-- | An exception for reading config file
data ConfigurationException = ConfigurationException String
  deriving (Show)
instance Exception ConfigurationException


-- | Run app
main :: IO ()
main = do
  schedulesOrNot <- runEitherT readHuchedulerConfig
  case schedulesOrNot of
    Left  e         -> print (e :: SomeException)
    Right schedules -> do
      x <- todayIsConfirmed
      when (not x) $ do
        viewSchedulesOfToday schedules
        confirmToRemember
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

    -- | Make sure you should remember it or not
    confirmToRemember :: IO ()
    confirmToRemember = do
      prompt "Do you remember reading the schedule ? (y/n) "
      x <- getChar
      case x of
        'y' -> createMarkerOfToday
        'n' -> return ()
        _   -> putStrLn "" >> confirmToRemember  -- Confirm once more

    -- Remember reading the schedule.
    -- See @getMarkerFilePath@ .
    createMarkerOfToday :: IO ()
    createMarkerOfToday = do
      today          <- utctDay <$> getCurrentTime
      markerFilePath <- getMarkerFilePath today
      writeFile markerFilePath ""
      putStrLn "reading the schedule is remembered !"


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
-- ('mark' and 'marker' means files of ~/.cache/hucheduler/daily/*_is-checked)
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

-- | Execute putStr without buffering
prompt :: String -> IO ()
prompt msg = putStr msg >> hFlush stdout
