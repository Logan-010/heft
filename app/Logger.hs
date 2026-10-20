{-# LANGUAGE OverloadedStrings #-}

module Logger
  ( Logger,
    Level (..),
    withLogger,
    logMsg,
  )
where

import Control.Exception (bracket)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import System.Log.FastLogger
  ( LoggerSet,
    defaultBufSize,
    newStdoutLoggerSet,
    pushLogStrLn,
    rmLoggerSet,
    toLogStr,
  )

data Level
  = Debug
  | Info
  | Warn
  | Error
  deriving (Show, Eq, Ord)

newtype Logger = Logger LoggerSet

withLogger :: (Logger -> IO a) -> IO a
withLogger action =
  bracket
    (Logger <$> newStdoutLoggerSet defaultBufSize)
    (\(Logger ls) -> rmLoggerSet ls)
    action

logMsg :: Logger -> Level -> Text -> IO ()
logMsg (Logger ls) level msg = do
  now <- getCurrentTime

  let line =
        T.pack (show now)
          <> " ["
          <> T.pack (show level)
          <> "] "
          <> msg

  pushLogStrLn ls (toLogStr line)
