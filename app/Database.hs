{-# LANGUAGE OverloadedStrings #-}

module Database (openDb) where

import Control.Exception (onException)
import Database.Migrate (migrate)
import Database.SQLite.Simple (Connection, close, execute_, open)

openDb :: String -> IO Connection
openDb path = do
  conn <- open path
  let initialize = do
        execute_ conn "PRAGMA foreign_keys = ON"
        migrate conn
  initialize `onException` close conn
  pure conn
