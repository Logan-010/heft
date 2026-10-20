{-# LANGUAGE OverloadedStrings #-}

module Database.Migrate (migrate) where

import Control.Monad (when)
import Database.SQLite.Simple
  ( Connection,
    Only (..),
    execute_,
    query_,
    withImmediateTransaction,
  )

migrate :: Connection -> IO ()
migrate conn = withImmediateTransaction conn $ do
  [Only version] <-
    query_ conn "PRAGMA user_version" ::
      IO [Only Int]

  when (version < 1) $ do
    migration1 conn
    execute_ conn "PRAGMA user_version = 1"

migration1 :: Connection -> IO ()
migration1 conn = do
  execute_
    conn
    "CREATE TABLE users (\
    \ id INTEGER PRIMARY KEY,\
    \ username TEXT NOT NULL UNIQUE,\
    \ password_hash TEXT NOT NULL\
    \)"

  execute_
    conn
    "CREATE TABLE sessions (\
    \ id TEXT PRIMARY KEY,\
    \ user_id INTEGER NOT NULL,\
    \ expires_at INTEGER NOT NULL,\
    \ FOREIGN KEY (user_id) REFERENCES users(id)\
    \ ON DELETE CASCADE\
    \)"

  execute_
    conn
    "CREATE INDEX idx_sessions_user_id \
    \ON sessions(user_id)"

  execute_
    conn
    "CREATE INDEX idx_sessions_expires_at \
    \ON sessions(expires_at)"
