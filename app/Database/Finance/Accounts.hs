{-# LANGUAGE OverloadedStrings #-}

module Database.Finance.Accounts (list, open, close) where

import Database.SQLite.Simple
  ( Connection,
    Only (..),
    execute,
    query,
    withImmediateTransaction,
    withTransaction,
  )

open :: Connection -> String -> String -> Int -> IO (Maybe String)
open db username accountName initialBalance = withImmediateTransaction db $ do
  users <- query db "SELECT id FROM users WHERE username = ?" (Only username) :: IO [Only Int]
  case users of
    [Only userId] -> do
      accounts <-
        query db "SELECT id FROM accounts WHERE user_id = ? AND name = ?" (userId, accountName) ::
          IO [Only Int]
      if null accounts
        then do
          execute
            db
            "INSERT INTO accounts (user_id, name, unit, initial) VALUES (?, ?, '', ?)"
            (userId, accountName, initialBalance)
          pure Nothing
        else pure $ Just "Error: Account already exists"
    _ -> pure $ Just "Error: User not found"

close :: Connection -> String -> String -> IO (Maybe String)
close db username accountName = withImmediateTransaction db $ do
  accounts <-
    query
      db
      "SELECT accounts.id, accounts.initial + COALESCE(SUM(transactions.amount), 0) \
      \FROM accounts JOIN users ON users.id = accounts.user_id \
      \LEFT JOIN transactions ON transactions.account_id = accounts.id \
      \WHERE users.username = ? AND accounts.name = ? GROUP BY accounts.id"
      (username, accountName) ::
      IO [(Int, Integer)]
  case accounts of
    [(accountId, balance)]
      | balance == 0 -> do
          execute db "DELETE FROM accounts WHERE id = ?" (Only accountId)
          pure Nothing
      | otherwise -> pure $ Just "Error: Account is not empty"
    [] -> pure $ Just "Error: Account not found"
    _ -> pure $ Just "Error: Account name is ambiguous"

list :: Connection -> String -> IO (Either String [(String, Int)])
list db username = withTransaction db $ do
  users <- query db "SELECT id FROM users WHERE username = ?" (Only username) :: IO [Only Int]
  case users of
    [Only userId] ->
      Right
        <$> query
          db
          "SELECT accounts.name, accounts.initial + COALESCE(SUM(transactions.amount), 0) \
          \FROM accounts LEFT JOIN transactions ON transactions.account_id = accounts.id \
          \WHERE accounts.user_id = ? GROUP BY accounts.id ORDER BY accounts.name, accounts.id"
          (Only userId)
    _ -> pure $ Left "Error: User not found"
