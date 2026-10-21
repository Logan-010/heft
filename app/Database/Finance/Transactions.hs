{-# LANGUAGE OverloadedStrings #-}

module Database.Finance.Transactions (list, new, remove, edit) where

import Database.SQLite.Simple
  ( Connection,
    Only (..),
    query,
    withImmediateTransaction,
    withTransaction,
  )

new :: Connection -> String -> String -> String -> Int -> Int -> IO (Either String Int)
new db username accountName transactionName amount timestamp = withImmediateTransaction db $ do
  account <- findAccount db username accountName
  case account of
    Right (accountId, userId) -> do
      transactions <-
        query
          db
          "INSERT INTO transactions (user_id, account_id, name, amount, timestamp) \
          \VALUES (?, ?, ?, ?, ?) RETURNING id"
          (userId, accountId, transactionName, amount, timestamp) ::
          IO [Only Int]
      case transactions of
        [Only transactionId] -> pure $ Right transactionId
        _ -> pure $ Left "Error: Could not create transaction"
    Left err -> pure $ Left err

remove :: Connection -> String -> String -> Int -> IO (Maybe String)
remove db username accountName transactionId = do
  transactions <-
    query
      db
      "DELETE FROM transactions WHERE id = ? \
      \AND user_id = (SELECT id FROM users WHERE username = ?) \
      \AND account_id IN (SELECT id FROM accounts WHERE name = ? \
      \AND user_id = transactions.user_id) RETURNING id"
      (transactionId, username, accountName) ::
      IO [Only Int]
  pure $ if null transactions then Just "Error: Transaction not found" else Nothing

edit :: Connection -> String -> String -> Int -> String -> Int -> Int -> IO (Maybe String)
edit db username accountName transactionId transactionName amount timestamp = do
  transactions <-
    query
      db
      "UPDATE transactions SET name = ?, amount = ?, timestamp = ? WHERE id = ? \
      \AND user_id = (SELECT id FROM users WHERE username = ?) \
      \AND account_id IN (SELECT id FROM accounts WHERE name = ? \
      \AND user_id = transactions.user_id) RETURNING id"
      (transactionName, amount, timestamp, transactionId, username, accountName) ::
      IO [Only Int]
  pure $ if null transactions then Just "Error: Transaction not found" else Nothing

list :: Connection -> String -> String -> IO (Either String [(Int, String, Int, Int)])
list db username accountName = withTransaction db $ do
  account <- findAccount db username accountName
  case account of
    Right (accountId, userId) ->
      Right
        <$> query
          db
          "SELECT id, name, amount, timestamp FROM transactions \
          \WHERE account_id = ? AND user_id = ? ORDER BY timestamp, id"
          (accountId, userId)
    Left err -> pure $ Left err

findAccount :: Connection -> String -> String -> IO (Either String (Int, Int))
findAccount db username accountName = do
  accounts <-
    query
      db
      "SELECT accounts.id, accounts.user_id FROM accounts \
      \JOIN users ON users.id = accounts.user_id \
      \WHERE users.username = ? AND accounts.name = ?"
      (username, accountName)
  pure $ case accounts of
    [account] -> Right account
    [] -> Left "Error: Account not found"
    _ -> Left "Error: Account name is ambiguous"
