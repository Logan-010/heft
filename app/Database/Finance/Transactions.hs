{-# LANGUAGE OverloadedStrings #-}

module Database.Finance.Transactions (new, remove, edit) where

import Database.SQLite.Simple
  ( Connection,
    Only (..),
    query,
    withImmediateTransaction,
  )

new :: Connection -> String -> String -> Int -> IO (Either String Int)
new db username accountName amount = withImmediateTransaction db $ do
  accounts <-
    query
      db
      "SELECT accounts.id, accounts.user_id FROM accounts \
      \JOIN users ON users.id = accounts.user_id \
      \WHERE users.username = ? AND accounts.name = ?"
      (username, accountName) ::
      IO [(Int, Int)]
  case accounts of
    [(accountId, userId)] -> do
      transactions <-
        query
          db
          "INSERT INTO transactions (user_id, account_id, amount) VALUES (?, ?, ?) RETURNING id"
          (userId, accountId, amount) ::
          IO [Only Int]
      case transactions of
        [Only transactionId] -> pure $ Right transactionId
        _ -> pure $ Left "Error: Could not create transaction"
    [] -> pure $ Left "Error: Account not found"
    _ -> pure $ Left "Error: Account name is ambiguous"

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

edit :: Connection -> String -> String -> Int -> Int -> IO (Maybe String)
edit db username accountName transactionId amount = do
  transactions <-
    query
      db
      "UPDATE transactions SET amount = ? WHERE id = ? \
      \AND user_id = (SELECT id FROM users WHERE username = ?) \
      \AND account_id IN (SELECT id FROM accounts WHERE name = ? \
      \AND user_id = transactions.user_id) RETURNING id"
      (amount, transactionId, username, accountName) ::
      IO [Only Int]
  pure $ if null transactions then Just "Error: Transaction not found" else Nothing
