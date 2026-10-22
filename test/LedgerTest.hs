{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception (bracket)
import Control.Monad (forM_, unless)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Ledger (exportHledger, importHledger)
import Database.Migrate (migrate)
import Database.SQLite.Simple
import Hledger.Data (Journal (..))
import Hledger.Read (readJournal'')

assert :: String -> Bool -> IO ()
assert message condition = unless condition (fail message)

right :: (Show e) => Either e a -> IO a
right = either (fail . show) pure

snapshot :: Connection -> Int -> IO ([(Text, Text, Int64)], [(Text, Text, Int64, Text, Int64)])
snapshot conn userId = do
  accounts <-
    query
      conn
      "SELECT name, unit, initial FROM accounts WHERE user_id = ? ORDER BY name"
      (Only userId)
  transactions <-
    query
      conn
      "SELECT a.name, a.unit, t.amount, t.name, t.timestamp FROM transactions t \
      \JOIN accounts a ON a.id = t.account_id WHERE t.user_id = ? ORDER BY t.timestamp, t.id"
      (Only userId)
  pure (accounts, transactions)

entry :: Text -> Text -> Text
entry description postings = "2026-10-21 " <> description <> "\n" <> postings <> "\n"

main :: IO ()
main = bracket (open ":memory:") close $ \conn -> do
  execute_ conn "PRAGMA foreign_keys = ON"
  migrate conn
  execute_ conn "INSERT INTO users (id, username, password_hash) VALUES (1, 'alice', ''), (2, 'bob', '')"
  execute_ conn "INSERT INTO accounts (id, user_id, name, unit, initial) VALUES (1, 1, 'Checking:Daily', 'USD', 10001), (2, 1, 'Empty', '$', 0), (3, 2, 'Private', 'EUR', 999), (4, 1, 'Quoted', 'USD 2', -123)"
  let names :: [Text]
      names = ["Lunch", "App opening balance", "* special", "! pending", "(code) literal", "Coffee | café"]
  forM_ (zip [0 :: Int ..] names) $ \(index, name) ->
    execute
      conn
      "INSERT INTO transactions (user_id, account_id, amount, name, timestamp) VALUES (1, 1, ?, ?, ?)"
      (if index == 0 then minBound :: Int64 else fromIntegral index, name, 1792584001 + fromIntegral index :: Int64)
  execute
    conn
    "INSERT INTO transactions (user_id, account_id, amount, name, timestamp) VALUES (1, 2, ?, 'Maximum', -1)"
    (Only (maxBound :: Int64))
  execute_ conn "INSERT INTO transactions (user_id, account_id, amount, name, timestamp) VALUES (1, 4, 0, 'Zero', 0)"

  original <- snapshot conn 1
  otherUser <- snapshot conn 2
  exported <- exportHledger conn "alice" >>= right
  journal <- readJournal'' exported
  assert "Export must contain every account and transaction" (length (jtxns journal) == 11)
  importHledger conn "alice" exported >>= right
  restored <- snapshot conn 1
  assert "Export/import must preserve all account and transaction data" (original == restored)
  reexported <- exportHledger conn "alice" >>= right
  assert "Export/import/export must be stable" (exported == reexported)
  otherAfter <- snapshot conn 2
  assert "Import must not change another user's data" (otherUser == otherAfter)

  malformed <- importHledger conn "alice" "not a journal"
  assert "Syntax errors must still be reported" (either (const True) (const False) malformed)
  unchanged <- snapshot conn 1
  assert "Syntax errors must preserve existing data" (unchanged == original)

  let partialJournals =
        [ ("fractional cents", entry "Fraction" "    assets:Checking  0.001 USD\n    equity:unclassified  -0.001 USD\n"),
          ("virtual postings", entry "Virtual" "    [assets:Checking]  1 USD\n    [equity:unclassified]  -1 USD\n"),
          ("posting date mismatch", entry "Posting date" "    assets:Checking  1 USD  ; date: 2026-10-20\n    equity:unclassified  -1 USD\n"),
          ("costed amounts", entry "Cost" "    assets:Checking  1 USD @ 1 USD\n    equity:unclassified  -1 USD\n"),
          ("multiple asset postings", entry "Transfer" "    assets:Checking  1 USD\n    assets:Savings  -1 USD\n"),
          ("unbalanced", entry "Unbalanced" "    assets:Checking  1 USD\n    equity:unclassified  -2 USD\n"),
          ("unsupported balancing account", entry "Unknown" "    assets:Checking  1 USD\n    income:Other  -1 USD\n"),
          ("duplicate openings", T.replicate 2 (entry "App opening balance" "    assets:Checking  1 USD\n    equity:opening-balances  -1 USD\n")),
          ("multiple commodities", entry "USD" "    assets:Checking  1 USD\n    equity:unclassified  -1 USD\n" <> entry "EUR" "    assets:Checking  1 EUR\n    equity:unclassified  -1 EUR\n"),
          ("invalid timestamp", entry "Bad timestamp" "    ; app-timestamp: abc\n    assets:Checking  1 USD\n    equity:unclassified  -1 USD\n"),
          ("overflowing timestamp", entry "Bad timestamp" "    ; app-timestamp: 18446744075502135616\n    assets:Checking  1 USD\n    equity:unclassified  -1 USD\n"),
          ("date mismatch", entry "Wrong date" "    ; app-timestamp: 0\n    assets:Checking  1 USD\n    equity:unclassified  -1 USD\n"),
          ("duplicate timestamp", entry "Duplicate tag" "    ; app-timestamp: 1792584000, app-timestamp: 1792584000\n    assets:Checking  1 USD\n    equity:unclassified  -1 USD\n"),
          ("overflowing amount", entry "Overflow" "    assets:Checking  92233720368547758.08 USD\n    equity:unclassified  -92233720368547758.08 USD\n")
        ]
      usable = entry "Keep me" "    assets:Kept  2 USD\n    income:Salary  -2 USD\n"
  forM_ partialJournals $ \(name, input) -> do
    importHledger conn "alice" (input <> usable) >>= right
    (_, after) <- snapshot conn 1
    assert
      (name ++ " must not discard usable entries")
      (("Kept", "USD", 200, "Keep me", 1792540800) `elem` after)

  let checkImport name input expectedAccounts expectedTransactions = do
        importHledger conn "alice" input >>= right
        actual <- snapshot conn 1
        assert name (actual == (expectedAccounts, expectedTransactions))
      midnight = 1792540800
  checkImport
    "Transfers must create both account movements"
    (entry "Transfer" "    assets:Checking  -1 USD\n    assets:Savings  1 USD\n")
    [("Checking", "USD", 0), ("Savings", "USD", 0)]
    [("Checking", "USD", -100, "Transfer", midnight), ("Savings", "USD", 100, "Transfer", midnight)]
  checkImport
    "Partially supported transactions must retain exact asset amounts and ignore costs"
    (entry "Partial" "    assets:Checking  1 USD @ 1 USD\n    assets:Tiny  0.001 USD\n    (assets:Virtual)  5 USD\n    income:Other  -1.001 USD\n")
    [("Checking", "USD", 0)]
    [("Checking", "USD", 100, "Partial", midnight)]
  checkImport
    "An omitted asset amount must be inferred from expense postings"
    (entry "Lunch" "    expenses:Food  12.50 USD\n    assets:Wallet\n")
    [("Wallet", "USD", 0)]
    [("Wallet", "USD", -1250, "Lunch", midnight)]
  checkImport
    "Usable components of an inferred multi-commodity amount must survive"
    (entry "Mixed" "    expenses:Food  1 USD\n    expenses:Tiny  0.001 EUR\n    assets:Wallet\n")
    [("Wallet", "USD", 0)]
    [("Wallet", "USD", -100, "Mixed", midnight)]
  checkImport
    "Unresolved amounts must be skipped while explicit amounts survive"
    (entry "Unresolved" "    assets:Known  2 USD\n    assets:Unknown\n    income:Unknown\n")
    [("Known", "USD", 0)]
    [("Known", "USD", 200, "Unresolved", midnight)]
  checkImport
    "Unbalanced transactions must retain explicit usable asset amounts"
    (entry "Unbalanced" "    assets:Checking  1 USD\n    income:Other  -2 USD\n")
    [("Checking", "USD", 0)]
    [("Checking", "USD", 100, "Unbalanced", midnight)]
  checkImport
    "Unsupported-only input must succeed as an empty ledger"
    (entry "Skip" "    assets:Tiny  0.001 USD\n    income:Other  -0.001 USD\n")
    []
    []
  checkImport
    "Repeated opening balances must accumulate"
    (T.replicate 2 (entry "Opening balances" "    assets:Checking  1 USD\n    equity:opening-balances  -1 USD\n"))
    [("Checking", "USD", 200)]
    []
  checkImport
    "An overflowing opening balance must be skipped without losing the valid balance"
    ( entry "Opening" "    assets:Checking  92233720368547758.07 USD\n    equity:opening-balances  -92233720368547758.07 USD\n"
        <> entry "Extra opening" "    assets:Checking  0.01 USD\n    equity:opening-balances  -0.01 USD\n"
    )
    [("Checking", "USD", maxBound)]
    []
  checkImport
    "Conflicting commodities must keep the first usable commodity"
    (entry "USD" "    assets:Checking  1 USD\n    income:Other  -1 USD\n" <> entry "EUR" "    assets:Checking  1 EUR\n    income:Other  -1 EUR\n")
    [("Checking", "USD", 0)]
    [("Checking", "USD", 100, "USD", midnight)]
  checkImport
    "Posting-specific dates must be retained"
    (entry "Dated" "    assets:Checking  1 USD  ; date: 2026-10-20\n    income:Other  -1 USD\n")
    [("Checking", "USD", 0)]
    [("Checking", "USD", 100, "Dated", midnight - 86400)]
  forM_ ["abc", "18446744075502135616", "0", "1792584000, app-timestamp: 1792584000"] $ \tag ->
    checkImport
      "Unusable timestamp tags must fall back to UTC midnight"
      (entry "Fallback" ("    ; app-timestamp: " <> tag <> "\n    assets:Checking  1 USD\n    income:Other  -1 USD\n"))
      [("Checking", "USD", 0)]
      [("Checking", "USD", 100, "Fallback", midnight)]

  importHledger conn "alice" exported >>= right

  execute_ conn "CREATE TRIGGER reject_import BEFORE INSERT ON transactions BEGIN SELECT RAISE(ABORT, 'test insert failure'); END"
  failed <- importHledger conn "alice" exported
  assert "Database failures must be reported" (either (const True) (const False) failed)
  rolledBack <- snapshot conn 1
  assert "Database failures must roll back the entire replacement" (rolledBack == original)
  execute_ conn "DROP TRIGGER reject_import"

  imported <- importHledger conn "alice" (entry "Ordinary" "    assets:New  1.25 USD\n    equity:unclassified\n")
  right imported
  (accounts, transactions) <- snapshot conn 1
  assert "An inferred balancing amount and absent opening balance must be supported" (accounts == [("New", "USD", 0)])
  assert "Dates without tags must use UTC midnight" (transactions == [("New", "USD", 125, "Ordinary", 1792540800)])
  importHledger conn "alice" "" >>= right
  empty <- snapshot conn 1
  assert "An empty journal must replace data with an empty ledger" (empty == ([], []))
  emptyExport <- exportHledger conn "alice" >>= right
  assert "An empty ledger must export successfully" (T.null emptyExport)
  missingExport <- exportHledger conn "missing"
  assert "Export must report missing users" (missingExport == Left "User not found")
  missingImport <- importHledger conn "missing" exported
  assert "Import must report missing users" (missingImport == Left "User not found")

  forM_ ["Double  space", "Trailing:", "Comment;account", "Embedded\nnewline"] $ \name -> do
    execute conn "UPDATE accounts SET name = ? WHERE user_id = 2" (Only (name :: Text))
    partialExport <- exportHledger conn "bob" >>= right
    assert "Export must skip names that cannot round-trip" (T.null partialExport)
  execute_ conn "UPDATE accounts SET name = 'Private' WHERE user_id = 2"
  execute
    conn
    "INSERT INTO transactions (user_id, account_id, amount, name, timestamp) VALUES (2, 3, 1, 'Far future', ?)"
    (Only (maxBound :: Int64))
  partialDateExport <- exportHledger conn "bob" >>= right
  dateJournal <- readJournal'' partialDateExport
  assert "Export must skip invalid dates and retain the account" (length (jtxns dateJournal) == 1)
  execute_ conn "INSERT INTO accounts (id, user_id, name, unit, initial) VALUES (10, 2, 'Bad;name', 'EUR', 1), (11, 2, 'Private', 'USD', 2)"
  execute_ conn "INSERT INTO transactions (user_id, account_id, amount, name, timestamp) VALUES (2, 3, 10, 'Usable', 0), (2, 3, 20, 'Bad;description', 0), (2, 10, 30, 'Bad account', 0), (2, 11, 40, 'Duplicate account', 0)"
  partialExport <- exportHledger conn "bob" >>= right
  partialJournal <- readJournal'' partialExport
  assert "Export must retain usable rows alongside invalid and duplicate accounts and invalid transactions" (length (jtxns partialJournal) == 2)
  assert "Export must contain the usable transaction" ("Usable" `T.isInfixOf` partialExport)
  putStrLn "Ledger import/export tests passed."
