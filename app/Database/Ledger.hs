{-# LANGUAGE OverloadedStrings #-}

module Database.Ledger  where


import Control.Exception (SomeException, displayException, try)
import Control.Monad (forM_, unless)
import Data.Char (isLetter)
import Data.Int (Int64)
import Data.List (nub)
import Data.Ratio (denominator, numerator)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
  ( Day,
    UTCTime (..),
    defaultTimeLocale,
    formatTime,
  )
import Data.Time.Clock.POSIX
  ( posixSecondsToUTCTime,
    utcTimeToPOSIXSeconds,
  )
import Database.SQLite.Simple
  ( Connection,
    Only (..),
    execute,
    query,
    withImmediateTransaction,
  )
import Hledger.Data
  ( Amount (..),
    Journal (..),
    Posting (..),
    Transaction (..),
  )
import Hledger.Data.Amount (amounts)
import Hledger.Read (readJournal'')

data ImportedAccount = ImportedAccount
  { iaName :: Text,
    iaUnit :: Text,
    iaInitial :: Int64
  }

data ImportedTransaction = ImportedTransaction
  { itAccount :: Text,
    itUnit :: Text,
    itAmount :: Int64,
    itName :: Text,
    itTimestamp :: Int64
  }

formatAmount :: Integral a => a -> Text
formatAmount value =
  let n = toInteger value
      (whole, cents) = abs n `divMod` 100
      sign = if n < 0 then "-" else ""
      fraction = T.justifyRight 2 '0' (T.pack (show cents))
   in sign <> T.pack (show whole) <> "." <> fraction

toCents :: Amount -> Either Text Int64
toCents amount
  | acost amount /= Nothing =
      Left "Cost-based amounts are not supported"
  | acostbasis amount /= Nothing =
      Left "Cost-basis amounts are not supported"
  | denominator scaled /= 1 =
      Left "Amount has fractional cents"
  | cents < toInteger (minBound :: Int64)
      || cents > toInteger (maxBound :: Int64) =
      Left "Amount exceeds the SQLite integer range"
  | otherwise =
      Right (fromInteger cents)
  where
    scaled = toRational (aquantity amount) * 100
    cents = numerator scaled

formatDay :: Day -> Text
formatDay =
  T.pack . formatTime defaultTimeLocale "%Y-%m-%d"

-- | Convert a Unix timestamp to a UTC calendar date.
timestampDay :: Int64 -> Day
timestampDay =
  utctDay . posixSecondsToUTCTime . fromIntegral

-- | UTC midnight on the given day.
dayTimestamp :: Day -> Int64
dayTimestamp day =
  round $
    utcTimeToPOSIXSeconds (UTCTime day 0)

-- | Journal-safe names and simple commodity symbols.
validName :: Text -> Bool
validName value =
  not (T.null value)
    && T.strip value == value
    && not (T.any (`elem` ['\n', '\r', ';', '\t']) value)

validUnit :: Text -> Bool
validUnit value =
  not (T.null value) && T.all isLetter value

-- | Render a two-posting hledger transaction.
renderEntry
  :: Day
  -> Text
  -> Text
  -> Text
  -> Int64
  -> Text
  -> Maybe Int64
  -> Text
renderEntry day description account unit amount other mTimestamp =
  T.unlines $
    [ formatDay day <> " " <> description
    ]
      ++ timestampComment
      ++ [ "    " <> account
             <> "  " <> formatAmount amount <> " " <> unit,
           "    " <> other
             <> "  " <> formatAmount (negate (toInteger amount))
             <> " " <> unit,
           ""
         ]
  where
    timestampComment =
      case mTimestamp of
        Nothing -> []
        Just ts -> ["    ; app-timestamp: " <> T.pack (show ts)]

-- | Find a user by username.
findUser :: Connection -> Text -> IO (Maybe Int64)
findUser conn username = do
  rows <-
    query conn
      "SELECT id FROM users WHERE username = ?"
      (Only username)
      :: IO [Only Int64]

  pure $ case rows of
    [Only uid] -> Just uid
    _ -> Nothing

-- | Export all accounts and transactions belonging to a username.
exportHledger :: Connection -> Text -> IO (Either Text Text)
exportHledger conn username = do
  mUserId <- findUser conn username

  case mUserId of
    Nothing ->
      pure (Left "User not found")

    Just userId -> do
      accounts <-
        query conn
          "SELECT id, name, unit, initial \
          \FROM accounts WHERE user_id = ? ORDER BY id"
          (Only userId)
          :: IO [(Int64, Text, Text, Int64)]

      transactions <-
        query conn
          "SELECT account_id, amount, name, timestamp \
          \FROM transactions WHERE user_id = ? \
          \ORDER BY timestamp, id"
          (Only userId)
          :: IO [(Int64, Int64, Text, Int64)]

      let invalidAccounts =
            [ name
            | (_, name, unit, _) <- accounts,
              not (validName name && validUnit unit)
            ]

          accountKeys =
            [(name, unit) | (_, name, unit, _) <- accounts]

          ambiguousNames =
            length (nub (map (\(n, _) -> n) accountKeys))
              /= length accountKeys

          invalidTransactions =
            [ name
            | (_, _, name, _) <- transactions,
              not (validName name)
            ]

          missingAccounts =
            [ aid
            | (aid, _, _, _) <- transactions,
              aid `notElem` [i | (i, _, _, _) <- accounts]
            ]

      if not (null invalidAccounts)
        then pure (Left "Invalid account name or commodity")
        else
          if ambiguousNames
            then pure (Left "Account names must be unique")
            else
              if not (null invalidTransactions)
                then pure (Left "Invalid transaction description")
                else
                  if not (null missingAccounts)
                    then pure (Left "Transaction references a missing account")
                    else do
                      let openingDate = read "1970-01-01" :: Day

                          openingEntries =
                            [ renderEntry
                                openingDate
                                "App opening balance"
                                ("assets:" <> name)
                                unit
                                initial
                                "equity:opening-balances"
                                Nothing
                            | (_, name, unit, initial) <- accounts
                            ]

                          transactionEntries =
                            [ renderEntry
                                (timestampDay timestamp)
                                description
                                ("assets:" <> accountName)
                                unit
                                amount
                                "equity:unclassified"
                                (Just timestamp)
                            | (aid, amount, description, timestamp) <- transactions,
                              (_, accountName, unit, _) <- accounts,
                              aid == fst4 (_, accountName, unit, 0)
                            ]

                      pure $
                        Right $
                          T.concat (openingEntries ++ transactionEntries)
  where
    -- Helper for matching account IDs.
    fst4 (a, _, _, _) = a

-- | Extract one amount, rejecting multi-commodity postings.
postingAmount :: Posting -> Either Text Amount
postingAmount posting =
  case amounts (pamount posting) of
    [a] -> Right a
    _ -> Left "Multi-commodity postings are not supported"

-- | Decode a journal transaction into one application posting.
decodeTransaction
  :: Transaction
  -> Either Text (Text, Text, Int64, Int64, Bool)
decodeTransaction txn = do
  let assetPostings =
        [ p
        | p <- tpostings txn,
          "assets:" `T.isPrefixOf` paccount p
        ]

      otherPostings =
        [ p
        | p <- tpostings txn,
          not ("assets:" `T.isPrefixOf` paccount p)
        ]

  (asset, other) <-
    case (assetPostings, otherPostings) of
      ([a], [b]) -> Right (a, b)
      _ -> Left "Each transaction must have one asset and one balancing posting"

  let isOpening = tdescription txn == "App opening balance"
      expectedOther =
        if isOpening
          then "equity:opening-balances"
          else "equity:unclassified"

      accountName = T.drop 7 (paccount asset)

  unless (paccount other == expectedOther) $
    Left "Unsupported balancing account"

  unless (validName accountName) $
    Left "Invalid account name"

  a <- postingAmount asset
  b <- postingAmount other

  unless (acommodity a == acommodity b) $
    Left "Posting commodities do not match"

  unless (validUnit (acommodity a)) $
    Left "Invalid commodity"

  centsA <- toCents a
  centsB <- toCents b

  unless (toInteger centsA + toInteger centsB == 0) $
    Left "Transaction does not balance"

  let defaultTimestamp = dayTimestamp (tdate txn)

      timestampTag = lookup "app-timestamp" (ttags txn)

  timestamp <-
    case timestampTag of
      Nothing -> Right defaultTimestamp
      Just raw ->
        case reads (T.unpack (T.strip raw)) of
          [(ts, "")] -> Right ts
          _ -> Left "Invalid app-timestamp tag"

  unless (timestampDay timestamp == tdate txn) $
    Left "Timestamp does not match journal date"

  pure (accountName, acommodity a, centsA, timestamp, isOpening)

-- | Validate and prepare all imported data before writing to SQLite.
prepareImport
  :: Journal
  -> Either Text ([ImportedAccount], [ImportedTransaction])
prepareImport journal = do
  decoded <- traverse decodeTransaction (jtxns journal)

  let openingRows =
        [ (name, unit, amount)
        | (name, unit, amount, _, True) <- decoded
        ]

      keys =
        nub
          [ (name, unit)
          | (name, unit, _, _, _) <- decoded
          ]

      names = map fst keys

  unless (length names == length (nub names)) $
    Left "One account name cannot have multiple commodities"

  unless
    (length openingRows == length (nub [(n, u) | (n, u, _) <- openingRows]))
    (Left "Duplicate opening balance")

  let initialFor name unit =
        case [n | (a, u, n) <- openingRows, a == name, u == unit] of
          [n] -> n
          _ -> 0

      accounts =
        [ ImportedAccount name unit (initialFor name unit)
        | (name, unit) <- keys
        ]

      transactions =
        [ ImportedTransaction name unit amount (tdescription txn) timestamp
        | txn <- jtxns journal,
          let decodedTxn = decodeTransaction txn,
          Right (name, unit, amount, timestamp, False) <- [decodedTxn]
        ]

  pure (accounts, transactions)

-- | Replace a user's existing financial data with journal contents.
importHledger
  :: Connection
  -> Text
  -> Text
  -> IO (Either Text ())
importHledger conn username journalText = do
  mUserId <- findUser conn username

  case mUserId of
    Nothing ->
      pure (Left "User not found")

    Just userId -> do
      parsed <-
        try (readJournal'' journalText)
          :: IO (Either SomeException Journal)

      case parsed of
        Left err ->
          pure (Left (T.pack (displayException err)))

        Right journal ->
          case prepareImport journal of
            Left err ->
              pure (Left err)

            Right (accounts, transactions) -> do
              result <-
                try $
                  withImmediateTransaction conn $ do
                    execute conn
                      "DELETE FROM transactions WHERE user_id = ?"
                      (Only userId)

                    execute conn
                      "DELETE FROM accounts WHERE user_id = ?"
                      (Only userId)

                    forM_ accounts $ \account ->
                      execute conn
                        "INSERT INTO accounts \
                        \(user_id, name, unit, initial) \
                        \VALUES (?, ?, ?, ?)"
                        ( userId,
                          iaName account,
                          iaUnit account,
                          iaInitial account
                        )

                    forM_ transactions $ \txn -> do
                      accountRows <-
                        query conn
                          "SELECT id FROM accounts \
                          \WHERE user_id = ? AND name = ? AND unit = ?"
                          (userId, itAccount txn, itUnit txn)
                          :: IO [Only Int64]

                      case accountRows of
                        [Only accountId] ->
                          execute conn
                            "INSERT INTO transactions \
                            \(user_id, account_id, amount, name, timestamp) \
                            \VALUES (?, ?, ?, ?, ?)"
                            ( userId,
                              accountId,
                              itAmount txn,
                              itName txn,
                              itTimestamp txn
                            )
                        _ ->
                          fail "Imported account not found"

                :: IO (Either SomeException ())

              pure $
                case result of
                  Left err -> Left (T.pack (displayException err))
                  Right () -> Right ()

