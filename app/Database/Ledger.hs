{-# LANGUAGE OverloadedStrings #-}

module Database.Ledger (exportHledger, importHledger) where

import Control.Exception (SomeAsyncException, SomeException, displayException, fromException, tryJust)
import Control.Monad (forM_)
import Data.Char (isControl)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Ratio (denominator, numerator)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
  ( Day,
    UTCTime (..),
    defaultTimeLocale,
    formatTime,
    toGregorian,
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
    withTransaction,
  )
import Hledger.Data
  ( Amount (..),
    Journal (..),
    Posting (..),
    PostingType (..),
    Transaction (..),
  )
import Hledger.Data.Amount (amounts, isMissingMixedAmount, showCommoditySymbol)
import Hledger.Data.Balancing (BalancingOpts (..), balanceSingleTransaction, defbalancingopts)
import Hledger.Data.Journal (journalReverse)
import Hledger.Read (definputopts)
import Hledger.Read.JournalReader (journalp, runErroringJournalParser)
import Hledger.Utils (customErrorBundlePretty)

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

formatAmount :: (Integral a) => a -> Text
formatAmount value =
  let n = toInteger value
      (whole, cents) = abs n `divMod` 100
      sign = if n < 0 then "-" else ""
      fraction = T.justifyRight 2 '0' (T.pack (show cents))
   in sign <> T.pack (show whole) <> "." <> fraction

toCents :: Amount -> Either Text Int64
toCents amount = integerCents amount >>= sqliteInteger

integerCents :: Amount -> Either Text Integer
integerCents amount
  | denominator scaled /= 1 =
      Left "Amount has fractional cents"
  | otherwise =
      Right cents
  where
    scaled = toRational (aquantity amount) * 100
    cents = numerator scaled

sqliteInteger :: Integer -> Either Text Int64
sqliteInteger n
  | n < toInteger (minBound :: Int64) || n > toInteger (maxBound :: Int64) =
      Left "Value exceeds the SQLite integer range"
  | otherwise = Right (fromInteger n)

trySync :: IO a -> IO (Either SomeException a)
trySync = tryJust $ \err ->
  case fromException err :: Maybe SomeAsyncException of
    Just _ -> Nothing
    Nothing -> Just err

formatDay :: Day -> Text
formatDay =
  T.pack . formatTime defaultTimeLocale "%Y-%m-%d"

timestampDay :: Int64 -> Day
timestampDay =
  utctDay . posixSecondsToUTCTime . fromIntegral

dayTimestamp :: Day -> Int64
dayTimestamp day =
  round $
    utcTimeToPOSIXSeconds (UTCTime day 0)

validName :: Text -> Bool
validName value =
  not (T.null value)
    && T.strip value == value
    && not (T.any (\c -> isControl c || c == ';') value)

validAccount :: Text -> Bool
validAccount value =
  validName value
    && not ("  " `T.isInfixOf` value)
    && all (\part -> not (T.null part) && T.strip part == part) (T.splitOn ":" value)

validUnit :: Text -> Bool
validUnit value =
  not (T.null value)
    && T.strip value == value
    && not (T.any (\c -> isControl c || c `elem` ['"', '\\']) value)

validTimestamp :: Int64 -> Bool
validTimestamp timestamp =
  let (year, _, _) = toGregorian (timestampDay timestamp)
   in year >= 1 && year <= 9999

renderEntry ::
  Day ->
  Text ->
  Text ->
  Text ->
  Int64 ->
  Text ->
  Maybe Int64 ->
  Text
renderEntry day description account unit amount other mTimestamp =
  T.unlines $
    [ formatDay day <> " (heft) " <> description
    ]
      ++ timestampComment
      ++ [ "    "
             <> account
             <> "  "
             <> formatAmount amount
             <> " "
             <> showCommoditySymbol unit,
           "    "
             <> other
             <> "  "
             <> formatAmount (negate (toInteger amount))
             <> " "
             <> showCommoditySymbol unit,
           ""
         ]
  where
    timestampComment =
      case mTimestamp of
        Nothing -> []
        Just ts -> ["    ; app-timestamp: " <> T.pack (show ts)]

findUser :: Connection -> Text -> IO (Maybe Int64)
findUser conn username = do
  rows <-
    query
      conn
      "SELECT id FROM users WHERE username = ?"
      (Only username) ::
      IO [Only Int64]

  pure $ case rows of
    [Only uid] -> Just uid
    _ -> Nothing

exportHledger :: Connection -> Text -> IO (Either Text Text)
exportHledger conn username = do
  result <- trySync (withTransaction conn (exportUser conn username))
  pure $ either (Left . T.pack . displayException) id result

exportUser :: Connection -> Text -> IO (Either Text Text)
exportUser conn username = do
  mUserId <- findUser conn username

  case mUserId of
    Nothing ->
      pure (Left "User not found")
    Just userId -> do
      accounts <-
        query
          conn
          "SELECT id, name, unit, initial \
          \FROM accounts WHERE user_id = ? ORDER BY id"
          (Only userId) ::
          IO [(Int64, Text, Text, Int64)]

      transactions <-
        query
          conn
          "SELECT account_id, amount, name, timestamp \
          \FROM transactions WHERE user_id = ? \
          \ORDER BY timestamp, id"
          (Only userId) ::
          IO [(Int64, Int64, Text, Int64)]

      let usableAccounts = reverse $ foldl' keepAccount [] accounts
          keepAccount kept row@(_, name, unit, _)
            | validAccount name
                && validUnit unit
                && all (\(_, existing, _, _) -> existing /= name) kept =
                row : kept
            | otherwise = kept
          openingEntries =
            [ renderEntry
                (read "1970-01-01")
                "App opening balance"
                ("assets:" <> name)
                unit
                initial
                "equity:opening-balances"
                Nothing
            | (_, name, unit, initial) <- usableAccounts
            ]
          transactionEntries =
            [ renderEntry
                (timestampDay timestamp)
                description
                ("assets:" <> name)
                unit
                amount
                "equity:unclassified"
                (Just timestamp)
            | (aid, amount, description, timestamp) <- transactions,
              validName description,
              validTimestamp timestamp,
              (accountId, name, unit, _) <- usableAccounts,
              aid == accountId
            ]
      pure (Right (T.concat (openingEntries ++ transactionEntries)))

readBestEffort :: Text -> IO (Either Text Journal)
readBestEffort input = do
  parsed <- runErroringJournalParser (journalp definputopts) input
  pure $ case parsed of
    Left err -> Left (T.pack (show err))
    Right (Left err) -> Left (T.pack (customErrorBundlePretty err))
    Right (Right journal) -> Right (journalReverse journal)

postingTimestamp :: Transaction -> Posting -> Maybe Int64
postingTimestamp txn posting =
  let day = fromMaybe (tdate txn) (pdate posting)
      fallback = dayTimestamp day
      tags = [value | (name, value) <- ttags txn, name == "app-timestamp"]
      tagged = case tags of
        [raw] -> case reads (T.unpack (T.strip raw)) of
          [(ts, "")] -> either (const Nothing) Just (sqliteInteger ts)
          _ -> Nothing
        _ -> Nothing
      timestamp = case tagged of
        Just ts | timestampDay ts == day -> ts
        _ -> fallback
   in if validTimestamp timestamp && timestampDay timestamp == day
        then Just timestamp
        else Nothing

decodeTransaction :: Transaction -> [(ImportedTransaction, Bool)]
decodeTransaction original =
  let txn =
        either (const original) id $
          balanceSingleTransaction defbalancingopts {ignore_assertions_ = True} original
      isOpening = any (\p -> ptype p == RegularPosting && paccount p == "equity:opening-balances") (tpostings txn)
      description = if T.null (T.strip (tdescription txn)) then "Imported transaction" else tdescription txn
   in [ (ImportedTransaction name (acommodity amount) cents description timestamp, isOpening)
      | posting <- tpostings txn,
        ptype posting == RegularPosting,
        Just name <- [T.stripPrefix "assets:" (paccount posting)],
        validAccount name,
        not (isMissingMixedAmount (pamount posting)),
        amount <- amounts (pamount posting),
        validUnit (acommodity amount),
        Right cents <- [toCents amount],
        Just timestamp <- [postingTimestamp txn posting]
      ]

prepareImport :: Journal -> ([ImportedAccount], [ImportedTransaction])
prepareImport journal =
  let (accounts, transactions) = foldl' collect ([], []) (concatMap decodeTransaction (jtxns journal))
      collect (kept, txns) (txn, opening) =
        case filter ((== itAccount txn) . iaName) kept of
          account : _
            | iaUnit account /= itUnit txn -> (kept, txns)
            | opening -> case sqliteInteger (toInteger (iaInitial account) + toInteger (itAmount txn)) of
                Left _ -> (kept, txns)
                Right initial -> (map (\a -> if iaName a == itAccount txn then a {iaInitial = initial} else a) kept, txns)
            | otherwise -> (kept, txn : txns)
          [] ->
            ( ImportedAccount (itAccount txn) (itUnit txn) (if opening then itAmount txn else 0) : kept,
              if opening then txns else txn : txns
            )
   in (reverse accounts, reverse transactions)

importHledger ::
  Connection ->
  Text ->
  Text ->
  IO (Either Text ())
importHledger conn username journalText = do
  result <- trySync (importUser conn username journalText)
  pure $ either (Left . T.pack . displayException) id result

importUser :: Connection -> Text -> Text -> IO (Either Text ())
importUser conn username journalText = do
  mUserId <- findUser conn username

  case mUserId of
    Nothing ->
      pure (Left "User not found")
    Just userId -> do
      parsed <- readBestEffort journalText
      case parsed of
        Left err ->
          pure (Left err)
        Right journal -> withImmediateTransaction conn $ do
          let (accounts, transactions) = prepareImport journal
          execute conn "DELETE FROM transactions WHERE user_id = ?" (Only userId)
          execute conn "DELETE FROM accounts WHERE user_id = ?" (Only userId)

          forM_ accounts $ \account ->
            execute
              conn
              "INSERT INTO accounts \
              \(user_id, name, unit, initial) VALUES (?, ?, ?, ?)"
              (userId, iaName account, iaUnit account, iaInitial account)

          forM_ transactions $ \txn -> do
            accountRows <-
              query
                conn
                "SELECT id FROM accounts \
                \WHERE user_id = ? AND name = ? AND unit = ?"
                (userId, itAccount txn, itUnit txn) ::
                IO [Only Int64]

            case accountRows of
              [Only accountId] ->
                execute
                  conn
                  "INSERT INTO transactions \
                  \(user_id, account_id, amount, name, timestamp) \
                  \VALUES (?, ?, ?, ?, ?)"
                  (userId, accountId, itAmount txn, itName txn, itTimestamp txn)
              _ -> fail "Imported account not found"

          pure (Right ())
