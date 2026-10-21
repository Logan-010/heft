{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Http.Finance.Transactions (list, new, remove) where

import Control.Exception (IOException, try)
import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Database.Finance.Transactions
import Database.SQLite.Simple (Connection)
import GHC.Generics (Generic)
import qualified Http.Auth
import Logger (Logger)
import qualified Logger
import Network.HTTP.Types (status200, status400, status500)
import Network.Wai (Application, responseLBS, strictRequestBody)

data ListRequest = ListRequest
  { accountName :: Text
  }
  deriving (Show, Generic)

instance FromJSON ListRequest

data ListResponse = ListResponse
  { transactionName :: Text,
    ammount :: Int,
    timestamp :: Int
  }
  deriving (Show, Generic)

instance ToJSON ListResponse

list :: Connection -> Logger -> Application
list db logger req respond =
  strictRequestBody req >>= \b -> case eitherDecode b of
    Left _ -> respond $ responseLBS status400 [("Content-Type", "text/plain")] $ LBS.pack "Bad request"
    Right (ListRequest n) ->
      Http.Auth.authenticate db req >>= \case
        Left (status, err) ->
          respond
            $ responseLBS
              status
              [("Content-Type", "text/plain")]
            $ LBS.pack err
        Right u ->
          try (Database.Finance.Transactions.list db u $ T.unpack n) >>= \case
            Left (err :: IOException) ->
              Logger.logMsg
                logger
                Logger.Warn
                ( T.pack $
                    show err
                )
                >> respond (responseLBS status500 [("Content-Type", "text/plain")] "Internal Server Error")
            Right (Left err) ->
              respond
                $ responseLBS
                  status400
                  [("Content-Type", "text/plain")]
                $ LBS.pack err
            Right (Right t) ->
              respond
                $ responseLBS
                  status200
                  [("Content-Type", "aplication/json")]
                $ encode
                $ map (\(na, a, ti) -> ListResponse (T.pack na) a ti) t

data NewRequest = NewRequest
  { name :: Text,
    accountName :: Text,
    ammount :: Int
  }
  deriving (Show, Generic)

instance FromJSON NewRequest

new :: Connection -> Logger -> Application
new db logger req respond =
  strictRequestBody req >>= \b -> case eitherDecode b of
    Left _ -> respond $ responseLBS status400 [("Content-Type", "text/plain")] $ LBS.pack "Bad request"
    Right (NewRequest n a m) ->
      Http.Auth.authenticate db req >>= \case
        Left (status, err) ->
          respond
            $ responseLBS
              status
              [("Content-Type", "text/plain")]
            $ LBS.pack err
        Right u ->
          getTimestamp >>= \t ->
            try (Database.Finance.Transactions.new db u (T.unpack n) (T.unpack a) m t) >>= \case
              Left (err :: IOException) ->
                Logger.logMsg
                  logger
                  Logger.Warn
                  ( T.pack $
                      show err
                  )
                  >> respond (responseLBS status500 [("Content-Type", "text/plain")] "Internal Server Error")
              Right (Left err) ->
                respond
                  $ responseLBS
                    status400
                    [("Content-Type", "text/plain")]
                  $ LBS.pack err
              Right (Right tid) ->
                respond
                  $ responseLBS
                    status200
                    [("Content-Type", "aplication/json")]
                  $ LBS.pack
                  $ show tid
  where
    getTimestamp = floor <$> getPOSIXTime

data RemoveRequest = RemoveRequest
  { id :: Int,
    accountName :: Text
  }
  deriving (Show, Generic)

instance FromJSON RemoveRequest

remove :: Connection -> Logger -> Application
remove db logger req respond =
  strictRequestBody req >>= \b -> case eitherDecode b of
    Left _ -> respond $ responseLBS status400 [("Content-Type", "text/plain")] $ LBS.pack "Bad request"
    Right (RemoveRequest i a) ->
      Http.Auth.authenticate db req >>= \case
        Left (status, err) ->
          respond
            $ responseLBS
              status
              [("Content-Type", "text/plain")]
            $ LBS.pack err
        Right u ->
          try (Database.Finance.Transactions.remove db u (T.unpack a) i) >>= \case
            Left (err :: IOException) ->
              Logger.logMsg
                logger
                Logger.Warn
                ( T.pack $
                    show err
                )
                >> respond (responseLBS status500 [("Content-Type", "text/plain")] "Internal Server Error")
            Right (Just err) ->
              respond
                $ responseLBS
                  status400
                  [("Content-Type", "text/plain")]
                $ LBS.pack err
            Right Nothing ->
              respond
                $ responseLBS
                  status200
                  [("Content-Type", "text/plain")]
                $ LBS.pack "OK!"
