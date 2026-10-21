{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Http.Finance.Transactions where

import Control.Exception (IOException, try)
import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Text (Text)
import qualified Logger
import qualified Data.Text as T
import qualified Database.Finance.Transactions
import Database.SQLite.Simple (Connection)
import GHC.Generics (Generic)
import qualified Http.Auth
import Logger (Logger)
import Network.HTTP.Types (status200, status400, status500)
import Network.Wai (Application, responseLBS, strictRequestBody)

data ListRequest = ListRequest
  { name :: Text
  }
  deriving (Show, Generic)

instance FromJSON ListRequest

data ListResponse = ListResponse
  { name :: Text,
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
