{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Http.Finance.Accounts (list, open, close) where

import Control.Exception (IOException, try)
import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Database.Finance.Accounts
import Database.SQLite.Simple (Connection)
import GHC.Generics (Generic)
import qualified Http.Auth
import Logger (Logger)
import qualified Logger
import Network.HTTP.Types (status200, status400, status500)
import Network.Wai (Application, responseLBS, strictRequestBody)

data OpenRequest = OpenRequest
  { name :: Text,
    unit :: Text,
    balance :: Int
  }
  deriving (Show, Generic)

instance FromJSON OpenRequest

open :: Connection -> Logger -> Application
open db logger req respond =
  strictRequestBody req >>= \b -> case eitherDecode b of
    Left _ ->
      respond $
        responseLBS
          status400
          [("Content-Type", "text/plain")]
          "Bad request"
    Right (OpenRequest n un a) ->
      Http.Auth.authenticate db req >>= \case
        Left (status, err) ->
          respond
            $ responseLBS
              status
              [("Content-Type", "text/plain")]
            $ LBS.pack err
        Right u ->
          try (Database.Finance.Accounts.open db u (T.unpack n) (T.unpack un) a) >>= \case
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
              respond $
                responseLBS
                  status200
                  [("Content-Type", "text/plain")]
                  "OK!"

data CloseRequest = CloseRequest
  { name :: Text
  }
  deriving (Show, Generic)

instance FromJSON CloseRequest

close :: Connection -> Logger -> Application
close db logger req respond =
  strictRequestBody req >>= \b -> case eitherDecode b of
    Left _ ->
      respond $
        responseLBS
          status400
          [("Content-Type", "text/plain")]
          "Bad request"
    Right (CloseRequest n) ->
      Http.Auth.authenticate db req >>= \case
        Left (status, err) ->
          respond
            $ responseLBS
              status
              [("Content-Type", "text/plain")]
            $ LBS.pack err
        Right u ->
          try (Database.Finance.Accounts.close db u (T.unpack n)) >>= \case
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
              respond $
                responseLBS
                  status200
                  [("Content-Type", "text/plain")]
                  "OK!"

data ListResponse = ListResponse
  { name :: Text,
    unit :: Text,
    balance :: Int
  }
  deriving (Show, Generic)

instance ToJSON ListResponse

list :: Connection -> Logger -> Application
list db logger req respond =
  Http.Auth.authenticate db req >>= \case
    Left (status, err) ->
      respond
        $ responseLBS
          status
          [("Content-Type", "text/plain")]
        $ LBS.pack err
    Right u ->
      try (Database.Finance.Accounts.list db u) >>= \case
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
        Right (Right a) ->
          respond
            $ responseLBS
              status200
              [("Content-Type", "application/json")]
            $ encode
            $ map (\(n, un, b) -> ListResponse (T.pack n) (T.pack un) b) a
