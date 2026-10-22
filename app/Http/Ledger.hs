{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Http.Ledger (importLedger, exportLedger) where

import Control.Exception (IOException, try)
import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Database.Ledger
import Database.SQLite.Simple (Connection)
import GHC.Generics (Generic)
import qualified Http.Auth
import Logger (Logger)
import qualified Logger
import Network.HTTP.Types (status200, status400, status500)
import Network.Wai (Application, responseLBS, strictRequestBody)

data ImportRequest = ImportRequest
  { ledger :: Text
  }
  deriving (Show, Generic)

instance FromJSON ImportRequest

importLedger :: Connection -> Logger -> Application
importLedger db logger req respond =
  strictRequestBody req >>= \b -> case eitherDecode b of
    Left _ ->
      respond $
        responseLBS
          status400
          [("Content-Type", "text/plain")]
          "Bad request"
    Right (ImportRequest l) ->
      Http.Auth.authenticate db req >>= \case
        Left (status, err) ->
          respond
            $ responseLBS
              status
              [("Content-Type", "text/plain")]
            $ LBS.pack err
        Right u ->
          try (Database.Ledger.importHledger db (T.pack u) l) >>= \case
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
                $ LBS.pack (T.unpack err)
            Right (Right _) ->
              respond $
                responseLBS
                  status200
                  [("Content-Type", "text/plain")]
                  "OK!"

data ExportResponse = ExportResponse
  { ledger :: Text
  }
  deriving (Show, Generic)

instance ToJSON ExportResponse

exportLedger :: Connection -> Logger -> Application
exportLedger db logger req respond =
  Http.Auth.authenticate db req >>= \case
    Left (status, err) ->
      respond
        $ responseLBS
          status
          [("Content-Type", "text/plain")]
        $ LBS.pack err
    Right u ->
      try (Database.Ledger.exportHledger db (T.pack u)) >>= \case
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
            $ LBS.pack (T.unpack err)
        Right (Right t) ->
          respond
            $ responseLBS
              status200
              [("Content-Type", "text/plain")]
            $ encode
            $ ExportResponse t
