{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Http.Auth (authenticate, register, login, validate) where

import Control.Exception (IOException, try)
import Data.Aeson (FromJSON, eitherDecode)
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Password.Argon2 (Argon2, PasswordCheck (PasswordCheckFail, PasswordCheckSuccess), PasswordHash, checkPassword, mkPassword)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Database.Auth
import Database.SQLite.Simple (Connection)
import GHC.Generics (Generic)
import Logger (Logger)
import qualified Logger
import Network.HTTP.Types (Status, status200, status400, status401, status409, status500)
import Network.Wai (Application, Request, isSecure, requestHeaders, responseLBS, strictRequestBody)
import Web.Cookie (SetCookie (..), defaultSetCookie, parseCookies, renderSetCookie, sameSiteLax)

sessionCookieName :: BS.ByteString
sessionCookieName = "session"

authorize :: Connection -> Logger -> String -> LBS.ByteString -> Application
authorize db logger user body req respond =
  try (Database.Auth.createSession db user) >>= \case
    Left (err :: IOException) ->
      Logger.logMsg logger Logger.Warn (T.pack $ show err)
        >> respond (responseLBS status500 [("Content-Type", "text/plain")] "Internal Server Error")
    Right session ->
      respond $
        responseLBS
          status200
          [ ("Content-Type", "text/plain"),
            ( "Set-Cookie",
              LBS.toStrict . toLazyByteString . renderSetCookie $
                defaultSetCookie
                  { setCookieName = sessionCookieName,
                    setCookieValue = BS.pack session,
                    setCookiePath = Just "/",
                    setCookieMaxAge = Just 2592000,
                    setCookieHttpOnly = True,
                    setCookieSecure = isSecure req,
                    setCookieSameSite = Just sameSiteLax
                  }
            )
          ]
          body

data LoginRequest = LoginRequest
  { username :: Text,
    password :: Text
  }
  deriving (Show, Generic)

instance FromJSON LoginRequest

login :: Connection -> Logger -> Application
login db logger req respond =
  strictRequestBody req >>= \b -> case eitherDecode b of
    Left _ ->
      respond $
        responseLBS
          status400
          [("Content-Type", "text/plain")]
          "Bad request"
    Right (LoginRequest u p) ->
      try (Database.Auth.getPasswordHash db $ T.unpack u) >>= \case
        Left (err :: IOException) ->
          Logger.logMsg
            logger
            Logger.Warn
            (T.pack $ show err)
            >> respond (responseLBS status500 [("Content-Type", "text/plain")] "Internal Server Error")
        Right (Just hash) ->
          case checkPassword (mkPassword p) hash of
            PasswordCheckSuccess ->
              authorize db logger (T.unpack u) "Welcome!" req respond
            PasswordCheckFail ->
              respond $
                responseLBS
                  status401
                  [("Content-Type", "text/plain")]
                  "Wrong password"
        Right Nothing ->
          respond $
            responseLBS
              status401
              [("Content-Type", "text/plain")]
              "User does not exist"

data RegisterRequest = RegisterRequest
  { username :: Text,
    password :: Text,
    secret :: Text
  }
  deriving (Show, Generic)

instance FromJSON RegisterRequest

register :: Connection -> PasswordHash Argon2 -> Logger -> Application
register db appSecret logger req respond =
  strictRequestBody req >>= \b -> case eitherDecode b of
    Left _ ->
      respond $
        responseLBS
          status400
          [("Content-Type", "text/plain")]
          "Bad request"
    Right (RegisterRequest u p s) ->
      case checkPassword (mkPassword s) appSecret of
        PasswordCheckSuccess ->
          try (Database.Auth.register db (T.unpack u) (T.unpack p)) >>= \case
            Left (err :: IOException) ->
              Logger.logMsg
                logger
                Logger.Warn
                ( T.pack $
                    show err
                )
                >> respond (responseLBS status500 [("Content-Type", "text/plain")] "Internal Server Error")
            Right True ->
              authorize db logger (T.unpack u) "Created" req respond
            Right False ->
              respond $
                responseLBS
                  status409
                  [("Content-Type", "text/plain")]
                  "User already exists"
        PasswordCheckFail ->
          respond $
            responseLBS
              status401
              [("Content-Type", "text/plain")]
              "Wrong secret"

authenticate :: Connection -> Request -> IO (Either (Status, String) String)
authenticate db req =
  case lookup sessionCookieName cookies of
    Just session ->
      Database.Auth.validateSession db (BS.unpack session) >>= \case
        Just user -> pure $ Right user
        Nothing -> pure $ Left (status401, "Invalid session")
    Nothing -> pure $ Left (status401, "Missing session cookie")
  where
    cookies = concatMap (parseCookies . snd) $ filter ((== "Cookie") . fst) (requestHeaders req)

validate :: Connection -> Logger -> Application
validate db logger req respond =
  try (authenticate db req) >>= \case
    Left (err :: IOException) ->
      Logger.logMsg
        logger
        Logger.Warn
        ( T.pack $
            show err
        )
        >> respond (responseLBS status500 [("Content-Type", "text/plain")] "Internal Server Error")
    Right (Left (status, err)) ->
      respond $ responseLBS status [("Content-Type", "text/plain")] $ LBS.pack err
    Right (Right user) ->
      respond $
        responseLBS
          status200
          [("Content-Type", "text/plain")]
          (LBS.fromStrict $ TE.encodeUtf8 $ T.pack user)
