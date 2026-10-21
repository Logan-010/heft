{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Http.Router (root) where

import qualified Data.ByteString.Lazy as LBS
import Data.FileEmbed (embedFileRelative)
import Data.Password.Argon2 (Argon2, PasswordHash)
import Database.SQLite.Simple (Connection)
import qualified Http.Auth
import Logger (Logger)
import Network.HTTP.Types (status200, status404)
import Network.Wai
  ( Application,
    Request (pathInfo, requestMethod),
    responseLBS,
  )

indexHtml :: LBS.ByteString
indexHtml =
  LBS.fromStrict $(embedFileRelative "frontend/dist/index.html")

elmJs :: LBS.ByteString
elmJs =
  LBS.fromStrict $(embedFileRelative "frontend/dist/elm.js")

styleCss :: LBS.ByteString
styleCss =
  LBS.fromStrict $(embedFileRelative "frontend/dist/style.css")

iconPng :: LBS.ByteString
iconPng =
  LBS.fromStrict $(embedFileRelative "frontend/dist/icon.png")

root :: Connection -> PasswordHash Argon2 -> Logger -> Application
root db secret logger req respond =
  case (requestMethod req, pathInfo req) of
    -- API routes
    ("GET", ["api", "health"]) ->
      respond $
        responseLBS
          status200
          [("Content-Type", "text/plain")]
          "OK"
    ("POST", ["api", "auth", "login"]) ->
      Http.Auth.login db logger req respond
    ("POST", ["api", "auth", "register"]) ->
      Http.Auth.register db secret logger req respond
    ("GET", ["api", "auth", "validate"]) ->
      Http.Auth.validate db logger req respond
    -- Frontend routes
    ("GET", ["style.css"]) ->
      respond $
        responseLBS
          status200
          [("Content-Type", "text/css; charset=utf-8")]
          styleCss
    ("GET", ["icon.png"]) ->
      respond $
        responseLBS
          status200
          [("Content-Type", "image/png")]
          iconPng
    ("GET", ["elm.js"]) ->
      respond $
        responseLBS
          status200
          [("Content-Type", "application/javascript")]
          elmJs
    -- API 404
    (_, "api" : _) -> respond404
    ("GET", _) ->
      respond $
        responseLBS
          status200
          [("Content-Type", "text/html; charset=utf-8")]
          indexHtml
    -- 404
    _ -> respond404
  where
    respond404 =
      respond $
        responseLBS
          status404
          [("Content-Type", "text/plain")]
          "Not found"
