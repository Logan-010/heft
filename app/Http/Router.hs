{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Http.Router (root) where

import qualified Data.ByteString.Lazy as LBS
import Data.FileEmbed (embedFileRelative)
import Database.SQLite.Simple (Connection)
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

root :: Connection -> Logger -> Application
root db logger req respond =
  case (requestMethod req, pathInfo req) of
    -- API routes
    ("GET", ["api", "health"]) ->
      respond $
        responseLBS
          status200
          [("Content-Type", "text/plain")]
          "OK"
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
    ("GET", _) ->
      respond $
        responseLBS
          status200
          [("Content-Type", "text/html; charset=utf-8")]
          indexHtml
    -- 404s
    (_, "api" : _) ->
      notFound
    _ ->
      notFound
  where
    notFound =
      respond $
        responseLBS
          status404
          [("Content-Type", "text/plain")]
          "Not found"
