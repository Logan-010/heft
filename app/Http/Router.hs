{-# LANGUAGE OverloadedStrings #-}

module Http.Router (root) where

import Database.SQLite.Simple (Connection)
import Logger (Logger)
import Network.HTTP.Types (status404)
import Network.HTTP.Types.Status (status200)
import Network.Wai (Application, Request (pathInfo, requestMethod), responseLBS)

root :: Connection -> Logger -> Application
root db logger req respond = case (requestMethod req, pathInfo req) of
  ("GET", ["api", "health"]) ->
    respond $
      responseLBS
        status200
        [("Content-Type", "text/plain")]
        "OK"
  _ ->
    respond $
      responseLBS
        status404
        [("Content-Type", "text/plain")]
        "Not found"
