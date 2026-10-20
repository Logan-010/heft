{-# LANGUAGE OverloadedStrings #-}

module Http.Serve (serve, serveTLS) where

import Data.String (fromString)
import Database.SQLite.Simple (Connection)
import qualified Http.Router
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setHost, setPort)
import Network.Wai.Handler.WarpTLS (runTLS, tlsSettings)
import System.IO (hPutStrLn, stderr)

openDb :: String -> IO Connection
openDb path = error "todo"

splitHostPort :: String -> Maybe (String, Int)
splitHostPort s =
  case break (== ':') (reverse s) of
    (port, ':' : host) ->
      case reads (reverse port) of
        [(n, "")] | n >= 0 && n <= 65535 -> Just (reverse host, n)
        _ -> Nothing
    _ -> Nothing

serve :: String -> String -> IO ()
serve addr db =
  case splitHostPort addr of
    Just (h, p) ->
      let s = ((setPort p) . (setHost $ fromString h)) defaultSettings
       in openDb db >>= \d -> runSettings s $ Http.Router.root d
    Nothing -> hPutStrLn stderr "heft: invalid address"

serveTLS :: String -> String -> String -> String -> IO ()
serveTLS addr db cert key =
  case splitHostPort addr of
    Just (h, p) ->
      let s = ((setPort p) . (setHost $ fromString h)) defaultSettings
       in openDb db >>= \d -> runTLS (tlsSettings cert key) s $ Http.Router.root d
    Nothing -> hPutStrLn stderr "heft: invalid address"
