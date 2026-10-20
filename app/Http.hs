{-# LANGUAGE OverloadedStrings #-}

module Http (serve, serveTLS) where

import Data.String (fromString)
import qualified Data.Text as T
import qualified Database
import qualified Http.Router
import Logger (Level (Info), logMsg, withLogger)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setHost, setPort)
import Network.Wai.Handler.WarpTLS (runTLS, tlsSettings)
import Network.Wai.Middleware.RequestLogger (logStdout)
import System.IO (hPutStrLn, stderr)

splitHostPort :: String -> Maybe (String, Int)
splitHostPort s = case s of
  '[' : rest -> case break (== ']') rest of
    (host, ']' : ':' : port) -> parsePort host port
    _ -> Nothing
  _ -> case break (== ':') s of
    (host, ':' : port) -> parsePort host port
    _ -> Nothing
  where
    parsePort host port
      | null host || any (`elem` ['[', ']']) host = Nothing
      | null port || any (`notElem` ['0' .. '9']) port = Nothing
      | otherwise = case reads port of
          [(n, "")] | n >= 0 && n <= 65535 -> Just (host, n)
          _ -> Nothing

serve :: String -> String -> IO ()
serve addr db =
  case splitHostPort addr of
    Just (h, p) ->
      withLogger $ \logger -> do
        let s =
              setPort p
                . setHost (fromString h)
                $ defaultSettings

        logMsg logger Info $
          "Listening on http://" <> T.pack addr

        conn <- Database.openDb db

        logMsg logger Info "Database initialized"

        runSettings s $
          logStdout $
            Http.Router.root conn logger
    Nothing ->
      hPutStrLn stderr "heft: invalid address"

serveTLS :: String -> String -> String -> String -> IO ()
serveTLS addr db cert key =
  case splitHostPort addr of
    Just (h, p) ->
      withLogger $ \logger -> do
        let s = ((setPort p) . (setHost $ fromString h)) defaultSettings

        logMsg logger Info $
          "Listening on https://" <> T.pack addr

        conn <- Database.openDb db

        logMsg logger Info "Database initialized"

        runTLS (tlsSettings cert key) s $
          logStdout $
            Http.Router.root conn logger
    Nothing ->
      hPutStrLn stderr "heft: invalid address"
