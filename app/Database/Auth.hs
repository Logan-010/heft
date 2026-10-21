{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Auth (validateSession, createSession, getPasswordHash, register) where

import Crypto.Random (getRandomBytes)
import qualified Data.ByteString as BS
import Data.Maybe (listToMaybe)
import Data.Password.Argon2 (Argon2, PasswordHash (..), hashPassword, mkPassword)
import qualified Data.Text as T
import Database.SQLite.Simple (Connection, Only (..), query)
import Numeric (showHex)

validateSession :: Connection -> String -> IO (Maybe String)
validateSession db session =
  fmap fromOnly . listToMaybe
    <$> query
      db
      "SELECT id FROM sessions WHERE id = ? AND expires_at > unixepoch()"
      (Only session)

createSession :: Connection -> String -> IO String
createSession db username =
  (getRandomBytes 32 :: IO BS.ByteString) >>= \bytes ->
    let session = concatMap (\byte -> let hex = showHex byte "" in replicate (2 - length hex) '0' ++ hex) (BS.unpack bytes)
     in query
          db
          "INSERT INTO sessions (id, user_id, expires_at) \
          \SELECT ?, id, unixepoch() + 2592000 FROM users WHERE username = ? \
          \RETURNING id"
          (session, username)
          >>= \case
            [Only token] -> pure token
            _ -> ioError $ userError "Cannot create a session for a nonexistent user"

getPasswordHash :: Connection -> String -> IO (Maybe (PasswordHash Argon2))
getPasswordHash db username =
  fmap (PasswordHash . fromOnly) . listToMaybe
    <$> query
      db
      "SELECT password_hash FROM users WHERE username = ?"
      (Only username)

register :: Connection -> String -> String -> IO Bool
register db username password =
  hashPassword (mkPassword $ T.pack password) >>= \(PasswordHash hash) ->
    not . null
      <$> ( query
              db
              "INSERT INTO users (username, password_hash) VALUES (?, ?) \
              \ON CONFLICT(username) DO NOTHING RETURNING id"
              (username, hash) ::
              IO [Only Int]
          )
