module Main (main) where

import qualified Cli
import Data.Password.Argon2
  ( Argon2,
    PasswordHash (..),
    extractParams,
    hashPassword,
    mkPassword,
  )
import qualified Data.Text as T
import Http (serve, serveTLS)
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main =
  getArgs >>= \a -> case Cli.parse a of
    Left e -> hPutStrLn stderr $ "heft: parse error: " ++ e
    Right c ->
      if Cli.help c
        then
          hPutStrLn stderr Cli.helpMenu
        else
          if Cli.version c
            then
              hPutStrLn stderr Cli.versionMenu
            else case Cli.hash c of
              Just s ->
                hash s >>= \p ->
                  hPutStrLn stderr $ T.unpack $ unPasswordHash p
              Nothing ->
                ( case Cli.secret c of
                    Just p ->
                      let h = PasswordHash (T.pack p) :: PasswordHash Argon2
                       in case extractParams h of
                            Just _ -> pure h
                            Nothing -> ioError $ userError "Invalid Argon2 hash"
                    Nothing -> hash "Please!"
                )
                  >>= \x ->
                    if Cli.https c
                      then case (Cli.cert c, Cli.key c) of
                        (Just cert, Just key) -> serveTLS (Cli.addr c) (Cli.db c) x cert key
                        _ -> hPutStrLn stderr "heft: HTTPS requires a cert and key path set"
                      else
                        serve (Cli.addr c) (Cli.db c) x
  where
    hash s = ((hashPassword . mkPassword . T.pack) s)
