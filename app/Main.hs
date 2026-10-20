module Main (main) where

import qualified Cli
import Http.Serve (serve, serveTLS)
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
            else
              if Cli.https c
                then case (Cli.cert c, Cli.key c) of
                  (Just cert, Just key) -> serveTLS (Cli.addr c) (Cli.db c) cert key
                  _ -> hPutStrLn stderr "heft: HTTPS requires a cert and key path set"
                else
                  serve (Cli.addr c) (Cli.db c)
