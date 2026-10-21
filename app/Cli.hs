module Cli (Cli (..), new, parse, helpMenu, versionMenu) where

data Cli = Cli
  { addr :: String,
    db :: String,
    secret :: Maybe String,
    https :: Bool,
    cert :: Maybe String,
    key :: Maybe String,
    help :: Bool,
    version :: Bool,
    hash:: Maybe String
  }
  deriving (Show, Eq)

helpMenu :: String
helpMenu =
  unlines
    [ "heft: A pure personal financial tracking service",
      "",
      "Usage: heft[OPTIONS]",
      "",
      "Options:",
      "  -x, --hash SECRET     Produce the hash string of a secret",
      "  -s, --secret HASH     Hash of the login question",
      "  -a, --addr ADDR       Address to listen on (default: [::]:7766)",
      "  -d, --database PATH   Database path (default: data.db)",
      "  -H, --https           Enable HTTPS (requires cert and key)",
      "  -c, --cert PATH       TLS certificate path",
      "  -k, --key PATH        TLS private key path",
      "  -h, --help            Show this help menu",
      "  -v, --version         Show version information"
    ]

versionMenu :: String
versionMenu = "heft 0.1.0.0"

new :: Cli
new = Cli "[::]:7766" "data.db" Nothing False Nothing Nothing False False Nothing

data Pending = Addr | Db | Cert | Key | Secret | Hash

parse' :: [String] -> Maybe Pending -> Cli -> Either String Cli
parse' args pending cli = case pending of
  Just Addr -> case args of
    [] -> Left "missing addr"
    h : r -> parse' r Nothing $ cli {addr = h}
  Just Db -> case args of
    [] -> Left "missing database path"
    h : r -> parse' r Nothing $ cli {db = h}
  Just Cert -> case args of
    [] -> Left "missing certificate path"
    h : r -> parse' r Nothing $ cli {cert = Just h}
  Just Key -> case args of
    [] -> Left "missing key path"
    h : r -> parse' r Nothing $ cli {key = Just h}
  Just Secret -> case args of
    [] -> Left "missing secret hash"
    h : r -> parse' r Nothing $ cli {secret = Just h}
  Just Hash -> case args of
    [] -> Left "missing secret"
    h : r -> parse' r Nothing $ cli {hash = Just h}
  Nothing -> case args of
    [] -> Right cli
    h : r
      | h `elem` ["-x", "--hash"] -> parse' r (Just Hash) cli
      | h `elem` ["-s", "--secret"] -> parse' r (Just Secret) cli
      | h `elem` ["-a", "--addr"] -> parse' r (Just Addr) cli
      | h `elem` ["-d", "--database"] -> parse' r (Just Db) cli
      | h `elem` ["-H", "--https"] -> parse' r Nothing $ cli {https = True}
      | h `elem` ["-c", "--cert"] -> parse' r (Just Cert) cli
      | h `elem` ["-k", "--key"] -> parse' r (Just Key) cli
      | h `elem` ["-h", "--help"] -> parse' r Nothing $ cli {help = True}
      | h `elem` ["-v", "--version"] -> parse' r Nothing $ cli {version = True}
      | otherwise -> Left $ "unknown arg/flag " ++ h

parse :: [String] -> Either String Cli
parse args = parse' (expandArgs args) Nothing new
  where
    expandArgs =
      concat
        . map
          ( \a -> case a of
              '-' : '-' : r -> ["--" ++ r]
              '-' : f -> map (\s -> "-" ++ [s]) f
              e -> [e]
          )
