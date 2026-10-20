module Http.Router (root) where

import Database.SQLite.Simple (Connection)
import Network.Wai (Application)

root :: Connection -> Application
root db = error "todo"
