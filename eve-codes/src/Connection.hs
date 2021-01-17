module Connection where

import qualified Database.PostgreSQL.Simple as PG
import qualified Data.ByteString.Char8 as BC  
  
connectionString :: String -> String -> String -> String
connectionString h user db = "host=" ++ h ++ " user=" ++ user ++ " dbname=" ++ db

open :: String -> String -> String -> IO PG.Connection
open host user db = PG.connectPostgreSQL $ BC.pack (connectionString host user db)
