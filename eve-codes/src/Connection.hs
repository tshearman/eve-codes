module Connection where

import qualified Database.PostgreSQL.Simple as PG
import qualified Data.ByteString.Char8 as BC
import Control.Retry

connectionString :: String -> String -> String -> String -> String
connectionString h user pass db = "host=" ++ h ++ " user=" ++ user ++ " password=\"" ++ pass ++"\" dbname=" ++ db

open :: String -> String -> String -> String -> IO PG.Connection
open host user pass db = PG.connectPostgreSQL $ BC.pack (connectionString host user pass db)

retryPolicy :: RetryPolicy
retryPolicy = exponentialBackoff 200000 <> limitRetries 5
