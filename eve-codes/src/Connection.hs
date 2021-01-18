module Connection where

import qualified Database.PostgreSQL.Simple as PG
import qualified Data.ByteString.Char8 as BC
import qualified Control.Concurrent.PooledIO.Final as Pool
import Control.DeepSeq (NFData)

connectionString :: String -> String -> String -> String
connectionString h user db = "host=" ++ h ++ " user=" ++ user ++ " dbname=" ++ db

open :: String -> String -> String -> IO PG.Connection
open host user db = PG.connectPostgreSQL $ BC.pack (connectionString host user db)

mapPool ::
   (Traversable t, NFData b) =>
   Int -> (a -> IO b) -> t a -> IO (t b)
mapPool n f = Pool.runLimited n . traverse (Pool.fork . f)
