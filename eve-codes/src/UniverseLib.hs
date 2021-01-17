{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module UniverseLib where

import Data.Aeson
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Database.PostgreSQL.Simple.ToField (toField)
import Database.PostgreSQL.Simple.ToRow (ToRow)
import Endpoints
import GHC.Generics (Generic)
import qualified Data.Text as T
import Esi

universePath :: String -> EsiVersion -> EsiSource -> Endpoint
universePath end ver source = Endpoint p o
  where p = esi ./ show ver ./ "universe" ./ end
        o = genOptions params
        params = [Param "datasource" [T.pack $ show source]]

data SystemJumps = SystemJumps
  { ship_jumps :: Integer,
    system_id :: SystemId
  } deriving (Show, Generic, ToRow)
instance FromJSON SystemJumps
instance Processable SystemJumps
instance Collectible SystemJumps
instance Sqlible SystemJumps where
  query = "INSERT INTO universe.\"systemJumps\" (time, systemid, shipjumps) VALUES (?, ?, ?) ON CONFLICT DO NOTHING"
instance TimeToValue SystemJumps where
  timeToValue ts (SystemJumps j i) = [toField $ posixSecondsToUTCTime ts, toField i, toField j]
systemJumpsPath :: EsiVersion -> EsiSource -> Endpoint
systemJumpsPath = universePath "system_jumps"

data SystemKills = SystemKills
  { npc_kills :: Integer,
    pod_kills :: Integer,
    ship_kills :: Integer,
    system_id :: SystemId
  } deriving (Show, Generic, ToRow)
instance FromJSON SystemKills
instance Processable SystemKills
instance Collectible SystemKills
instance Sqlible SystemKills where
  query = "INSERT INTO universe.\"systemKills\" (time, systemid, npckills, podkills, shipkills) VALUES (?, ?, ?, ?, ?) ON CONFLICT DO NOTHING"
instance TimeToValue SystemKills where
  timeToValue ts (SystemKills n p s i) = [toField $ posixSecondsToUTCTime ts, toField i, toField n, toField p, toField s]
systemKillsPath :: EsiVersion -> EsiSource -> Endpoint
systemKillsPath = universePath "system_kills"
