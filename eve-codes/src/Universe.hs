{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Universe where

import Data.Aeson
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Database.PostgreSQL.Simple.ToField (toField)
import Database.PostgreSQL.Simple.ToRow (ToRow)
import Endpoints
import GHC.Generics (Generic)
import qualified Data.Text as T

type SystemId = Integer

data EsiSource = Tranquility | Singularity
instance Show EsiSource where
  show Singularity = "singularity"
  show Tranquility = "tranquility"

data EsiVersion = Latest
instance Show EsiVersion where
  show Latest = "latest"

esi :: String
esi = "https://esi.evetech.net"

eveDateFormat :: DateFormat
eveDateFormat = "%a, %d %b %Y %T %Z"

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
instance Collectible SystemJumps
instance DatedSqlible SystemJumps where
  query = "INSERT INTO universe.\"systemJumps\" (time, systemid, shipjumps) VALUES (?, ?, ?) ON CONFLICT DO NOTHING"
  toValue ts (SystemJumps j i) = [toField $ posixSecondsToUTCTime ts, toField i, toField j]
systemJumpsPath :: EsiVersion -> EsiSource -> Endpoint
systemJumpsPath = universePath "system_jumps"

data SystemKills = SystemKills
  { npc_kills :: Integer,
    pod_kills :: Integer,
    ship_kills :: Integer,
    system_id :: SystemId
  } deriving (Show, Generic, ToRow)
instance FromJSON SystemKills
instance Collectible SystemKills
instance DatedSqlible SystemKills where
  query = "INSERT INTO universe.\"systemKills\" (time, systemid, npckills, podkills, shipkills) VALUES (?, ?, ?, ?, ?) ON CONFLICT DO NOTHING"
  toValue ts (SystemKills n p s i) = [toField $ posixSecondsToUTCTime ts, toField i, toField n, toField p, toField s]
systemKillsPath :: EsiVersion -> EsiSource -> Endpoint
systemKillsPath = universePath "system_kills"
