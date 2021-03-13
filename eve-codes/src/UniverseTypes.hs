{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module UniverseTypes where

import Data.Aeson
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Database.PostgreSQL.Simple.ToField (toField)
import Database.PostgreSQL.Simple.ToRow (ToRow)
import EndpointsLib
import Esi
import GHC.Generics (Generic)

data SystemJumps = SystemJumps
  { ship_jumps :: Integer,
    system_id :: SystemId
  }
  deriving (Show, Generic, ToRow)

instance FromJSON SystemJumps

instance Processable SystemJumps

instance Collectible SystemJumps

instance Sqlible SystemJumps where
  query = "INSERT INTO universe.\"systemJumps\" (time, systemid, shipjumps) VALUES (?, ?, ?) ON CONFLICT DO NOTHING"

instance TimeToValue SystemJumps where
  timeToValue ts (SystemJumps j i) = [toField $ posixSecondsToUTCTime ts, toField i, toField j]

data SystemKills = SystemKills
  { npc_kills :: Integer,
    pod_kills :: Integer,
    ship_kills :: Integer,
    system_id :: SystemId
  }
  deriving (Show, Generic, ToRow)

instance FromJSON SystemKills

instance Processable SystemKills

instance Collectible SystemKills

instance Sqlible SystemKills where
  query = "INSERT INTO universe.\"systemKills\" (time, systemid, npckills, podkills, shipkills) VALUES (?, ?, ?, ?, ?) ON CONFLICT DO NOTHING"

instance TimeToValue SystemKills where
  timeToValue ts (SystemKills n p s i) = [toField $ posixSecondsToUTCTime ts, toField i, toField n, toField p, toField s]

data System = System
  { constellation_id :: Integer,
    name :: String,
    planets :: Maybe [Planet],
    position :: Position,
    security_class :: Maybe String,
    security_status :: Double,
    star_id :: Maybe Integer,
    stargates :: Maybe [Integer],
    stations :: Maybe [Integer],
    system_id :: SystemId
  }
  deriving (Show, Generic)

instance FromJSON System

instance ProcessableInd System

instance CollectibleInd System

data Position = Position
  { x :: Double,
    y :: Double,
    z :: Double
  }
  deriving (Show, Generic)

instance FromJSON Position

data Planet = Planet
  { asteroid_belts :: Maybe [Integer],
    moons :: Maybe [Integer],
    planet_id :: Integer
  }
  deriving (Show, Generic)

instance FromJSON Planet

data Stargate = Stargate
  { destination :: Destination,
    name :: String,
    position :: Position,
    stargate_id :: Integer,
    system_id :: Integer,
    type_id :: Integer
  }
  deriving (Show, Generic)

instance FromJSON Stargate

instance Collectible Stargate

instance Processable Stargate

data Destination = Destination
  { stargate_id :: Integer,
    system_id :: SystemId
  }
  deriving (Show, Generic)

instance FromJSON Destination
