{-# LANGUAGE TypeSynonymInstances #-}

module Esi where

import EndpointsLib

type RegionId = Integer

type SystemId = Integer

type LocationId = Integer

type TypeId = Integer

instance Processable RegionId

instance Collectible RegionId

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

postgresDateFormat :: DateFormat
postgresDateFormat = "%Y-%m-%d %T%z"
