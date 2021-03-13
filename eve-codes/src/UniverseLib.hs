{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module UniverseLib where

import qualified Database.PostgreSQL.Simple as PG
import EndpointsLib
import Esi
import UniverseEndpoints
import UniverseTypes

data Sources = Jumps | Kills

sourcesFromString :: String -> Sources
sourcesFromString "jumps" = Jumps
sourcesFromString _ = Kills

collectSystemJumps :: EsiVersion -> EsiSource -> IO (Header, [SystemJumps])
collectSystemJumps ver source = do
  collect (systemJumpsPath ver source) :: IO (Header, [SystemJumps])

collectSystemKills :: EsiVersion -> EsiSource -> IO (Header, [SystemKills])
collectSystemKills ver source = do
  collect (systemKillsPath ver source) :: IO (Header, [SystemKills])

collectRegions :: EsiVersion -> EsiSource -> IO (Header, [RegionId])
collectRegions ver source = do
  collect (regionsPath ver source) :: IO (Header, [RegionId])

collectSystems :: EsiVersion -> EsiSource -> IO (Header, [SystemId])
collectSystems ver source = do
  collect (systemsPath ver source) :: IO (Header, [SystemId])

collectSystem :: EsiVersion -> EsiSource -> SystemId -> IO (Header, Maybe System)
collectSystem ver source s = do
  collectInd (systemPath s ver source) :: IO (Header, Maybe System)

collectStargate :: EsiVersion -> EsiSource -> Integer -> IO (Header, [Stargate])
collectStargate ver source s = do
  collect (stargatePath s ver source) :: IO (Header, [Stargate])

collected :: Sources -> PG.Connection -> IO ()
collected Jumps conn = do
  (header, data_) <- collectSystemJumps Latest Tranquility
  rowsInserted <- PG.executeMany conn (query @SystemJumps) (map (timeToValue (getLastModified eveDateFormat header)) data_)
  print rowsInserted
collected Kills conn = do
  (header, data_) <- collectSystemKills Latest Tranquility
  rowsInserted <- PG.executeMany conn (query @SystemKills) (map (timeToValue (getLastModified eveDateFormat header)) data_)
  print rowsInserted
