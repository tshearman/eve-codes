{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module UniverseEndpoints where

import qualified Data.Text as T
import EndpointsLib
import Esi

universePath :: String -> EsiVersion -> EsiSource -> Endpoint
universePath end ver source = Endpoint p o
  where
    p = esi ./ show ver ./ "universe" ./ end
    o = genOptions params
    params = [Param "datasource" [T.pack $ show source]]

systemJumpsPath :: EsiVersion -> EsiSource -> Endpoint
systemJumpsPath = universePath "system_jumps"

systemKillsPath :: EsiVersion -> EsiSource -> Endpoint
systemKillsPath = universePath "system_kills"

regionsPath :: EsiVersion -> EsiSource -> Endpoint
regionsPath = universePath "regions"

systemsPath :: EsiVersion -> EsiSource -> Endpoint
systemsPath = universePath "systems"

universeSubPath :: Show a => String -> a -> EsiVersion -> EsiSource -> Endpoint
universeSubPath k v = universePath (k ++ "/" ++ show v)

systemPath :: SystemId -> EsiVersion -> EsiSource -> Endpoint
systemPath = universeSubPath "systems"

stargatePath :: Integer -> EsiVersion -> EsiSource -> Endpoint
stargatePath = universeSubPath "stargates/"
