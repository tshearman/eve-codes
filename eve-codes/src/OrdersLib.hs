{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module OrdersLib where

import Data.Aeson
import Data.Time.Clock.POSIX (POSIXTime, posixSecondsToUTCTime)
import Database.PostgreSQL.Simple.ToField (toField, Action)
import Database.PostgreSQL.Simple.ToRow (ToRow)
import Endpoints
import Esi
import GHC.Generics (Generic)
import qualified Data.Text as T

data OrderTypes = All | Buy | Sell
instance Show OrderTypes where
  show All = "all"
  show Buy = "buy"
  show Sell = "sell"

class TimeRegionToValue a where
  timeRegionToValue :: POSIXTime -> RegionId -> a -> [Action]

data MarketOrder = MarketOrder
  { duration :: Integer,
    is_buy_order :: Bool,
    issued :: String,
    location_id :: LocationId,
    min_volume :: Integer,
    order_id :: Integer,
    price :: Float,
    range :: String,
    system_id :: SystemId,
    type_id :: TypeId,
    volume_remain :: Integer,
    volume_total :: Integer
  } deriving (Show, Generic, ToRow)
instance FromJSON MarketOrder
instance Processable MarketOrder
instance Collectible MarketOrder
instance Sqlible MarketOrder where
  query = "INSERT INTO markets.orders (time, region_id, duration, is_buy_order, issued, location_id, min_volume, order_id, price, range, system_id, type_id, volume_remain, volume_total) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?) ON CONFLICT DO NOTHING"
instance TimeRegionToValue MarketOrder where
  timeRegionToValue ts regionId (MarketOrder duration_ is_buy_order_ issued_ location_id_ min_volume_ order_id_ price_ range_ system_id_ type_id_ volume_remain_ volume_total_) = [
      toField $ posixSecondsToUTCTime ts,
      toField regionId,
      toField duration_,
      toField is_buy_order_,
      toField issued_,
      toField location_id_,
      toField min_volume_,
      toField order_id_,
      toField price_,
      toField range_,
      toField system_id_,
      toField type_id_,
      toField volume_remain_,
      toField volume_total_]
marketsRegionsOrdersPath :: EsiVersion -> EsiSource -> RegionId -> OrderTypes -> Int -> Endpoint
marketsRegionsOrdersPath ver source region orders page = Endpoint p o
  where p = esi ./ show ver ./ "markets" ./ show region ./ "orders"
        o = genOptions params
        params = [Param "datasource" [T.pack $ show source],
                  Param "order_type" [T.pack $ show orders],
                  Param "page" [T.pack $ show page]]
