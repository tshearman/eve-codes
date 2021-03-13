{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module OrdersLib where

import Control.Concurrent.Async
import qualified Database.PostgreSQL.Simple as PG
import Database.PostgreSQL.Simple.ToField (Action)
import EndpointsLib
import Esi
import OrdersEndpoints
import OrdersTypes

collectOrderPage :: RegionId -> OrderTypes -> Int -> IO (Header, [MarketOrder])
collectOrderPage region_ orders page = do
  (h, r) <- collect (marketsRegionsOrdersPath Latest Tranquility region_ orders page) :: IO (Header, [MarketOrder])
  return (h, r)

parseOrder :: TimeRegionToValue a => Header -> RegionId -> a -> [Action]
parseOrder header = timeRegionToValue (getLastModified eveDateFormat header)

collectedOrders :: RegionId -> OrderTypes -> PG.Connection -> IO ()
collectedOrders region_ orders conn = do
  (header, data_) <- collectOrderPage region_ orders 1
  initialRowsInserted <- PG.executeMany conn (query @MarketOrder) $ map (parseOrder header region_) data_
  res <- mapConcurrently (collectOrderPage region_ orders) [2 .. (getPages header)]
  rowsInserted <- PG.executeMany conn (query @MarketOrder) $ map (parseOrder header region_) (concatMap snd res)
  print (initialRowsInserted + rowsInserted)

filteredRegions :: Integer -> [RegionId] -> [RegionId]
filteredRegions region_ regions = if region_ < 0 then filter notMarketRegions regions else [region_]

marketRegions :: [RegionId]
marketRegions = [10000002, 10000030, 10000032, 10000042, 10000043]

notMarketRegions :: RegionId -> Bool
notMarketRegions i = i `notElem` marketRegions
