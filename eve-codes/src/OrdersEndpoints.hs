{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module OrdersEndpoints where

import qualified Data.Text as T
import EndpointsLib
import Esi

data OrderTypes = All | Buy | Sell

instance Show OrderTypes where
  show All = "all"
  show Buy = "buy"
  show Sell = "sell"

marketsRegionsOrdersPath :: EsiVersion -> EsiSource -> RegionId -> OrderTypes -> Int -> Endpoint
marketsRegionsOrdersPath ver source region orders page = Endpoint p o
  where
    p = esi ./ show ver ./ "markets" ./ show region ./ "orders"
    o = genOptions params
    params =
      [ Param "datasource" [T.pack $ show source],
        Param "order_type" [T.pack $ show orders],
        Param "page" [T.pack $ show page]
      ]
