{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module OrderBooks where

import Esi

data Options = Options {
  host :: String,
  dbname :: String,
  user :: String
}

theQuery :: String -> RegionId -> String
theQuery startDate region = unlines [
  "INSERT INTO markets.order_books",
  "SELECT",
  "    time,",
  "    region_id,",
  "    system_id,",
  "    location_id,",
  "    type_id,",
  "    is_buy_order,",
  "    ARRAY_AGG(price) AS prices,",
  "    ARRAY_AGG(volume_remain) AS volume ",
  "FROM ",
  "    (SELECT  ",
  "        time,",
  "        region_id,",
  "        system_id,",
  "        location_id,",
  "        type_id,",
  "        is_buy_order,",
  "        price,",
  "        SUM(volume_remain) AS volume_remain  ",
  "    FROM ",
  "        markets.orders ",
  "    WHERE  ",
  "        region_id = " ++ show region,
  "        AND time >= '" ++ startDate ++ "' ",
  "    GROUP BY ",
  "        time,",
  "        region_id,",
  "        system_id,",
  "        location_id,",
  "        type_id,",
  "        is_buy_order,",
  "        price) AS a  ",
  "GROUP BY ",
  "    time,",
  "    region_id,",
  "    system_id,",
  "    type_id,",
  "    location_id,",
  "    is_buy_order",
  "ON CONFLICT DO NOTHING"]

main :: IO ()
main = putStrLn (theQuery "2021-01-17" 101)
