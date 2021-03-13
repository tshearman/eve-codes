{-# LANGUAGE OverloadedStrings #-}

module Orders where

import Connection
import Control.Concurrent.Async
import Esi
import Options.Applicative ((<**>))
import qualified Options.Applicative as Opt
import OrdersEndpoints
import OrdersLib
import UniverseLib (collectRegions)

data Options = Options
  { host :: String,
    dbname :: String,
    user :: String,
    region :: Integer,
    order_types :: String
  }

orderTypesFromString :: String -> OrderTypes
orderTypesFromString "buy" = Buy
orderTypesFromString "sell" = Sell
orderTypesFromString _ = All

parse :: Opt.Parser Options
parse =
  Options
    <$> Opt.strOption (Opt.long "host" <> Opt.help "POSTGRES Hostname")
    <*> Opt.strOption (Opt.long "dbname" <> Opt.help "POSTGRES Database")
    <*> Opt.strOption (Opt.long "user" <> Opt.help "POSTGRES User name")
    <*> Opt.option Opt.auto (Opt.long "region" <> Opt.help "Eve Region id")
    <*> Opt.strOption (Opt.long "types" <> Opt.help "Order types to collect")

execute :: Options -> IO ()
execute (Options host_ db_ user_ region_ order_types_) = do
  (_, regions) <- collectRegions Latest Tranquility
  conn <- Connection.open host_ user_ db_
  _ <- mapConcurrently (\r -> collectedOrders r orderType conn) (regionFilter regions)
  putStrLn "completed"
  where
    orderType = orderTypesFromString order_types_
    regionFilter = filteredRegions region_

main :: IO ()
main = execute =<< Opt.execParser options
  where
    options =
      Opt.info
        (parse <**> Opt.helper)
        ( Opt.fullDesc
            <> Opt.progDesc "Collect Eve Markets data from ESI"
        )
