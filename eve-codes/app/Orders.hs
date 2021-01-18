{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module Orders where

import Endpoints
import Esi
import OrdersLib
import qualified Database.PostgreSQL.Simple as PG
import Database.PostgreSQL.Simple.ToField (Action)
import qualified Options.Applicative as Opt
import Options.Applicative ((<**>))
import Connection
import Control.Concurrent.Async

data Options = Options {
  host :: String,
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
parse = Options 
  <$> Opt.strOption ( Opt.long "host" <> Opt.help "POSTGRES Hostname" )
  <*> Opt.strOption ( Opt.long "dbname" <> Opt.help "POSTGRES Database" )
  <*> Opt.strOption ( Opt.long "user" <> Opt.help "POSTGRES User name" ) 
  <*> Opt.option Opt.auto ( Opt.long "region" <> Opt.help "Eve Region id" )
  <*> Opt.strOption ( Opt.long "types" <> Opt.help "Order types to collect" )

collectThePage :: RegionId -> OrderTypes -> Int -> IO (Header, [MarketOrder])
collectThePage region_ orders page = do
  (h, r) <- collect (marketsRegionsOrdersPath Latest Tranquility region_ orders page) :: IO (Header, [MarketOrder])
  return (h, r)

parseOrder :: TimeRegionToValue a => Header -> RegionId -> a -> [Action]
parseOrder header = timeRegionToValue (getLastModified eveDateFormat header)

collectedOrders :: RegionId -> OrderTypes -> PG.Connection -> IO ()
collectedOrders region_ orders conn = do
    (header, data_) <- collectThePage region_ orders 1
    initialRowsInserted <- PG.executeMany conn (query @MarketOrder) $ map (parseOrder header region_) data_
    res <- mapConcurrently (collectThePage region_ orders) [2 .. (getPages header)]
    rowsInserted <- PG.executeMany conn (query @MarketOrder) $ map (parseOrder header region_) (concatMap snd res)
    print (initialRowsInserted + rowsInserted)

execute :: Options -> IO ()
execute (Options host_ db_ user_ region_ order_types_) = do
  conn <- Connection.open host_ user_ db_
  _ <- mapConcurrently (\r -> collectedOrders r (orderTypesFromString order_types_) conn) regions
  putStrLn "completed" where
    regions = if region_ < 0 then filter notMarketRegions allRegions else [region_]

main :: IO ()
main = execute =<< Opt.execParser options
  where
    options = Opt.info (parse <**> Opt.helper)
      (Opt.fullDesc
        <> Opt.progDesc "Collect Eve Markets data from ESI")
