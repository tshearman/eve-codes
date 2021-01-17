{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module Orders where

import Endpoints
import Esi
import OrdersLib
import Control.Concurrent.Async
import qualified Database.PostgreSQL.Simple as PG
import Database.PostgreSQL.Simple.ToField (Action)
import qualified Options.Applicative as Opt
import Options.Applicative ((<**>))
import Connection

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
execute (Options h db user_ region_ ts) = do
  conn <- Connection.open h user_ db
  collectedOrders region_ (orderTypesFromString ts) conn

main :: IO ()
main = execute =<< Opt.execParser options
  where
    options = Opt.info (parse <**> Opt.helper)
      (Opt.fullDesc
        <> Opt.progDesc "Collect Eve Universe data from ESI")
