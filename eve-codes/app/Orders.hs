{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module Orders where

import Endpoints
import Esi
import OrdersLib
import Control.Concurrent.Async
import Control.Monad (join)
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

collectedRegions :: String -> String -> String -> IO [PG.Only Integer]
collectedRegions host_ user_ db_ = do
  conn <- Connection.open host_ user_ db_
  PG.query_ conn "SELECT DISTINCT \"regionID\" FROM  sde.\"mapRegions\"" :: IO [PG.Only Integer]    

allRegions :: [Integer]
allRegions = [10000045,10000036,10000019,10000039,11000014,11000028,10000010,10000038,12000004,10000059,10000069,10000004,10000050,10000067,11000012,11000025,10000061,10000006,11000009,10000043,10000054,11000018,10000033,11000030,11000011,10000065,10000046,11000022,10000012,11000032,10000048,10000062,10000066,10000037,10000007,10000011,10000002,11000017,11000008,10000064,11000007,10000005,10000016,10000031,13000001,10000020,10000034,10000001,11000004,10000047,11000031,10000021,11000001,10000049,10000015,10000022,11000015,12000002,10000028,12000005,11000002,10000008,10000029,10000025,10000057,11000023,12000003,10000058,11000013,10000040,10000052,11000010,11000021,10000060,10000027,10000003,10000044,11000029,11000024,10000055,10000070,10000068,10000017,10000013,10000018,10000041,11000026,10000009,11000020,10000014,10000042,11000016,11000005,10000053,11000033,10000032,11000003,12000001,10000035,10000056,10000051,10000030,11000019,10000063,11000027,10000023,11000006]

notSpecial :: Integer -> Bool
notSpecial i = i `notElem` [10000002, 10000030, 10000032, 10000042, 10000043]

execute :: Options -> IO ()
execute (Options host_ db_ user_ region_ ts) = do
  conn <- Connection.open host_ user_ db_
  _ <- mapConcurrently (\r -> collectedOrders r (orderTypesFromString ts) conn) regions
  print "completed" where
    regions = if region_ < 0 then filter notSpecial allRegions else [region_]

main :: IO ()
main = execute =<< Opt.execParser options
  where
    options = Opt.info (parse <**> Opt.helper)
      (Opt.fullDesc
        <> Opt.progDesc "Collect Eve Universe data from ESI")