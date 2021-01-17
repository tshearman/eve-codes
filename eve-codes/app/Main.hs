{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment as SE
import Universe
import Endpoints
import Connection
import Control.Concurrent.Async
import qualified Database.PostgreSQL.Simple as PG
import qualified Data.ByteString.Char8 as BC
import qualified Options.Applicative as Opt
import Options.Applicative ((<**>))

data Options = Options {
  host :: String,
  dbname :: String,
  endpoint :: String,
  user :: String
}

data Sources = Jumps | Kills

sourcesFromString :: String -> Sources
sourcesFromString "jumps" = Jumps
sourcesFromString _ = Kills

parse :: Opt.Parser Options
parse = Options 
  <$> Opt.strOption ( Opt.long "host" <> Opt.help "POSTGRES Hostname" )
  <*> Opt.strOption ( Opt.long "dbname" <> Opt.help "POSTGRES Database" )
  <*> Opt.strOption ( Opt.long "endpoint" <> Opt.help "Data to collect" )
  <*> Opt.strOption ( Opt.long "user" <> Opt.help "POSTGRES User name" )

collected :: Sources -> PG.Connection -> IO ()
collected Jumps conn = do 
  (header, data_) <- collect (systemJumpsPath Latest Tranquility) :: IO (Header, [SystemJumps])
  rowsInserted <- PG.executeMany conn (query @SystemJumps) (map (timeToValue (getLastModified eveDateFormat header)) data_)
  print rowsInserted
collected Kills conn = do 
  (header, data_) <- collect (systemKillsPath Latest Tranquility) :: IO (Header, [SystemKills])
  rowsInserted <- PG.executeMany conn (query @SystemKills) (map (timeToValue (getLastModified eveDateFormat header)) data_)
  print rowsInserted

execute :: Options -> IO ()
execute (Options h db e user) = do
  conn <- Connection.open h user db
  collected (sourcesFromString e) conn

main :: IO ()
main = execute =<< Opt.execParser options
  where
    options = Opt.info (parse <**> Opt.helper)
      (Opt.fullDesc
        <> Opt.progDesc "Collect Eve Universe data from ESI")
