{-# LANGUAGE OverloadedStrings #-}

module Universe where

import Connection
import Options.Applicative ((<**>))
import qualified Options.Applicative as Opt
import UniverseLib (collected, sourcesFromString)
import qualified System.Environment as Env

data Options = Options
  { host :: String,
    dbname :: String,
    endpoint :: String
  }

parse :: Opt.Parser Options
parse =
  Options
    <$> Opt.strOption (Opt.long "host" <> Opt.help "POSTGRES Hostname")
    <*> Opt.strOption (Opt.long "dbname" <> Opt.help "POSTGRES Database")
    <*> Opt.strOption (Opt.long "endpoint" <> Opt.help "Data to collect: {jumps | kills}")

execute :: Options -> IO ()
execute (Options h db e) = do
  user_ <- Env.getEnv "PG_USER"
  pass_ <- Env.getEnv "PG_PASSWORD"
  conn <- Connection.open h user_ pass_ db
  collected (sourcesFromString e) conn

main :: IO ()
main = execute =<< Opt.execParser options
  where
    options =
      Opt.info
        (parse <**> Opt.helper)
        ( Opt.fullDesc
            <> Opt.progDesc "Collect Eve Universe data from ESI"
        )
