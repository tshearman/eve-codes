{-# LANGUAGE OverloadedStrings #-}

module Universe where

import Connection
import Options.Applicative ((<**>))
import qualified Options.Applicative as Opt
import UniverseLib (collected, sourcesFromString)

data Options = Options
  { host :: String,
    dbname :: String,
    endpoint :: String,
    user :: String,
    password :: String
  }

parse :: Opt.Parser Options
parse =
  Options
    <$> Opt.strOption (Opt.long "host" <> Opt.help "POSTGRES Hostname")
    <*> Opt.strOption (Opt.long "dbname" <> Opt.help "POSTGRES Database")
    <*> Opt.strOption (Opt.long "endpoint" <> Opt.help "Data to collect: {jumps | kills}")
    <*> Opt.strOption (Opt.long "user" <> Opt.help "POSTGRES User name")
    <*> Opt.strOption (Opt.long "password" <> Opt.help "POSTGRES Password")

execute :: Options -> IO ()
execute (Options h db e user_ pass_) = do
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
