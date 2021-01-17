{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE AllowAmbiguousTypes #-} 
{-# LANGUAGE ScopedTypeVariables #-} 

module Endpoints where

import Control.Lens
import Data.Aeson
import Data.Maybe
import Data.Time (UTCTime, parseTimeM, defaultTimeLocale)
import Data.Time.Clock.POSIX (POSIXTime, utcTimeToPOSIXSeconds)
import Database.PostgreSQL.Simple.ToField (Action)
import Network.HTTP.Simple hiding (Header, Query)
import Network.HTTP.Types.Header (HeaderName)
import Network.Wreq
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Lazy.Internal as BLI
import qualified Data.CaseInsensitive as CI
import qualified Data.Text as T
import qualified Network.Wreq.Lens as L
import qualified Database.PostgreSQL.Simple as PG

type Header = [(HeaderName, BI.ByteString)]
type DateFormat = String

data RequestMethod = Get | Push
instance Show RequestMethod where
  show Get = "GET"
  show Push = "PUSH"

data Endpoint = Endpoint {
 path :: String,
 opts :: Maybe L.Options
} deriving Show

data Param = Param {
  name :: T.Text,
  values :: [T.Text]
} deriving Show

addParam :: L.Options -> Param -> L.Options
addParam o p = o & param (name p) .~ values p

addParams :: L.Options -> [Param] -> L.Options
addParams = foldl addParam

retrieve :: Endpoint -> IO (Response BLI.ByteString)
retrieve (Endpoint e (Just o)) = getWith o e
retrieve (Endpoint e Nothing) = get e

genOptions :: [Param] -> Maybe L.Options
genOptions [] = Nothing
genOptions ps = Just(addParams defaults ps)

filterHeader :: String -> Header -> (HeaderName, BI.ByteString)
filterHeader key hdr = head ls where
  k = CI.mk $ BC.pack key
  ls = filter (\(n, _) -> n == k) hdr

getPages :: Header -> Int
getPages hdr = read(BC.unpack $ snd $ filterHeader "x-pages" hdr) :: Int

getDate :: DateFormat -> String -> Header -> POSIXTime
getDate fmt key r = fromJust $ parseEpoch fmt $ BC.unpack $ snd $ filterHeader key r

getExpiration :: DateFormat -> Header -> POSIXTime
getExpiration fmt = getDate fmt "expires"

getLastModified :: DateFormat -> Header -> POSIXTime
getLastModified fmt = getDate fmt "last-modified"

parseEpoch :: DateFormat -> String -> Maybe POSIXTime
parseEpoch f t = utcTimeToPOSIXSeconds <$> s
  where s = parseTimeM True locale f t :: Maybe UTCTime
        locale = defaultTimeLocale

esi :: String
esi = "https://esi.evetech.net"

eveDateFormat :: DateFormat
eveDateFormat = "%a, %d %b %Y %T %Z"

(./) :: String -> String -> String
x ./ y = ((x ++) . ("/" ++) . (y ++)) ""

class Processable a where
  process :: FromJSON a => Response BLI.ByteString -> (Header, [a])
  process response = (getResponseHeaders response, processBody response)

  processBody :: FromJSON a => Response BLI.ByteString -> [a]
  processBody response = rows where
    body = getResponseBody response
    decoded = decode body
    rows = case decoded of
      Nothing -> []
      (Just as) -> as

class Collectible a where
  collect :: (FromJSON a, Processable a) => Endpoint -> IO (Header, [a])
  collect e = fmap process (retrieve e)

class Sqlible a where
  query :: PG.Query

class TimeToValue a where
  timeToValue :: POSIXTime -> a -> [Action]
