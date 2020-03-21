{-# LANGUAGE DeriveGeneric #-}

module Common.Types where

------------------------------------------------------------------------------
import           Data.Aeson
import           Data.Int
import           Data.Text (Text)
import           Data.Time
import           Data.Time.Clock
import           Data.Time.Clock.POSIX
import           GHC.Generics
------------------------------------------------------------------------------

data LiveCounter a = LiveCounter
  { liveCounter_time :: POSIXTime
  , liveCounter_position :: a
  , liveCounter_velocity :: Double
  } deriving (Generic)

predict :: LiveCounter Double -> NominalDiffTime -> LiveCounter Double
predict (LiveCounter t p v) dt = LiveCounter (t + dt) (p + realToFrac dt * v) v

predictTo :: LiveCounter Double -> POSIXTime -> LiveCounter Double
predictTo lc t1 = predict lc (t1 - liveCounter_time lc)

instance ToJSON a => ToJSON (LiveCounter a) where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON a => FromJSON (LiveCounter a)

{-
First

curl 'https://services1.arcgis.com/0MSEUqKaxRlEPj5g/arcgis/rest/services/cases_time_v3/FeatureServer/0/query?f=json&where=1%3D1&returnGeometry=false&spatialRel=esriSpatialRelIntersects&outFields=*&orderByFields=Report_Date_String%20asc&resultOffset=0&resultRecordCount=2000&cacheHint=true' -H 'Referer: https://www.arcgis.com/apps/opsdashboard/index.html' -H 'Origin: https://www.arcgis.com' -H 'Sec-Fetch-Dest: empty' -H 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_13_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/80.0.3987.116 Safari/537.36' --compressed

Better

curl 'https://services9.arcgis.com/N9p5hsImWXAccRNI/arcgis/rest/services/Z7biAeD8PAkqgmWhxG2A/FeatureServer/2/query?f=json&where=Confirmed%20%3E%200&returnGeometry=false&spatialRel=esriSpatialRelIntersects&outFields=*&orderByFields=Confirmed%20desc&resultOffset=0&resultRecordCount=200&cacheHint=true' -H 'authority: services9.arcgis.com' -H 'origin: https://www.arcgis.com' -H 'sec-fetch-dest: empty' -H 'user-agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_13_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/80.0.3987.116 Safari/537.36' -H 'accept: */*' -H 'sec-fetch-site: same-site' -H 'sec-fetch-mode: cors' -H 'referer: https://www.arcgis.com/apps/opsdashboard/index.html' -H 'accept-language: en-US,en;q=0.9'

curl 'https://services9.arcgis.com/N9p5hsImWXAccRNI/arcgis/rest/services/Z7biAeD8PAkqgmWhxG2A/FeatureServer/2/query?f=json&where=Confirmed%20%3E%200&returnGeometry=false&spatialRel=esriSpatialRelIntersects&outFields=*&orderByFields=Confirmed%20desc&outSR=102100&resultOffset=0&resultRecordCount=200&cacheHint=true' -H 'authority: services9.arcgis.com' -H 'origin: https://www.arcgis.com' -H 'sec-fetch-dest: empty' -H 'user-agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_13_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/80.0.3987.116 Safari/537.36' -H 'accept: */*' -H 'sec-fetch-site: same-site' -H 'sec-fetch-mode: cors' -H 'referer: https://www.arcgis.com/apps/opsdashboard/index.html' -H 'accept-language: en-US,en;q=0.9'
-}

data DataPoint = DataPoint
  { dataPoint_lastUpdate :: POSIXTime
  , dataPoint_lat :: Double
  , dataPoint_lon :: Double
  , dataPoint_confirmed :: Int64
  , dataPoint_deaths :: Int64
  , dataPoint_recovered :: Int64
  , dataPoint_active :: Int64
  , dataPoint_objectId :: Int
  }
