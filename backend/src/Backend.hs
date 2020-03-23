{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Backend where

------------------------------------------------------------------------------
import           Common.Route
import           Control.Concurrent
import           Control.Exception
import           Control.Lens
import           Control.Monad
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.Csv as C
import           Data.Csv.Lens
import           Data.Either
import           Data.Pool
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.String.Conv
import           Data.Text (Text)
import qualified Data.Text.Read as T
import           Data.Time
import           Database.Beam
import           Database.Beam.Backend.SQL
import           Database.Beam.Postgres
import           Network.Http.Client hiding (Connection, withConnection)
import           Obelisk.Backend
import           OpenSSL
import           Options.Applicative hiding (columns)
import           System.IO.Streams (InputStream, OutputStream, stdout)
import qualified System.IO.Streams as Streams
import           Text.Printf
------------------------------------------------------------------------------
import           Common.Types.Place
import           Database
------------------------------------------------------------------------------

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> serve $ const $ return ()
  , _backend_routeEncoder = fullRouteEncoder
  }

data Args = Args Command DbConnect

data Env = Env
  { _env_dbConnectInfo :: DbConnect
  }

data DbConnect = PGInfo ConnectInfo | PGString ByteString

-- | Open a postgres database connection.
getConnection :: DbConnect -> IO Connection
getConnection (PGInfo ci) = connect ci
getConnection (PGString s) = connectPostgreSQL s

-- | A bracket for `Connection` interaction.
withConnection :: DbConnect -> (Connection -> IO a) -> IO a
withConnection c = bracket (getConnection c) close

-- | Create a `Pool` based on `DbConnect` settings designated on the command line.
getPool :: DbConnect -> IO (Pool Connection)
getPool c = do
  caps <- getNumCapabilities
  createPool (getConnection c) close 1 5 caps

-- | A bracket for `Pool` interaction.
withPool :: DbConnect -> (Pool Connection -> IO a) -> IO a
withPool c = bracket (getPool c) destroyAllResources

data Command = Scrape

envP :: Parser Args
envP = Args
  <$> commands
  <*> connectP

connectP :: Parser DbConnect
connectP = (PGString <$> pgstringP) <|> (PGInfo <$> connectInfoP)

pgstringP :: Parser ByteString
pgstringP = strOption (long "dbstring" <> help "Postgres Connection String")

-- | These defaults are pulled from the postgres-simple docs.
connectInfoP :: Parser ConnectInfo
connectInfoP = ConnectInfo
  <$> strOption   (long "dbhost" <> value "localhost" <> help "Postgres DB hostname")
  <*> option auto (long "dbport" <> value 5432        <> help "Postgres DB port")
  <*> strOption   (long "dbuser" <> value "postgres"  <> help "Postgres DB user")
  <*> strOption   (long "dbpass" <> value ""          <> help "Postgres DB password")
  <*> strOption   (long "dbname" <> value "postgres"  <> help "Postgres DB name")

commands :: Parser Command
commands = hsubparser
  (  command "scrape" (info (pure Scrape)
       (progDesc "Scrape reports from the internet"))
--  <> command "server" (info (pure Server)
--       (progDesc "Run backend web server"))
  )

--runScraper :: Env -> IO ()
--runScraper env = do
runScraper :: Env -> IO ()
runScraper (Env dbConn) = do
  withOpenSSL $ do
    let prefix = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/"
    now <- getCurrentTime
    confirmed <- get (prefix <> "time_series_19-covid-Confirmed.csv") concatHandler
    deaths <- get (prefix <> "time_series_19-covid-Deaths.csv") concatHandler
    recovered <- get (prefix <> "time_series_19-covid-Recovered.csv") concatHandler
    printf "Inserting %d records\n" (length $ extractPlaces confirmed)
    withConnection dbConn $ \conn -> do
      runBeamPostgresDebug putStrLn conn $ do
        -- Write Pub Key many-to-many relationships if unique --
        runInsert $
          insert (_covidDb_places covidDb) $
          insertExpressions (rights $ extractPlaces confirmed)
          --onConflict (conflictingFields primaryKey) onConflictDoNothing

    return ()

place :: PlaceT (QExpr Postgres s)
place = Place default_ (val_ "US") (val_ $ Just ("TN" :: Text)) (val_ 5) (val_ 10)

data Column = StateCol | CountryCol | LatCol | LonCol
  deriving (Eq,Ord,Show,Read,Enum)

colName :: Column -> Text
colName StateCol = "Province/State"
colName CountryCol = "Country/Region"
colName LatCol = "Lat"
colName LonCol = "Long"

parseCol :: MonadPlus m => Text -> m Column
parseCol "Province/State" = pure StateCol
parseCol "Country/Region" = pure CountryCol
parseCol "Lat" = pure LatCol
parseCol "Long" = pure LonCol
parseCol _ = mzero

instance C.ToField Column where
  toField = toS . colName

instance C.FromField Column where
  parseField = parseCol . toS

instance C.ToField Day where
  toField = toS . formatTime defaultTimeLocale "%D"

instance C.FromField Day where
  parseField = parseTimeM True defaultTimeLocale "%D" . toS

extractPlaces :: ByteString -> [Either String (PlaceT (QExpr Postgres s))]
extractPlaces bs = map rowToPlace records
  where
    records = toS bs ^.. namedCsv . rows

rowToPlace :: CsvRecord C.Name -> Either String (PlaceT (QExpr Postgres s))
rowToPlace r = do
    (lat,_) <- T.double $ toS $ r ^. ix (toS $ colName LatCol)
    (lon,_) <- T.double $ toS $ r ^. ix (toS $ colName LonCol)
    pure $ Place default_ (val_ $ toS c) (val_ $ Just $ toS s) (val_ lat) (val_ lon)
  where
    c = r ^. ix (toS $ colName CountryCol)
    s = r ^. ix (toS $ colName StateCol)

scrape :: URL -> IO ByteString
scrape thing = withOpenSSL $ do
  let prefix = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/"
  get (prefix <> "time_series_19-covid-" <> thing <> ".csv") concatHandler
