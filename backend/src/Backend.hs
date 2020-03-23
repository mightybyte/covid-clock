{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Backend where

------------------------------------------------------------------------------
import           Common.Route
import           Control.Concurrent
import           Control.Exception
import           Control.Lens
import           Control.Monad
import           Data.ByteString (ByteString)
import qualified Data.Csv as C
import           Data.Csv.Lens
import           Data.Pool
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.String.Conv
import           Data.Text (Text)
import qualified Data.Text.Read as T
import           Data.Time
import           Database.Beam
import           Database.Beam.Backend.SQL
import           Database.Beam.Backend.SQL.BeamExtensions
import           Database.Beam.Postgres
import           Network.Http.Client hiding (Connection, withConnection)
import           Obelisk.Backend
import           OpenSSL
import           Options.Applicative hiding (columns)
import           Text.Printf
------------------------------------------------------------------------------
import           Common.Types.Place
import           Common.Types.Report
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
  , _env_now :: UTCTime
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
runScraper env = do
  withOpenSSL $ do
    let prefix = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/"
    confirmed <- get (prefix <> "time_series_19-covid-Confirmed.csv") concatHandler
    deaths <- get (prefix <> "time_series_19-covid-Deaths.csv") concatHandler
    recovered <- get (prefix <> "time_series_19-covid-Recovered.csv") concatHandler
    printf "Inserting confirmed records for %d locations\n" (length $ extractPlaces confirmed)
    forM_ (toS confirmed ^.. namedCsv . rows) $ insertChangedData env Confirmed
    printf "Inserting deaths records for %d locations\n" (length $ extractPlaces deaths)
    forM_ (toS deaths ^.. namedCsv . rows) $ insertChangedData env Deaths
    printf "Inserting recovered records for %d locations\n" (length $ extractPlaces recovered)
    forM_ (toS recovered ^.. namedCsv . rows) $ insertChangedData env Recovered
    --withConnection (_env_dbConnectInfo env) $ \conn -> do
    --  runBeamPostgresDebug putStrLn conn $ do
    --    runInsert $
    --      insert (_covidDb_places covidDb) $
    --      insertExpressions (rights $ extractPlaces confirmed)

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

data ValType = Confirmed | Deaths | Recovered

insertChangedData :: Env -> ValType -> CsvRecord C.Name -> IO ()
insertChangedData env vt r = withConnection (_env_dbConnectInfo env) $ \conn -> do
    let c = r ^. ix (toS $ colName CountryCol)
        s = r ^. ix (toS $ colName StateCol)
        eloc = do
          (lat,_) <- T.double $ toS $ r ^. ix (toS $ colName LatCol)
          (lon,_) <- T.double $ toS $ r ^. ix (toS $ colName LonCol)
          pure (lat,lon)
    pids <- runBeamPostgres conn $ runSelectReturningList $
      select $ do
        p <- all_ (_covidDb_places covidDb)
        guard_ (place_country p ==. val_ (toS c))
        guard_ (place_state p ==. val_ (Just $ toS s))
        pure $ place_id p
    case pids of
      [] -> case eloc of
              Left e -> do
                putStrLn $ "Data missing lat/lon for " <> show r
                print e
              Right (lat,lon) -> do
                ps <- runBeamPostgres conn $ runInsertReturningList $
                  insert (_covidDb_places covidDb)
                    (insertExpressions [Place default_ (val_ $ toS c) (val_ $ Just $ toS s) (val_ lat) (val_ lon)])
                case ps of
                  [p] -> dispatch vt conn (_env_now env) (unSerial $ place_id p) r
                  _ -> putStrLn $ "Unexpected number of pids returned: " <> show pids
      [SqlSerial pid] -> dispatch vt conn (_env_now env) pid r
      _ -> putStrLn $ "Multiple pids (shouldn't happen): " <> show pids
    return ()

dispatch :: ValType -> Connection -> UTCTime -> Int -> CsvRecord C.Name -> IO ()
dispatch Confirmed = insertReports Confirmed
dispatch Deaths = updateReports Deaths
dispatch Recovered = updateReports Recovered

insertReports :: ValType -> Connection -> UTCTime -> Int -> CsvRecord C.Name -> IO ()
insertReports vt conn now pid r = do
  case r ^? _NamedRecord @(Map String Text) of
    Nothing -> putStrLn "Error parsing to NamedRecord" >> print r
    Just m -> forM_ (M.toList m) $ \(fn, val) -> do
      case parseDay fn of
        Nothing -> return ()
        Just d -> case T.decimal val of
                    Left e -> putStrLn $ "Error parsing value: " <> show e
                    Right (v,_) -> runBeamPostgres conn $ runInsert $
                      insert (_covidDb_reports covidDb) $ insertValues $
                        case vt of
                          Confirmed -> [Report now (PlaceId $ SqlSerial pid) (UTCTime d 0) v 0 0]
                          Deaths -> [Report now (PlaceId $ SqlSerial pid) (UTCTime d 0) 0 v 0]
                          Recovered -> [Report now (PlaceId $ SqlSerial pid) (UTCTime d 0) 0 0 v]

updateReports :: ValType -> Connection -> UTCTime -> Int -> CsvRecord C.Name -> IO ()
updateReports vt conn now pid r = do
  case r ^? _NamedRecord @(Map String Text) of
    Nothing -> putStrLn "Error parsing to NamedRecord" >> print r
    Just m -> forM_ (M.toList m) $ \(fn, val) -> do
      case parseDay fn of
        Nothing -> return ()
        Just d -> case T.decimal val of
                    Left e -> putStrLn $ "Error parsing value: " <> show e
                    Right (v,_) -> runBeamPostgres conn $ runUpdate $

                      update (_covidDb_reports covidDb)
                             (\rep -> case vt of
                                      Confirmed -> report_confirmed rep <-. val_ v
                                      Deaths -> report_deaths rep <-. val_ v
                                      Recovered -> report_recovered rep <-. val_ v)
                             (\rep -> report_timestamp rep ==. val_ now &&.
                                    report_place rep ==. val_ (PlaceId $ SqlSerial pid) &&.
                                    report_asOf rep ==. val_ (UTCTime d 0))


parseDay :: String -> Maybe Day
parseDay = parseTimeM True defaultTimeLocale "%-m/%-d/%y"

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
