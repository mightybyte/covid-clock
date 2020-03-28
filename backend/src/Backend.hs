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
import           Control.Error
import           Control.Exception
import           Control.Lens
import           Control.Monad
import           Data.ByteString (ByteString)
import qualified Data.Csv as C
import           Data.Csv.Lens
import           Data.List (sort)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Pool
import qualified Data.Set as S
import           Data.String.Conv
import           Data.Text (Text)
import qualified Data.Text.Read as T
import           Data.Time
import           Data.Traversable
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
    confirmed <- get (prefix <> "time_series_covid19_confirmed_global.csv") concatHandler
    deaths <- get (prefix <> "time_series_covid19_deaths_global.csv") concatHandler
    recovered <- get (prefix <> "time_series_covid19_recovered_global.csv") concatHandler
    let emap = do
          c <- sequence $ recordToMap (\n -> mempty { pConfirmed = n }) <$> (toS confirmed ^.. namedCsv . rows)
          d <- sequence $ recordToMap (\n -> mempty { pDeaths = n }) <$> (toS deaths ^.. namedCsv . rows)
          r <- sequence $ recordToMap (\n -> mempty { pRecovered = n }) <$> (toS recovered ^.. namedCsv . rows)
          pure $ M.unionsWith mappend (c <> d <> r)
    case emap of
      Left e -> do
        putStrLn "Unrecoverable rror in scraped data:"
        putStrLn e
      Right m -> do
        pidMap <- insertPlaces env $ S.toList $ S.fromList $ map fst $ M.keys m
        insertMap env pidMap $ M.toList m

--    printf "Inserting confirmed records for %d locations\n" (length $ extractPlaces confirmed)
--    forM_ (toS confirmed ^.. namedCsv . rows) $ insertChangedData env Confirmed
--    printf "Inserting deaths records for %d locations\n" (length $ extractPlaces deaths)
--    forM_ (toS deaths ^.. namedCsv . rows) $ insertChangedData env Deaths
--    printf "Inserting recovered records for %d locations\n" (length $ extractPlaces recovered)
--    forM_ (toS recovered ^.. namedCsv . rows) $ insertChangedData env Recovered
    --withConnection (_env_dbConnectInfo env) $ \conn -> do
    --  runBeamPostgresDebug putStrLn conn $ do
    --    runInsert $
    --      insert (_covidDb_places covidDb) $
    --      insertExpressions (rights $ extractPlaces confirmed)

    return ()

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

data SimplePlace = SimplePlace
  { spCountry :: Text
  , spRegion :: Maybe Text
  , spLat :: Double
  , spLon :: Double
  } deriving (Eq,Ord,Show)

data Payload = Payload
  { pConfirmed :: Int
  , pDeaths :: Int
  , pRecovered :: Int
  } deriving (Eq,Ord,Show)

instance Semigroup Payload where
  Payload ac ad ar <> Payload bc bd br = Payload (ac+bc) (ad+bd) (ar+br)

instance Monoid Payload where
  mempty = Payload 0 0 0
  mappend (Payload ac ad ar) (Payload bc bd br) = Payload (ac+bc) (ad+bd) (ar+br)

recordToMap
  :: (Int -> Payload)
  -> CsvRecord C.Name
  -> Either String (Map (SimplePlace, Day) Payload)
recordToMap mkPayload record = do
    let c = record ^. ix (toS $ colName CountryCol)
        s = record ^. ix (toS $ colName StateCol)
    (lat,_) <- T.double $ toS $ record ^. ix (toS $ colName LatCol)
    (lon,_) <- T.double $ toS $ record ^. ix (toS $ colName LonCol)
    let sp = SimplePlace (toS c) (Just $ toS s) lat lon
    m <- note "Error parsing NamedRecord" $ record ^? _NamedRecord @(Map String Text)
    fmap M.unions $ forM (M.toList m) $ \(fn, val) -> do
      case parseDay fn of
        Nothing -> pure mempty
        Just d -> do
          (v,_) <- T.decimal val
          pure (M.singleton (sp, d) $ mkPayload v)

insertPlaces :: Env -> [SimplePlace] -> IO (Map SimplePlace PlaceId)
insertPlaces env places = withConnection (_env_dbConnectInfo env) $ \conn ->
    runBeamPostgres conn $ fmap M.unions $ forM places $ \sp@(SimplePlace c r lat lon) -> do
      ps <- runInsertReturningList $ insert (_covidDb_places covidDb) $
          insertExpressions [Place default_ (val_ $ toS c) (val_ r) (val_ lat) (val_ lon)]
      case ps of
        [p] -> return $ M.singleton sp $ pk p
        _ -> pure mempty

insertMap :: Env -> Map SimplePlace PlaceId -> [((SimplePlace, Day), Payload)] -> IO ()
insertMap env pidMap rows = withConnection (_env_dbConnectInfo env) $ \conn ->
    runBeamPostgres conn $ forM_ rows $ \((sp, d), (Payload p1 p2 p3)) -> do
      runInsert $ insert (_covidDb_reports covidDb) $ insertValues
        [Report (_env_now env) (pidMap M.! sp) d p1 p2 p3]


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
        guard_ (place_region p ==. val_ (Just $ toS s))
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
                          Confirmed -> [Report now (PlaceId $ SqlSerial pid) d v 0 0]
                          Deaths -> [Report now (PlaceId $ SqlSerial pid) d 0 v 0]
                          Recovered -> [Report now (PlaceId $ SqlSerial pid) d 0 0 v]

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
                                    report_asOf rep ==. val_ d)


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

{-
select asof, sum(confirmed) as confirmed, sum(confirmed) - lag(sum(confirmed)) over (order by asof) as delta_confirmed, sum(deaths) as deaths, sum(deaths) - lag(sum(deaths)) over (order by asof) as delta_deaths, sum(recovered) as recovered, sum(recovered) - lag(sum(recovered)) over (order by asof) as delta_recovered from reports group by asof order by asof;

-}
