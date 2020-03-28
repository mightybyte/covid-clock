{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Database
  ( CovidDb(..)
  , covidDb
  , initializeTables
  ) where

------------------------------------------------------------------------------
import qualified Data.Text as T
import           Data.Time
import           Database.Beam
import           Database.Beam.Backend.SQL.SQL92 (timestampType)
import           Database.Beam.Migrate
import           Database.Beam.Migrate.Generics (HasDefaultSqlDataType(..))
import           Database.Beam.Migrate.Simple
import           Database.Beam.Postgres
import           Database.Beam.Postgres.Migrate (migrationBackend)
------------------------------------------------------------------------------
import           Common.Types.Place
import           Common.Types.Report
------------------------------------------------------------------------------

data CovidDb f = CovidDb
  { _covidDb_places :: f (TableEntity PlaceT)
  , _covidDb_reports :: f (TableEntity ReportT)
  }
  deriving stock (Generic)
  deriving anyclass (Database be)

migratableDb :: CheckedDatabaseSettings Postgres CovidDb
migratableDb = defaultMigratableDbSettings `withDbModification` dbModification
  { _covidDb_places = modifyCheckedTable (T.toLower . T.drop 8) checkedTableModification
    { place_id = "id"
    , place_country = "country"
    , place_region = "region"
    , place_lat = "lat"
    , place_lon = "lon"
    }
  , _covidDb_reports = modifyCheckedTable (T.toLower . T.drop 8) checkedTableModification
    { report_timestamp = "timestamp"
    , report_place = PlaceId "place"
    , report_asOf = "asof"
    , report_confirmed = "confirmed"
    , report_deaths = "deaths"
    , report_recovered = "recovered"
    }
  }

covidDb :: DatabaseSettings Postgres CovidDb
covidDb = unCheckDatabase migratableDb

-- | Create the DB tables if necessary.
initializeTables :: Connection -> IO ()
initializeTables conn = runBeamPostgresDebug putStrLn conn $
  verifySchema migrationBackend migratableDb >>= \case
    VerificationFailed _ -> autoMigrate migrationBackend migratableDb
    VerificationSucceeded -> pure ()

--------------------------------------------------------------------------------
-- Orphan

-- Until this is merged: https://github.com/tathougies/beam/pull/422
instance HasDefaultSqlDataType Postgres UTCTime where
  defaultSqlDataType _ _ _ = timestampType Nothing True
