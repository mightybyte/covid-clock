{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Common.Types.Report where

------------------------------------------------------------------------------
import           Data.Aeson
import           Data.Text (Text)
import           Data.Time.Clock (UTCTime)
import           Database.Beam
------------------------------------------------------------------------------
import           Common.Types.Place
------------------------------------------------------------------------------

------------------------------------------------------------------------------
data ReportT f = Report
  { report_timestamp :: C f UTCTime
  , report_place :: PrimaryKey PlaceT f
  , report_asOf :: C f UTCTime
  , report_confirmed :: C f Int
  , report_deaths :: C f Int
  , report_recovered :: C f Int
  } deriving (Generic)

type Report = ReportT Identity
type ReportId = PrimaryKey ReportT Identity

deriving instance Eq (PrimaryKey ReportT Identity)
deriving instance Eq Report
deriving instance Ord (PrimaryKey ReportT Identity)
deriving instance Ord Report
deriving instance Show (PrimaryKey ReportT Identity)
deriving instance Show Report

instance ToJSON (PrimaryKey ReportT Identity) where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON (PrimaryKey ReportT Identity)

instance ToJSON Report where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON Report

instance Beamable ReportT

instance Table ReportT where
  data PrimaryKey ReportT f = ReportKey (C f UTCTime) (PrimaryKey PlaceT f) (C f UTCTime)
    deriving (Generic, Beamable)
  -- The Applicative instance for (->) is really convenient here
  primaryKey = ReportKey <$> report_timestamp <*> report_place <*> report_asOf
