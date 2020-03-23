module Main where

------------------------------------------------------------------------------
import Data.Time
import Obelisk.Backend
import Options.Applicative
------------------------------------------------------------------------------
import Backend
import Database
import Frontend
------------------------------------------------------------------------------


main :: IO ()
main = do
  Args c pgc <- execParser opts
  withConnection pgc $ \conn -> do
    initializeTables conn
    putStrLn "DB Tables Initialized"
    now <- getCurrentTime
    let env = Env pgc now
    case c of
      Scrape -> runScraper env
  where
    opts = info (envP <**> helper)
      (fullDesc <> header "chainweb-data - Processing and analysis of Chainweb data")
