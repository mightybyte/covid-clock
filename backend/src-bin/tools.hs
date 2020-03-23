module Main where

------------------------------------------------------------------------------
import Backend
import Database
import Frontend
import Obelisk.Backend
import Options.Applicative
------------------------------------------------------------------------------


main :: IO ()
main = do
  Args c pgc <- execParser opts
  withConnection pgc $ \conn -> do
    initializeTables conn
    putStrLn "DB Tables Initialized"
    let env = Env pgc
    case c of
      Scrape -> runScraper env
  where
    opts = info (envP <**> helper)
      (fullDesc <> header "chainweb-data - Processing and analysis of Chainweb data")
