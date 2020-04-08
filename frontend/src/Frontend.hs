{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Frontend where

------------------------------------------------------------------------------
import           Control.Monad.Fix
import           Control.Monad.Trans
import           Data.Default
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time
import           Data.Time.Clock
import           Data.Time.Clock.POSIX
import           Formattable
import           Obelisk.Frontend
import           Obelisk.Route
import           Obelisk.Generated.Static
import           Reflex.Dom.Core
------------------------------------------------------------------------------
import           Common.Types
import           Common.Route
import           CssClass
------------------------------------------------------------------------------


-- This runs in a monad that can be run on the client or the server.
-- To run code in a pure client or pure server context, use one of the
-- `prerender` functions.
frontend :: Frontend (R FrontendRoute)
frontend = Frontend
    { _frontend_head = do
        el "title" $ text "COVID-19 Clock"
        css $ static @"milligram.min.css"
        css $ static @"main.css"
    , _frontend_body = do
        elClass "main" "wrapper" app

        -- `prerender` and `prerender_` let you choose a widget to run on the server
        -- during prerendering and a different widget to run on the client with
        -- JavaScript. The following will generate a `blank` widget on the server and
        -- print "Hello, World!" on the client.
        --prerender_ blank $ liftJSM $ void $ eval ("console.log('Hello, World!')" :: T.Text)

    }
  where
    css href = elAttr "link" ("href" =: href <> "type" =: "text/css" <> "rel" =: "stylesheet") blank

data StatCfg = StatCfg
  { statCfg_class :: CssClass
  }

instance Default StatCfg where
  def = StatCfg mempty

data Reading = Reading
  { readingDate :: Day
  , readingValue :: Integer
  , readingDelta :: Integer
  }

readingToCounter :: Reading -> LiveCounter Double
readingToCounter (Reading d v dv) =
  LiveCounter (utcTimeToPOSIXSeconds $ UTCTime d 0) (fromIntegral v) (fromIntegral dv / 86400)

app
  :: (DomBuilder t m, Prerender js t m)
  => m ()
app = do
  let caseCounter = readingToCounter $ Reading (fromGregorian 2020 4 8) 1426096 80995
  let deathCounter = readingToCounter $ Reading (fromGregorian 2020 4 8) 81865 7300
  elClass "section" "page-title" $ do
    el "h1" $ text "COVID-19 Clock"
  elClass "section" "statistics" $ prerender_ blank $ do
    let counter c = liveCounter (dynText . fmap (runFormat intFmt)) c
        fmt = numFmt { _nfPrec = Just (2, SigFigs) }
        mkStat cfg nm c = stat cfg (nm <> " (one every " <>
                                    runFormat fmt (1.0 / liveCounter_velocity c) <>
                                    " sec)") $ counter c
    mkStat def "Estimated Cases" caseCounter
    mkStat (StatCfg "stat--red") "Estimated Deaths" deathCounter

stat
  :: Monad m
  => DomBuilder t m
  => StatCfg
  -> Text
  -> m a
  -> m ()
stat cfg caption c = do
  elKlass "div" ("stat" <> statCfg_class cfg) $ do
    _ <- divClass "stat__content" c
    divClass "stat__caption" $ text caption

------------------------------------------------------------------------------
liveCounter
  :: ( MonadIO m, MonadIO (Performable m), DomBuilder t m, PerformEvent t m
     , PostBuild t m, MonadHold t m, MonadFix m, TriggerEvent t m
     )
  => (Dynamic t Double -> m ())
  -> LiveCounter Double
  -> m ()
liveCounter counterWidget lc = do
  let tickDelta = realToFrac (1.0 / liveCounter_velocity lc) :: NominalDiffTime
  t0 <- liftIO getCurrentTime
  dti <- clockLossy tickDelta t0
  counterWidget $ do
    ti <- dti
    pure $ liveCounter_position $ predictTo lc $ utcTimeToPOSIXSeconds $ _tickInfo_lastUTC ti
