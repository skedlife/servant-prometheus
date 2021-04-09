{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Servant.Prometheus.Internal where

import           Control.Exception
import           Control.Monad
import           Data.Hashable                                 (Hashable (..))
import qualified Data.HashMap.Strict                           as H
import           Data.Monoid
import           Data.Text                                     (Text)
import qualified Data.Text                                     as T
import qualified Data.Text.Encoding                            as T
import           Data.Time.Clock
import           GHC.Generics                                  (Generic)
import           Network.HTTP.Types                            (Method,
                                                                Status (..))
import           Network.Wai                                   (Middleware,
                                                                responseStatus)
import           System.Metrics.Prometheus.Concurrent.Registry
import qualified System.Metrics.Prometheus.Metric.Counter      as Counter
import qualified System.Metrics.Prometheus.Metric.Gauge        as Gauge
import qualified System.Metrics.Prometheus.Metric.Histogram    as Histogram
import           System.Metrics.Prometheus.MetricId            (Name (..))
import qualified System.Metrics.Prometheus.MetricId            as Labels

-- import           System.Metrics
-- import qualified System.Metrics.Counter      as Counter
-- import qualified System.Metrics.Distribution as Distribution
-- import qualified System.Metrics.Gauge        as Gauge

data Meters = Meters
    { metersInflight :: Gauge.Gauge
    , metersC2XX     :: Counter.Counter
    , metersC4XX     :: Counter.Counter
    , metersC5XX     :: Counter.Counter
    , metersCXXX     :: Counter.Counter
    , metersTime     :: Histogram.Histogram
    }

data APIEndpoint = APIEndpoint {
    pathSegments :: [Text],
    method       :: Method
} deriving (Eq, Hashable, Show, Generic)

gaugeInflight :: Gauge.Gauge -> Middleware
gaugeInflight inflight application request respond =
    bracket_ (Gauge.inc inflight)
             (Gauge.dec inflight)
             (application request respond)

-- | Count responses with 2XX, 4XX, 5XX, and XXX response codes.
countResponseCodes
    :: (Counter.Counter, Counter.Counter, Counter.Counter, Counter.Counter)
    -> Middleware
countResponseCodes (c2XX, c4XX, c5XX, cXXX) application request respond =
    application request respond'
  where
    respond' res = count (responseStatus res) >> respond res
    count Status{statusCode = sc }
        | 200 <= sc && sc < 300 = Counter.inc c2XX
        | 400 <= sc && sc < 500 = Counter.inc c4XX
        | 500 <= sc && sc < 600 = Counter.inc c5XX
        | otherwise             = Counter.inc cXXX

responseTimeHistogram :: Histogram.Histogram -> Middleware
responseTimeHistogram dist application request respond =
    bracket getCurrentTime stop $ const $ application request respond
  where
    stop t1 = do
        t2 <- getCurrentTime
        let dt = diffUTCTime t2 t1
        Histogram.observe (fromRational $ (*1000) $ toRational dt) dist

bucket =
  [ 100.0
  , 200.0
  , 500.0
  , 1000.0
  , 2000.0
  , 5000.0
  , 10000.0
  ]

initializeMeters :: Registry -> APIEndpoint -> IO (APIEndpoint, Meters)
initializeMeters registry endpoint@APIEndpoint{..} = do
    metersInflight <- registerGauge     (Name ("servant_in_flight")) labels registry
    metersC2XX     <- registerCounter   (Name ("servant_responses_2XX")) labels registry
    metersC4XX     <- registerCounter   (Name ("servant_responses_4XX")) labels registry
    metersC5XX     <- registerCounter   (Name ("servant_responses_5XX")) labels registry
    metersCXXX     <- registerCounter   (Name ("servant_responses_XXX")) labels registry
    metersTime     <- registerHistogram (Name ("servant_time_ms")) labels bucket registry

    return (endpoint, Meters{..})

    where
        labels = Labels.fromList [("path", path)]
        path   = T.intercalate "_" $ pathSegments <> [T.decodeUtf8 method]

initializeMetersTable :: Registry -> [APIEndpoint] -> IO (H.HashMap APIEndpoint Meters)
initializeMetersTable registry endpoints = do
    meters  <- mapM (initializeMeters registry) endpoints

    return $ H.fromList meters
