{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}


{- |
= Ads

A domain for ad-related metrics and data.
-}


module CDP.Domains.Ads (module CDP.Domains.Ads) where

import           Control.Applicative  ((<$>))
import           Control.Monad
import           Control.Monad.Loops
import           Control.Monad.Trans  (liftIO)
import qualified Data.Map             as M
import           Data.Maybe          
import Data.Functor.Identity
import Data.String
import qualified Data.Text as T
import qualified Data.List as List
import qualified Data.Text.IO         as TI
import qualified Data.Vector          as V
import Data.Aeson.Types (Parser(..))
import           Data.Aeson           (FromJSON (..), ToJSON (..), (.:), (.:?), (.=), (.!=), (.:!))
import qualified Data.Aeson           as A
import qualified Network.HTTP.Simple as Http
import qualified Network.URI          as Uri
import qualified Network.WebSockets as WS
import Control.Concurrent
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as Map
import Data.Proxy
import System.Random
import GHC.Generics
import Data.Char
import Data.Default

import CDP.Internal.Utils


import CDP.Domains.DOMNetworkEmulationPageSecurity as DOMNetworkEmulationPageSecurity


-- | Type 'Ads.AdFrameData'.
--   Ad frame data.
data AdsAdFrameData = AdsAdFrameData
  {
    -- | The DevTools frame token.
    adsAdFrameDataFrameId :: DOMNetworkEmulationPageSecurity.PageFrameId,
    -- | The initial origin of the frame. To minimize the payload size, this is
    --   only sent once per frame.
    adsAdFrameDataInitialOrigin :: Maybe T.Text,
    -- | The network bytes of the frame.
    adsAdFrameDataNetworkBytes :: Double,
    -- | The CPU time of the frame, in milliseconds.
    adsAdFrameDataCpuTime :: Double
  }
  deriving (Eq, Show)
instance FromJSON AdsAdFrameData where
  parseJSON = A.withObject "AdsAdFrameData" $ \o -> AdsAdFrameData
    <$> o A..: "frameId"
    <*> o A..:? "initialOrigin"
    <*> o A..: "networkBytes"
    <*> o A..: "cpuTime"
instance ToJSON AdsAdFrameData where
  toJSON p = A.object $ catMaybes [
    ("frameId" A..=) <$> Just (adsAdFrameDataFrameId p),
    ("initialOrigin" A..=) <$> (adsAdFrameDataInitialOrigin p),
    ("networkBytes" A..=) <$> Just (adsAdFrameDataNetworkBytes p),
    ("cpuTime" A..=) <$> Just (adsAdFrameDataCpuTime p)
    ]

-- | Type 'Ads.AdMetrics'.
--   Ad metrics for a page.
data AdsAdMetrics = AdsAdMetrics
  {
    -- | The viewport ad density by area, represented as a percentage (an integer
    --   between 0 and 100).
    adsAdMetricsViewportAdDensityByArea :: Int,
    -- | The time-weighted average of the viewport ad density by area, measured
    --   across the duration of the page.
    adsAdMetricsAverageViewportAdDensityByArea :: Double,
    -- | The number of ads currently visible within the viewport.
    adsAdMetricsViewportAdCount :: Int,
    -- | The time-weighted average of the viewport ad count, measured across the
    --   duration of the page.
    adsAdMetricsAverageViewportAdCount :: Double,
    -- | The total ad CPU usage, in milliseconds.
    adsAdMetricsTotalAdCpuTime :: Double,
    -- | The total ad network bytes.
    adsAdMetricsTotalAdNetworkBytes :: Double,
    -- | The list of ad frames that have been updated since the last event.
    adsAdMetricsUpdateAdFrames :: [AdsAdFrameData],
    -- | The list of ad frame IDs that have been removed since the last event.
    adsAdMetricsRemoveAdFrames :: [DOMNetworkEmulationPageSecurity.PageFrameId]
  }
  deriving (Eq, Show)
instance FromJSON AdsAdMetrics where
  parseJSON = A.withObject "AdsAdMetrics" $ \o -> AdsAdMetrics
    <$> o A..: "viewportAdDensityByArea"
    <*> o A..: "averageViewportAdDensityByArea"
    <*> o A..: "viewportAdCount"
    <*> o A..: "averageViewportAdCount"
    <*> o A..: "totalAdCpuTime"
    <*> o A..: "totalAdNetworkBytes"
    <*> o A..: "updateAdFrames"
    <*> o A..: "removeAdFrames"
instance ToJSON AdsAdMetrics where
  toJSON p = A.object $ catMaybes [
    ("viewportAdDensityByArea" A..=) <$> Just (adsAdMetricsViewportAdDensityByArea p),
    ("averageViewportAdDensityByArea" A..=) <$> Just (adsAdMetricsAverageViewportAdDensityByArea p),
    ("viewportAdCount" A..=) <$> Just (adsAdMetricsViewportAdCount p),
    ("averageViewportAdCount" A..=) <$> Just (adsAdMetricsAverageViewportAdCount p),
    ("totalAdCpuTime" A..=) <$> Just (adsAdMetricsTotalAdCpuTime p),
    ("totalAdNetworkBytes" A..=) <$> Just (adsAdMetricsTotalAdNetworkBytes p),
    ("updateAdFrames" A..=) <$> Just (adsAdMetricsUpdateAdFrames p),
    ("removeAdFrames" A..=) <$> Just (adsAdMetricsRemoveAdFrames p)
    ]

-- | Retrieves ad metrics for the current page.

-- | Parameters of the 'Ads.getAdMetrics' command.
data PAdsGetAdMetrics = PAdsGetAdMetrics
  deriving (Eq, Show)
pAdsGetAdMetrics
  :: PAdsGetAdMetrics
pAdsGetAdMetrics
  = PAdsGetAdMetrics
instance ToJSON PAdsGetAdMetrics where
  toJSON _ = A.Null
data AdsGetAdMetrics = AdsGetAdMetrics
  {
    adsGetAdMetricsMetrics :: AdsAdMetrics
  }
  deriving (Eq, Show)
instance FromJSON AdsGetAdMetrics where
  parseJSON = A.withObject "AdsGetAdMetrics" $ \o -> AdsGetAdMetrics
    <$> o A..: "metrics"
instance Command PAdsGetAdMetrics where
  type CommandResponse PAdsGetAdMetrics = AdsGetAdMetrics
  commandName _ = "Ads.getAdMetrics"

