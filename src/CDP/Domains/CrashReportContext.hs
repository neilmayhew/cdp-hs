{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}


{- |
= CrashReportContext

This domain exposes the current state of the CrashReportContext API.
-}


module CDP.Domains.CrashReportContext (module CDP.Domains.CrashReportContext) where

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


-- | Type 'CrashReportContext.CrashReportContextEntry'.
--   Key-value pair in CrashReportContext.
data CrashReportContextCrashReportContextEntry = CrashReportContextCrashReportContextEntry
  {
    crashReportContextCrashReportContextEntryKey :: T.Text,
    crashReportContextCrashReportContextEntryValue :: T.Text,
    -- | The ID of the frame where the key-value pair was set.
    crashReportContextCrashReportContextEntryFrameId :: DOMNetworkEmulationPageSecurity.PageFrameId
  }
  deriving (Eq, Show)
instance FromJSON CrashReportContextCrashReportContextEntry where
  parseJSON = A.withObject "CrashReportContextCrashReportContextEntry" $ \o -> CrashReportContextCrashReportContextEntry
    <$> o A..: "key"
    <*> o A..: "value"
    <*> o A..: "frameId"
instance ToJSON CrashReportContextCrashReportContextEntry where
  toJSON p = A.object $ catMaybes [
    ("key" A..=) <$> Just (crashReportContextCrashReportContextEntryKey p),
    ("value" A..=) <$> Just (crashReportContextCrashReportContextEntryValue p),
    ("frameId" A..=) <$> Just (crashReportContextCrashReportContextEntryFrameId p)
    ]

-- | Returns all entries in the CrashReportContext across all frames in the page.

-- | Parameters of the 'CrashReportContext.getEntries' command.
data PCrashReportContextGetEntries = PCrashReportContextGetEntries
  deriving (Eq, Show)
pCrashReportContextGetEntries
  :: PCrashReportContextGetEntries
pCrashReportContextGetEntries
  = PCrashReportContextGetEntries
instance ToJSON PCrashReportContextGetEntries where
  toJSON _ = A.Null
data CrashReportContextGetEntries = CrashReportContextGetEntries
  {
    crashReportContextGetEntriesEntries :: [CrashReportContextCrashReportContextEntry]
  }
  deriving (Eq, Show)
instance FromJSON CrashReportContextGetEntries where
  parseJSON = A.withObject "CrashReportContextGetEntries" $ \o -> CrashReportContextGetEntries
    <$> o A..: "entries"
instance Command PCrashReportContextGetEntries where
  type CommandResponse PCrashReportContextGetEntries = CrashReportContextGetEntries
  commandName _ = "CrashReportContext.getEntries"

