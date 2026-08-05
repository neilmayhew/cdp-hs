{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}


{- |
= EventBreakpoints

EventBreakpoints permits setting JavaScript breakpoints on operations and events
occurring in native code invoked from JavaScript. Once breakpoint is hit, it is
reported through Debugger domain, similarly to regular breakpoints being hit.
-}


module CDP.Domains.EventBreakpoints (module CDP.Domains.EventBreakpoints) where

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




-- | Sets breakpoint on particular native event.

-- | Parameters of the 'EventBreakpoints.setInstrumentationBreakpoint' command.
data PEventBreakpointsSetInstrumentationBreakpoint = PEventBreakpointsSetInstrumentationBreakpoint
  {
    -- | Instrumentation name to stop on.
    pEventBreakpointsSetInstrumentationBreakpointEventName :: T.Text
  }
  deriving (Eq, Show)
pEventBreakpointsSetInstrumentationBreakpoint
  {-
  -- | Instrumentation name to stop on.
  -}
  :: T.Text
  -> PEventBreakpointsSetInstrumentationBreakpoint
pEventBreakpointsSetInstrumentationBreakpoint
  arg_pEventBreakpointsSetInstrumentationBreakpointEventName
  = PEventBreakpointsSetInstrumentationBreakpoint
    arg_pEventBreakpointsSetInstrumentationBreakpointEventName
instance ToJSON PEventBreakpointsSetInstrumentationBreakpoint where
  toJSON p = A.object $ catMaybes [
    ("eventName" A..=) <$> Just (pEventBreakpointsSetInstrumentationBreakpointEventName p)
    ]
instance Command PEventBreakpointsSetInstrumentationBreakpoint where
  type CommandResponse PEventBreakpointsSetInstrumentationBreakpoint = ()
  commandName _ = "EventBreakpoints.setInstrumentationBreakpoint"
  fromJSON = const . A.Success . const ()

-- | Removes breakpoint on particular native event.

-- | Parameters of the 'EventBreakpoints.removeInstrumentationBreakpoint' command.
data PEventBreakpointsRemoveInstrumentationBreakpoint = PEventBreakpointsRemoveInstrumentationBreakpoint
  {
    -- | Instrumentation name to stop on.
    pEventBreakpointsRemoveInstrumentationBreakpointEventName :: T.Text
  }
  deriving (Eq, Show)
pEventBreakpointsRemoveInstrumentationBreakpoint
  {-
  -- | Instrumentation name to stop on.
  -}
  :: T.Text
  -> PEventBreakpointsRemoveInstrumentationBreakpoint
pEventBreakpointsRemoveInstrumentationBreakpoint
  arg_pEventBreakpointsRemoveInstrumentationBreakpointEventName
  = PEventBreakpointsRemoveInstrumentationBreakpoint
    arg_pEventBreakpointsRemoveInstrumentationBreakpointEventName
instance ToJSON PEventBreakpointsRemoveInstrumentationBreakpoint where
  toJSON p = A.object $ catMaybes [
    ("eventName" A..=) <$> Just (pEventBreakpointsRemoveInstrumentationBreakpointEventName p)
    ]
instance Command PEventBreakpointsRemoveInstrumentationBreakpoint where
  type CommandResponse PEventBreakpointsRemoveInstrumentationBreakpoint = ()
  commandName _ = "EventBreakpoints.removeInstrumentationBreakpoint"
  fromJSON = const . A.Success . const ()

-- | Removes all breakpoints

-- | Parameters of the 'EventBreakpoints.disable' command.
data PEventBreakpointsDisable = PEventBreakpointsDisable
  deriving (Eq, Show)
pEventBreakpointsDisable
  :: PEventBreakpointsDisable
pEventBreakpointsDisable
  = PEventBreakpointsDisable
instance ToJSON PEventBreakpointsDisable where
  toJSON _ = A.Null
instance Command PEventBreakpointsDisable where
  type CommandResponse PEventBreakpointsDisable = ()
  commandName _ = "EventBreakpoints.disable"
  fromJSON = const . A.Success . const ()

