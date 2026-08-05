{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}


{- |
= DeviceAccess

-}


module CDP.Domains.DeviceAccess (module CDP.Domains.DeviceAccess) where

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




-- | Type 'DeviceAccess.RequestId'.
--   Device request id.
type DeviceAccessRequestId = T.Text

-- | Type 'DeviceAccess.DeviceId'.
--   A device id.
type DeviceAccessDeviceId = T.Text

-- | Type 'DeviceAccess.PromptDevice'.
--   Device information displayed in a user prompt to select a device.
data DeviceAccessPromptDevice = DeviceAccessPromptDevice
  {
    deviceAccessPromptDeviceId :: DeviceAccessDeviceId,
    -- | Display name as it appears in a device request user prompt.
    deviceAccessPromptDeviceName :: T.Text
  }
  deriving (Eq, Show)
instance FromJSON DeviceAccessPromptDevice where
  parseJSON = A.withObject "DeviceAccessPromptDevice" $ \o -> DeviceAccessPromptDevice
    <$> o A..: "id"
    <*> o A..: "name"
instance ToJSON DeviceAccessPromptDevice where
  toJSON p = A.object $ catMaybes [
    ("id" A..=) <$> Just (deviceAccessPromptDeviceId p),
    ("name" A..=) <$> Just (deviceAccessPromptDeviceName p)
    ]

-- | Type of the 'DeviceAccess.deviceRequestPrompted' event.
data DeviceAccessDeviceRequestPrompted = DeviceAccessDeviceRequestPrompted
  {
    deviceAccessDeviceRequestPromptedId :: DeviceAccessRequestId,
    deviceAccessDeviceRequestPromptedDevices :: [DeviceAccessPromptDevice]
  }
  deriving (Eq, Show)
instance FromJSON DeviceAccessDeviceRequestPrompted where
  parseJSON = A.withObject "DeviceAccessDeviceRequestPrompted" $ \o -> DeviceAccessDeviceRequestPrompted
    <$> o A..: "id"
    <*> o A..: "devices"
instance Event DeviceAccessDeviceRequestPrompted where
  eventName _ = "DeviceAccess.deviceRequestPrompted"

-- | Enable events in this domain.

-- | Parameters of the 'DeviceAccess.enable' command.
data PDeviceAccessEnable = PDeviceAccessEnable
  deriving (Eq, Show)
pDeviceAccessEnable
  :: PDeviceAccessEnable
pDeviceAccessEnable
  = PDeviceAccessEnable
instance ToJSON PDeviceAccessEnable where
  toJSON _ = A.Null
instance Command PDeviceAccessEnable where
  type CommandResponse PDeviceAccessEnable = ()
  commandName _ = "DeviceAccess.enable"
  fromJSON = const . A.Success . const ()

-- | Disable events in this domain.

-- | Parameters of the 'DeviceAccess.disable' command.
data PDeviceAccessDisable = PDeviceAccessDisable
  deriving (Eq, Show)
pDeviceAccessDisable
  :: PDeviceAccessDisable
pDeviceAccessDisable
  = PDeviceAccessDisable
instance ToJSON PDeviceAccessDisable where
  toJSON _ = A.Null
instance Command PDeviceAccessDisable where
  type CommandResponse PDeviceAccessDisable = ()
  commandName _ = "DeviceAccess.disable"
  fromJSON = const . A.Success . const ()

-- | Select a device in response to a DeviceAccess.deviceRequestPrompted event.

-- | Parameters of the 'DeviceAccess.selectPrompt' command.
data PDeviceAccessSelectPrompt = PDeviceAccessSelectPrompt
  {
    pDeviceAccessSelectPromptId :: DeviceAccessRequestId,
    pDeviceAccessSelectPromptDeviceId :: DeviceAccessDeviceId
  }
  deriving (Eq, Show)
pDeviceAccessSelectPrompt
  :: DeviceAccessRequestId
  -> DeviceAccessDeviceId
  -> PDeviceAccessSelectPrompt
pDeviceAccessSelectPrompt
  arg_pDeviceAccessSelectPromptId
  arg_pDeviceAccessSelectPromptDeviceId
  = PDeviceAccessSelectPrompt
    arg_pDeviceAccessSelectPromptId
    arg_pDeviceAccessSelectPromptDeviceId
instance ToJSON PDeviceAccessSelectPrompt where
  toJSON p = A.object $ catMaybes [
    ("id" A..=) <$> Just (pDeviceAccessSelectPromptId p),
    ("deviceId" A..=) <$> Just (pDeviceAccessSelectPromptDeviceId p)
    ]
instance Command PDeviceAccessSelectPrompt where
  type CommandResponse PDeviceAccessSelectPrompt = ()
  commandName _ = "DeviceAccess.selectPrompt"
  fromJSON = const . A.Success . const ()

-- | Cancel a prompt in response to a DeviceAccess.deviceRequestPrompted event.

-- | Parameters of the 'DeviceAccess.cancelPrompt' command.
data PDeviceAccessCancelPrompt = PDeviceAccessCancelPrompt
  {
    pDeviceAccessCancelPromptId :: DeviceAccessRequestId
  }
  deriving (Eq, Show)
pDeviceAccessCancelPrompt
  :: DeviceAccessRequestId
  -> PDeviceAccessCancelPrompt
pDeviceAccessCancelPrompt
  arg_pDeviceAccessCancelPromptId
  = PDeviceAccessCancelPrompt
    arg_pDeviceAccessCancelPromptId
instance ToJSON PDeviceAccessCancelPrompt where
  toJSON p = A.object $ catMaybes [
    ("id" A..=) <$> Just (pDeviceAccessCancelPromptId p)
    ]
instance Command PDeviceAccessCancelPrompt where
  type CommandResponse PDeviceAccessCancelPrompt = ()
  commandName _ = "DeviceAccess.cancelPrompt"
  fromJSON = const . A.Success . const ()

