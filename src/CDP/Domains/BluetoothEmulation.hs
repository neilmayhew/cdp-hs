{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}


{- |
= BluetoothEmulation

This domain allows configuring virtual Bluetooth devices to test
the web-bluetooth API.
-}


module CDP.Domains.BluetoothEmulation (module CDP.Domains.BluetoothEmulation) where

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




-- | Type 'BluetoothEmulation.CentralState'.
--   Indicates the various states of Central.
data BluetoothEmulationCentralState = BluetoothEmulationCentralStateAbsent | BluetoothEmulationCentralStatePoweredOff | BluetoothEmulationCentralStatePoweredOn
  deriving (Ord, Eq, Show, Read)
instance FromJSON BluetoothEmulationCentralState where
  parseJSON = A.withText "BluetoothEmulationCentralState" $ \v -> case v of
    "absent" -> pure BluetoothEmulationCentralStateAbsent
    "powered-off" -> pure BluetoothEmulationCentralStatePoweredOff
    "powered-on" -> pure BluetoothEmulationCentralStatePoweredOn
    "_" -> fail "failed to parse BluetoothEmulationCentralState"
instance ToJSON BluetoothEmulationCentralState where
  toJSON v = A.String $ case v of
    BluetoothEmulationCentralStateAbsent -> "absent"
    BluetoothEmulationCentralStatePoweredOff -> "powered-off"
    BluetoothEmulationCentralStatePoweredOn -> "powered-on"

-- | Type 'BluetoothEmulation.GATTOperationType'.
--   Indicates the various types of GATT event.
data BluetoothEmulationGATTOperationType = BluetoothEmulationGATTOperationTypeConnection | BluetoothEmulationGATTOperationTypeDiscovery
  deriving (Ord, Eq, Show, Read)
instance FromJSON BluetoothEmulationGATTOperationType where
  parseJSON = A.withText "BluetoothEmulationGATTOperationType" $ \v -> case v of
    "connection" -> pure BluetoothEmulationGATTOperationTypeConnection
    "discovery" -> pure BluetoothEmulationGATTOperationTypeDiscovery
    "_" -> fail "failed to parse BluetoothEmulationGATTOperationType"
instance ToJSON BluetoothEmulationGATTOperationType where
  toJSON v = A.String $ case v of
    BluetoothEmulationGATTOperationTypeConnection -> "connection"
    BluetoothEmulationGATTOperationTypeDiscovery -> "discovery"

-- | Type 'BluetoothEmulation.CharacteristicWriteType'.
--   Indicates the various types of characteristic write.
data BluetoothEmulationCharacteristicWriteType = BluetoothEmulationCharacteristicWriteTypeWriteDefaultDeprecated | BluetoothEmulationCharacteristicWriteTypeWriteWithResponse | BluetoothEmulationCharacteristicWriteTypeWriteWithoutResponse
  deriving (Ord, Eq, Show, Read)
instance FromJSON BluetoothEmulationCharacteristicWriteType where
  parseJSON = A.withText "BluetoothEmulationCharacteristicWriteType" $ \v -> case v of
    "write-default-deprecated" -> pure BluetoothEmulationCharacteristicWriteTypeWriteDefaultDeprecated
    "write-with-response" -> pure BluetoothEmulationCharacteristicWriteTypeWriteWithResponse
    "write-without-response" -> pure BluetoothEmulationCharacteristicWriteTypeWriteWithoutResponse
    "_" -> fail "failed to parse BluetoothEmulationCharacteristicWriteType"
instance ToJSON BluetoothEmulationCharacteristicWriteType where
  toJSON v = A.String $ case v of
    BluetoothEmulationCharacteristicWriteTypeWriteDefaultDeprecated -> "write-default-deprecated"
    BluetoothEmulationCharacteristicWriteTypeWriteWithResponse -> "write-with-response"
    BluetoothEmulationCharacteristicWriteTypeWriteWithoutResponse -> "write-without-response"

-- | Type 'BluetoothEmulation.CharacteristicOperationType'.
--   Indicates the various types of characteristic operation.
data BluetoothEmulationCharacteristicOperationType = BluetoothEmulationCharacteristicOperationTypeRead | BluetoothEmulationCharacteristicOperationTypeWrite | BluetoothEmulationCharacteristicOperationTypeSubscribeToNotifications | BluetoothEmulationCharacteristicOperationTypeUnsubscribeFromNotifications
  deriving (Ord, Eq, Show, Read)
instance FromJSON BluetoothEmulationCharacteristicOperationType where
  parseJSON = A.withText "BluetoothEmulationCharacteristicOperationType" $ \v -> case v of
    "read" -> pure BluetoothEmulationCharacteristicOperationTypeRead
    "write" -> pure BluetoothEmulationCharacteristicOperationTypeWrite
    "subscribe-to-notifications" -> pure BluetoothEmulationCharacteristicOperationTypeSubscribeToNotifications
    "unsubscribe-from-notifications" -> pure BluetoothEmulationCharacteristicOperationTypeUnsubscribeFromNotifications
    "_" -> fail "failed to parse BluetoothEmulationCharacteristicOperationType"
instance ToJSON BluetoothEmulationCharacteristicOperationType where
  toJSON v = A.String $ case v of
    BluetoothEmulationCharacteristicOperationTypeRead -> "read"
    BluetoothEmulationCharacteristicOperationTypeWrite -> "write"
    BluetoothEmulationCharacteristicOperationTypeSubscribeToNotifications -> "subscribe-to-notifications"
    BluetoothEmulationCharacteristicOperationTypeUnsubscribeFromNotifications -> "unsubscribe-from-notifications"

-- | Type 'BluetoothEmulation.DescriptorOperationType'.
--   Indicates the various types of descriptor operation.
data BluetoothEmulationDescriptorOperationType = BluetoothEmulationDescriptorOperationTypeRead | BluetoothEmulationDescriptorOperationTypeWrite
  deriving (Ord, Eq, Show, Read)
instance FromJSON BluetoothEmulationDescriptorOperationType where
  parseJSON = A.withText "BluetoothEmulationDescriptorOperationType" $ \v -> case v of
    "read" -> pure BluetoothEmulationDescriptorOperationTypeRead
    "write" -> pure BluetoothEmulationDescriptorOperationTypeWrite
    "_" -> fail "failed to parse BluetoothEmulationDescriptorOperationType"
instance ToJSON BluetoothEmulationDescriptorOperationType where
  toJSON v = A.String $ case v of
    BluetoothEmulationDescriptorOperationTypeRead -> "read"
    BluetoothEmulationDescriptorOperationTypeWrite -> "write"

-- | Type 'BluetoothEmulation.ManufacturerData'.
--   Stores the manufacturer data
data BluetoothEmulationManufacturerData = BluetoothEmulationManufacturerData
  {
    -- | Company identifier
    --   https://bitbucket.org/bluetooth-SIG/public/src/main/assigned_numbers/company_identifiers/company_identifiers.yaml
    --   https://usb.org/developers
    bluetoothEmulationManufacturerDataKey :: Int,
    -- | Manufacturer-specific data (Encoded as a base64 string when passed over JSON)
    bluetoothEmulationManufacturerDataData :: T.Text
  }
  deriving (Eq, Show)
instance FromJSON BluetoothEmulationManufacturerData where
  parseJSON = A.withObject "BluetoothEmulationManufacturerData" $ \o -> BluetoothEmulationManufacturerData
    <$> o A..: "key"
    <*> o A..: "data"
instance ToJSON BluetoothEmulationManufacturerData where
  toJSON p = A.object $ catMaybes [
    ("key" A..=) <$> Just (bluetoothEmulationManufacturerDataKey p),
    ("data" A..=) <$> Just (bluetoothEmulationManufacturerDataData p)
    ]

-- | Type 'BluetoothEmulation.ScanRecord'.
--   Stores the byte data of the advertisement packet sent by a Bluetooth device.
data BluetoothEmulationScanRecord = BluetoothEmulationScanRecord
  {
    bluetoothEmulationScanRecordName :: Maybe T.Text,
    bluetoothEmulationScanRecordUuids :: Maybe [T.Text],
    -- | Stores the external appearance description of the device.
    bluetoothEmulationScanRecordAppearance :: Maybe Int,
    -- | Stores the transmission power of a broadcasting device.
    bluetoothEmulationScanRecordTxPower :: Maybe Int,
    -- | Key is the company identifier and the value is an array of bytes of
    --   manufacturer specific data.
    bluetoothEmulationScanRecordManufacturerData :: Maybe [BluetoothEmulationManufacturerData]
  }
  deriving (Eq, Show)
instance FromJSON BluetoothEmulationScanRecord where
  parseJSON = A.withObject "BluetoothEmulationScanRecord" $ \o -> BluetoothEmulationScanRecord
    <$> o A..:? "name"
    <*> o A..:? "uuids"
    <*> o A..:? "appearance"
    <*> o A..:? "txPower"
    <*> o A..:? "manufacturerData"
instance ToJSON BluetoothEmulationScanRecord where
  toJSON p = A.object $ catMaybes [
    ("name" A..=) <$> (bluetoothEmulationScanRecordName p),
    ("uuids" A..=) <$> (bluetoothEmulationScanRecordUuids p),
    ("appearance" A..=) <$> (bluetoothEmulationScanRecordAppearance p),
    ("txPower" A..=) <$> (bluetoothEmulationScanRecordTxPower p),
    ("manufacturerData" A..=) <$> (bluetoothEmulationScanRecordManufacturerData p)
    ]

-- | Type 'BluetoothEmulation.ScanEntry'.
--   Stores the advertisement packet information that is sent by a Bluetooth device.
data BluetoothEmulationScanEntry = BluetoothEmulationScanEntry
  {
    bluetoothEmulationScanEntryDeviceAddress :: T.Text,
    bluetoothEmulationScanEntryRssi :: Int,
    bluetoothEmulationScanEntryScanRecord :: BluetoothEmulationScanRecord
  }
  deriving (Eq, Show)
instance FromJSON BluetoothEmulationScanEntry where
  parseJSON = A.withObject "BluetoothEmulationScanEntry" $ \o -> BluetoothEmulationScanEntry
    <$> o A..: "deviceAddress"
    <*> o A..: "rssi"
    <*> o A..: "scanRecord"
instance ToJSON BluetoothEmulationScanEntry where
  toJSON p = A.object $ catMaybes [
    ("deviceAddress" A..=) <$> Just (bluetoothEmulationScanEntryDeviceAddress p),
    ("rssi" A..=) <$> Just (bluetoothEmulationScanEntryRssi p),
    ("scanRecord" A..=) <$> Just (bluetoothEmulationScanEntryScanRecord p)
    ]

-- | Type 'BluetoothEmulation.CharacteristicProperties'.
--   Describes the properties of a characteristic. This follows Bluetooth Core
--   Specification BT 4.2 Vol 3 Part G 3.3.1. Characteristic Properties.
data BluetoothEmulationCharacteristicProperties = BluetoothEmulationCharacteristicProperties
  {
    bluetoothEmulationCharacteristicPropertiesBroadcast :: Maybe Bool,
    bluetoothEmulationCharacteristicPropertiesRead :: Maybe Bool,
    bluetoothEmulationCharacteristicPropertiesWriteWithoutResponse :: Maybe Bool,
    bluetoothEmulationCharacteristicPropertiesWrite :: Maybe Bool,
    bluetoothEmulationCharacteristicPropertiesNotify :: Maybe Bool,
    bluetoothEmulationCharacteristicPropertiesIndicate :: Maybe Bool,
    bluetoothEmulationCharacteristicPropertiesAuthenticatedSignedWrites :: Maybe Bool,
    bluetoothEmulationCharacteristicPropertiesExtendedProperties :: Maybe Bool
  }
  deriving (Eq, Show)
instance FromJSON BluetoothEmulationCharacteristicProperties where
  parseJSON = A.withObject "BluetoothEmulationCharacteristicProperties" $ \o -> BluetoothEmulationCharacteristicProperties
    <$> o A..:? "broadcast"
    <*> o A..:? "read"
    <*> o A..:? "writeWithoutResponse"
    <*> o A..:? "write"
    <*> o A..:? "notify"
    <*> o A..:? "indicate"
    <*> o A..:? "authenticatedSignedWrites"
    <*> o A..:? "extendedProperties"
instance ToJSON BluetoothEmulationCharacteristicProperties where
  toJSON p = A.object $ catMaybes [
    ("broadcast" A..=) <$> (bluetoothEmulationCharacteristicPropertiesBroadcast p),
    ("read" A..=) <$> (bluetoothEmulationCharacteristicPropertiesRead p),
    ("writeWithoutResponse" A..=) <$> (bluetoothEmulationCharacteristicPropertiesWriteWithoutResponse p),
    ("write" A..=) <$> (bluetoothEmulationCharacteristicPropertiesWrite p),
    ("notify" A..=) <$> (bluetoothEmulationCharacteristicPropertiesNotify p),
    ("indicate" A..=) <$> (bluetoothEmulationCharacteristicPropertiesIndicate p),
    ("authenticatedSignedWrites" A..=) <$> (bluetoothEmulationCharacteristicPropertiesAuthenticatedSignedWrites p),
    ("extendedProperties" A..=) <$> (bluetoothEmulationCharacteristicPropertiesExtendedProperties p)
    ]

-- | Type of the 'BluetoothEmulation.gattOperationReceived' event.
data BluetoothEmulationGattOperationReceived = BluetoothEmulationGattOperationReceived
  {
    bluetoothEmulationGattOperationReceivedAddress :: T.Text,
    bluetoothEmulationGattOperationReceivedType :: BluetoothEmulationGATTOperationType
  }
  deriving (Eq, Show)
instance FromJSON BluetoothEmulationGattOperationReceived where
  parseJSON = A.withObject "BluetoothEmulationGattOperationReceived" $ \o -> BluetoothEmulationGattOperationReceived
    <$> o A..: "address"
    <*> o A..: "type"
instance Event BluetoothEmulationGattOperationReceived where
  eventName _ = "BluetoothEmulation.gattOperationReceived"

-- | Type of the 'BluetoothEmulation.characteristicOperationReceived' event.
data BluetoothEmulationCharacteristicOperationReceived = BluetoothEmulationCharacteristicOperationReceived
  {
    bluetoothEmulationCharacteristicOperationReceivedCharacteristicId :: T.Text,
    bluetoothEmulationCharacteristicOperationReceivedType :: BluetoothEmulationCharacteristicOperationType,
    bluetoothEmulationCharacteristicOperationReceivedData :: Maybe T.Text,
    bluetoothEmulationCharacteristicOperationReceivedWriteType :: Maybe BluetoothEmulationCharacteristicWriteType
  }
  deriving (Eq, Show)
instance FromJSON BluetoothEmulationCharacteristicOperationReceived where
  parseJSON = A.withObject "BluetoothEmulationCharacteristicOperationReceived" $ \o -> BluetoothEmulationCharacteristicOperationReceived
    <$> o A..: "characteristicId"
    <*> o A..: "type"
    <*> o A..:? "data"
    <*> o A..:? "writeType"
instance Event BluetoothEmulationCharacteristicOperationReceived where
  eventName _ = "BluetoothEmulation.characteristicOperationReceived"

-- | Type of the 'BluetoothEmulation.descriptorOperationReceived' event.
data BluetoothEmulationDescriptorOperationReceived = BluetoothEmulationDescriptorOperationReceived
  {
    bluetoothEmulationDescriptorOperationReceivedDescriptorId :: T.Text,
    bluetoothEmulationDescriptorOperationReceivedType :: BluetoothEmulationDescriptorOperationType,
    bluetoothEmulationDescriptorOperationReceivedData :: Maybe T.Text
  }
  deriving (Eq, Show)
instance FromJSON BluetoothEmulationDescriptorOperationReceived where
  parseJSON = A.withObject "BluetoothEmulationDescriptorOperationReceived" $ \o -> BluetoothEmulationDescriptorOperationReceived
    <$> o A..: "descriptorId"
    <*> o A..: "type"
    <*> o A..:? "data"
instance Event BluetoothEmulationDescriptorOperationReceived where
  eventName _ = "BluetoothEmulation.descriptorOperationReceived"

-- | Enable the BluetoothEmulation domain.

-- | Parameters of the 'BluetoothEmulation.enable' command.
data PBluetoothEmulationEnable = PBluetoothEmulationEnable
  {
    -- | State of the simulated central.
    pBluetoothEmulationEnableState :: BluetoothEmulationCentralState,
    -- | If the simulated central supports low-energy.
    pBluetoothEmulationEnableLeSupported :: Bool
  }
  deriving (Eq, Show)
pBluetoothEmulationEnable
  {-
  -- | State of the simulated central.
  -}
  :: BluetoothEmulationCentralState
  {-
  -- | If the simulated central supports low-energy.
  -}
  -> Bool
  -> PBluetoothEmulationEnable
pBluetoothEmulationEnable
  arg_pBluetoothEmulationEnableState
  arg_pBluetoothEmulationEnableLeSupported
  = PBluetoothEmulationEnable
    arg_pBluetoothEmulationEnableState
    arg_pBluetoothEmulationEnableLeSupported
instance ToJSON PBluetoothEmulationEnable where
  toJSON p = A.object $ catMaybes [
    ("state" A..=) <$> Just (pBluetoothEmulationEnableState p),
    ("leSupported" A..=) <$> Just (pBluetoothEmulationEnableLeSupported p)
    ]
instance Command PBluetoothEmulationEnable where
  type CommandResponse PBluetoothEmulationEnable = ()
  commandName _ = "BluetoothEmulation.enable"
  fromJSON = const . A.Success . const ()

-- | Set the state of the simulated central.

-- | Parameters of the 'BluetoothEmulation.setSimulatedCentralState' command.
data PBluetoothEmulationSetSimulatedCentralState = PBluetoothEmulationSetSimulatedCentralState
  {
    -- | State of the simulated central.
    pBluetoothEmulationSetSimulatedCentralStateState :: BluetoothEmulationCentralState
  }
  deriving (Eq, Show)
pBluetoothEmulationSetSimulatedCentralState
  {-
  -- | State of the simulated central.
  -}
  :: BluetoothEmulationCentralState
  -> PBluetoothEmulationSetSimulatedCentralState
pBluetoothEmulationSetSimulatedCentralState
  arg_pBluetoothEmulationSetSimulatedCentralStateState
  = PBluetoothEmulationSetSimulatedCentralState
    arg_pBluetoothEmulationSetSimulatedCentralStateState
instance ToJSON PBluetoothEmulationSetSimulatedCentralState where
  toJSON p = A.object $ catMaybes [
    ("state" A..=) <$> Just (pBluetoothEmulationSetSimulatedCentralStateState p)
    ]
instance Command PBluetoothEmulationSetSimulatedCentralState where
  type CommandResponse PBluetoothEmulationSetSimulatedCentralState = ()
  commandName _ = "BluetoothEmulation.setSimulatedCentralState"
  fromJSON = const . A.Success . const ()

-- | Disable the BluetoothEmulation domain.

-- | Parameters of the 'BluetoothEmulation.disable' command.
data PBluetoothEmulationDisable = PBluetoothEmulationDisable
  deriving (Eq, Show)
pBluetoothEmulationDisable
  :: PBluetoothEmulationDisable
pBluetoothEmulationDisable
  = PBluetoothEmulationDisable
instance ToJSON PBluetoothEmulationDisable where
  toJSON _ = A.Null
instance Command PBluetoothEmulationDisable where
  type CommandResponse PBluetoothEmulationDisable = ()
  commandName _ = "BluetoothEmulation.disable"
  fromJSON = const . A.Success . const ()

-- | Simulates a peripheral with |address|, |name| and |knownServiceUuids|
--   that has already been connected to the system.

-- | Parameters of the 'BluetoothEmulation.simulatePreconnectedPeripheral' command.
data PBluetoothEmulationSimulatePreconnectedPeripheral = PBluetoothEmulationSimulatePreconnectedPeripheral
  {
    pBluetoothEmulationSimulatePreconnectedPeripheralAddress :: T.Text,
    pBluetoothEmulationSimulatePreconnectedPeripheralName :: T.Text,
    pBluetoothEmulationSimulatePreconnectedPeripheralManufacturerData :: [BluetoothEmulationManufacturerData],
    pBluetoothEmulationSimulatePreconnectedPeripheralKnownServiceUuids :: [T.Text]
  }
  deriving (Eq, Show)
pBluetoothEmulationSimulatePreconnectedPeripheral
  :: T.Text
  -> T.Text
  -> [BluetoothEmulationManufacturerData]
  -> [T.Text]
  -> PBluetoothEmulationSimulatePreconnectedPeripheral
pBluetoothEmulationSimulatePreconnectedPeripheral
  arg_pBluetoothEmulationSimulatePreconnectedPeripheralAddress
  arg_pBluetoothEmulationSimulatePreconnectedPeripheralName
  arg_pBluetoothEmulationSimulatePreconnectedPeripheralManufacturerData
  arg_pBluetoothEmulationSimulatePreconnectedPeripheralKnownServiceUuids
  = PBluetoothEmulationSimulatePreconnectedPeripheral
    arg_pBluetoothEmulationSimulatePreconnectedPeripheralAddress
    arg_pBluetoothEmulationSimulatePreconnectedPeripheralName
    arg_pBluetoothEmulationSimulatePreconnectedPeripheralManufacturerData
    arg_pBluetoothEmulationSimulatePreconnectedPeripheralKnownServiceUuids
instance ToJSON PBluetoothEmulationSimulatePreconnectedPeripheral where
  toJSON p = A.object $ catMaybes [
    ("address" A..=) <$> Just (pBluetoothEmulationSimulatePreconnectedPeripheralAddress p),
    ("name" A..=) <$> Just (pBluetoothEmulationSimulatePreconnectedPeripheralName p),
    ("manufacturerData" A..=) <$> Just (pBluetoothEmulationSimulatePreconnectedPeripheralManufacturerData p),
    ("knownServiceUuids" A..=) <$> Just (pBluetoothEmulationSimulatePreconnectedPeripheralKnownServiceUuids p)
    ]
instance Command PBluetoothEmulationSimulatePreconnectedPeripheral where
  type CommandResponse PBluetoothEmulationSimulatePreconnectedPeripheral = ()
  commandName _ = "BluetoothEmulation.simulatePreconnectedPeripheral"
  fromJSON = const . A.Success . const ()

-- | Simulates an advertisement packet described in |entry| being received by
--   the central.

-- | Parameters of the 'BluetoothEmulation.simulateAdvertisement' command.
data PBluetoothEmulationSimulateAdvertisement = PBluetoothEmulationSimulateAdvertisement
  {
    pBluetoothEmulationSimulateAdvertisementEntry :: BluetoothEmulationScanEntry
  }
  deriving (Eq, Show)
pBluetoothEmulationSimulateAdvertisement
  :: BluetoothEmulationScanEntry
  -> PBluetoothEmulationSimulateAdvertisement
pBluetoothEmulationSimulateAdvertisement
  arg_pBluetoothEmulationSimulateAdvertisementEntry
  = PBluetoothEmulationSimulateAdvertisement
    arg_pBluetoothEmulationSimulateAdvertisementEntry
instance ToJSON PBluetoothEmulationSimulateAdvertisement where
  toJSON p = A.object $ catMaybes [
    ("entry" A..=) <$> Just (pBluetoothEmulationSimulateAdvertisementEntry p)
    ]
instance Command PBluetoothEmulationSimulateAdvertisement where
  type CommandResponse PBluetoothEmulationSimulateAdvertisement = ()
  commandName _ = "BluetoothEmulation.simulateAdvertisement"
  fromJSON = const . A.Success . const ()

-- | Simulates the response code from the peripheral with |address| for a
--   GATT operation of |type|. The |code| value follows the HCI Error Codes from
--   Bluetooth Core Specification Vol 2 Part D 1.3 List Of Error Codes.

-- | Parameters of the 'BluetoothEmulation.simulateGATTOperationResponse' command.
data PBluetoothEmulationSimulateGATTOperationResponse = PBluetoothEmulationSimulateGATTOperationResponse
  {
    pBluetoothEmulationSimulateGATTOperationResponseAddress :: T.Text,
    pBluetoothEmulationSimulateGATTOperationResponseType :: BluetoothEmulationGATTOperationType,
    pBluetoothEmulationSimulateGATTOperationResponseCode :: Int
  }
  deriving (Eq, Show)
pBluetoothEmulationSimulateGATTOperationResponse
  :: T.Text
  -> BluetoothEmulationGATTOperationType
  -> Int
  -> PBluetoothEmulationSimulateGATTOperationResponse
pBluetoothEmulationSimulateGATTOperationResponse
  arg_pBluetoothEmulationSimulateGATTOperationResponseAddress
  arg_pBluetoothEmulationSimulateGATTOperationResponseType
  arg_pBluetoothEmulationSimulateGATTOperationResponseCode
  = PBluetoothEmulationSimulateGATTOperationResponse
    arg_pBluetoothEmulationSimulateGATTOperationResponseAddress
    arg_pBluetoothEmulationSimulateGATTOperationResponseType
    arg_pBluetoothEmulationSimulateGATTOperationResponseCode
instance ToJSON PBluetoothEmulationSimulateGATTOperationResponse where
  toJSON p = A.object $ catMaybes [
    ("address" A..=) <$> Just (pBluetoothEmulationSimulateGATTOperationResponseAddress p),
    ("type" A..=) <$> Just (pBluetoothEmulationSimulateGATTOperationResponseType p),
    ("code" A..=) <$> Just (pBluetoothEmulationSimulateGATTOperationResponseCode p)
    ]
instance Command PBluetoothEmulationSimulateGATTOperationResponse where
  type CommandResponse PBluetoothEmulationSimulateGATTOperationResponse = ()
  commandName _ = "BluetoothEmulation.simulateGATTOperationResponse"
  fromJSON = const . A.Success . const ()

-- | Simulates the response from the characteristic with |characteristicId| for a
--   characteristic operation of |type|. The |code| value follows the Error
--   Codes from Bluetooth Core Specification Vol 3 Part F 3.4.1.1 Error Response.
--   The |data| is expected to exist when simulating a successful read operation
--   response.

-- | Parameters of the 'BluetoothEmulation.simulateCharacteristicOperationResponse' command.
data PBluetoothEmulationSimulateCharacteristicOperationResponse = PBluetoothEmulationSimulateCharacteristicOperationResponse
  {
    pBluetoothEmulationSimulateCharacteristicOperationResponseCharacteristicId :: T.Text,
    pBluetoothEmulationSimulateCharacteristicOperationResponseType :: BluetoothEmulationCharacteristicOperationType,
    pBluetoothEmulationSimulateCharacteristicOperationResponseCode :: Int,
    pBluetoothEmulationSimulateCharacteristicOperationResponseData :: Maybe T.Text
  }
  deriving (Eq, Show)
pBluetoothEmulationSimulateCharacteristicOperationResponse
  :: T.Text
  -> BluetoothEmulationCharacteristicOperationType
  -> Int
  -> PBluetoothEmulationSimulateCharacteristicOperationResponse
pBluetoothEmulationSimulateCharacteristicOperationResponse
  arg_pBluetoothEmulationSimulateCharacteristicOperationResponseCharacteristicId
  arg_pBluetoothEmulationSimulateCharacteristicOperationResponseType
  arg_pBluetoothEmulationSimulateCharacteristicOperationResponseCode
  = PBluetoothEmulationSimulateCharacteristicOperationResponse
    arg_pBluetoothEmulationSimulateCharacteristicOperationResponseCharacteristicId
    arg_pBluetoothEmulationSimulateCharacteristicOperationResponseType
    arg_pBluetoothEmulationSimulateCharacteristicOperationResponseCode
    Nothing
instance ToJSON PBluetoothEmulationSimulateCharacteristicOperationResponse where
  toJSON p = A.object $ catMaybes [
    ("characteristicId" A..=) <$> Just (pBluetoothEmulationSimulateCharacteristicOperationResponseCharacteristicId p),
    ("type" A..=) <$> Just (pBluetoothEmulationSimulateCharacteristicOperationResponseType p),
    ("code" A..=) <$> Just (pBluetoothEmulationSimulateCharacteristicOperationResponseCode p),
    ("data" A..=) <$> (pBluetoothEmulationSimulateCharacteristicOperationResponseData p)
    ]
instance Command PBluetoothEmulationSimulateCharacteristicOperationResponse where
  type CommandResponse PBluetoothEmulationSimulateCharacteristicOperationResponse = ()
  commandName _ = "BluetoothEmulation.simulateCharacteristicOperationResponse"
  fromJSON = const . A.Success . const ()

-- | Simulates the response from the descriptor with |descriptorId| for a
--   descriptor operation of |type|. The |code| value follows the Error
--   Codes from Bluetooth Core Specification Vol 3 Part F 3.4.1.1 Error Response.
--   The |data| is expected to exist when simulating a successful read operation
--   response.

-- | Parameters of the 'BluetoothEmulation.simulateDescriptorOperationResponse' command.
data PBluetoothEmulationSimulateDescriptorOperationResponse = PBluetoothEmulationSimulateDescriptorOperationResponse
  {
    pBluetoothEmulationSimulateDescriptorOperationResponseDescriptorId :: T.Text,
    pBluetoothEmulationSimulateDescriptorOperationResponseType :: BluetoothEmulationDescriptorOperationType,
    pBluetoothEmulationSimulateDescriptorOperationResponseCode :: Int,
    pBluetoothEmulationSimulateDescriptorOperationResponseData :: Maybe T.Text
  }
  deriving (Eq, Show)
pBluetoothEmulationSimulateDescriptorOperationResponse
  :: T.Text
  -> BluetoothEmulationDescriptorOperationType
  -> Int
  -> PBluetoothEmulationSimulateDescriptorOperationResponse
pBluetoothEmulationSimulateDescriptorOperationResponse
  arg_pBluetoothEmulationSimulateDescriptorOperationResponseDescriptorId
  arg_pBluetoothEmulationSimulateDescriptorOperationResponseType
  arg_pBluetoothEmulationSimulateDescriptorOperationResponseCode
  = PBluetoothEmulationSimulateDescriptorOperationResponse
    arg_pBluetoothEmulationSimulateDescriptorOperationResponseDescriptorId
    arg_pBluetoothEmulationSimulateDescriptorOperationResponseType
    arg_pBluetoothEmulationSimulateDescriptorOperationResponseCode
    Nothing
instance ToJSON PBluetoothEmulationSimulateDescriptorOperationResponse where
  toJSON p = A.object $ catMaybes [
    ("descriptorId" A..=) <$> Just (pBluetoothEmulationSimulateDescriptorOperationResponseDescriptorId p),
    ("type" A..=) <$> Just (pBluetoothEmulationSimulateDescriptorOperationResponseType p),
    ("code" A..=) <$> Just (pBluetoothEmulationSimulateDescriptorOperationResponseCode p),
    ("data" A..=) <$> (pBluetoothEmulationSimulateDescriptorOperationResponseData p)
    ]
instance Command PBluetoothEmulationSimulateDescriptorOperationResponse where
  type CommandResponse PBluetoothEmulationSimulateDescriptorOperationResponse = ()
  commandName _ = "BluetoothEmulation.simulateDescriptorOperationResponse"
  fromJSON = const . A.Success . const ()

-- | Adds a service with |serviceUuid| to the peripheral with |address|.

-- | Parameters of the 'BluetoothEmulation.addService' command.
data PBluetoothEmulationAddService = PBluetoothEmulationAddService
  {
    pBluetoothEmulationAddServiceAddress :: T.Text,
    pBluetoothEmulationAddServiceServiceUuid :: T.Text
  }
  deriving (Eq, Show)
pBluetoothEmulationAddService
  :: T.Text
  -> T.Text
  -> PBluetoothEmulationAddService
pBluetoothEmulationAddService
  arg_pBluetoothEmulationAddServiceAddress
  arg_pBluetoothEmulationAddServiceServiceUuid
  = PBluetoothEmulationAddService
    arg_pBluetoothEmulationAddServiceAddress
    arg_pBluetoothEmulationAddServiceServiceUuid
instance ToJSON PBluetoothEmulationAddService where
  toJSON p = A.object $ catMaybes [
    ("address" A..=) <$> Just (pBluetoothEmulationAddServiceAddress p),
    ("serviceUuid" A..=) <$> Just (pBluetoothEmulationAddServiceServiceUuid p)
    ]
data BluetoothEmulationAddService = BluetoothEmulationAddService
  {
    -- | An identifier that uniquely represents this service.
    bluetoothEmulationAddServiceServiceId :: T.Text
  }
  deriving (Eq, Show)
instance FromJSON BluetoothEmulationAddService where
  parseJSON = A.withObject "BluetoothEmulationAddService" $ \o -> BluetoothEmulationAddService
    <$> o A..: "serviceId"
instance Command PBluetoothEmulationAddService where
  type CommandResponse PBluetoothEmulationAddService = BluetoothEmulationAddService
  commandName _ = "BluetoothEmulation.addService"

-- | Removes the service respresented by |serviceId| from the simulated central.

-- | Parameters of the 'BluetoothEmulation.removeService' command.
data PBluetoothEmulationRemoveService = PBluetoothEmulationRemoveService
  {
    pBluetoothEmulationRemoveServiceServiceId :: T.Text
  }
  deriving (Eq, Show)
pBluetoothEmulationRemoveService
  :: T.Text
  -> PBluetoothEmulationRemoveService
pBluetoothEmulationRemoveService
  arg_pBluetoothEmulationRemoveServiceServiceId
  = PBluetoothEmulationRemoveService
    arg_pBluetoothEmulationRemoveServiceServiceId
instance ToJSON PBluetoothEmulationRemoveService where
  toJSON p = A.object $ catMaybes [
    ("serviceId" A..=) <$> Just (pBluetoothEmulationRemoveServiceServiceId p)
    ]
instance Command PBluetoothEmulationRemoveService where
  type CommandResponse PBluetoothEmulationRemoveService = ()
  commandName _ = "BluetoothEmulation.removeService"
  fromJSON = const . A.Success . const ()

-- | Adds a characteristic with |characteristicUuid| and |properties| to the
--   service represented by |serviceId|.

-- | Parameters of the 'BluetoothEmulation.addCharacteristic' command.
data PBluetoothEmulationAddCharacteristic = PBluetoothEmulationAddCharacteristic
  {
    pBluetoothEmulationAddCharacteristicServiceId :: T.Text,
    pBluetoothEmulationAddCharacteristicCharacteristicUuid :: T.Text,
    pBluetoothEmulationAddCharacteristicProperties :: BluetoothEmulationCharacteristicProperties
  }
  deriving (Eq, Show)
pBluetoothEmulationAddCharacteristic
  :: T.Text
  -> T.Text
  -> BluetoothEmulationCharacteristicProperties
  -> PBluetoothEmulationAddCharacteristic
pBluetoothEmulationAddCharacteristic
  arg_pBluetoothEmulationAddCharacteristicServiceId
  arg_pBluetoothEmulationAddCharacteristicCharacteristicUuid
  arg_pBluetoothEmulationAddCharacteristicProperties
  = PBluetoothEmulationAddCharacteristic
    arg_pBluetoothEmulationAddCharacteristicServiceId
    arg_pBluetoothEmulationAddCharacteristicCharacteristicUuid
    arg_pBluetoothEmulationAddCharacteristicProperties
instance ToJSON PBluetoothEmulationAddCharacteristic where
  toJSON p = A.object $ catMaybes [
    ("serviceId" A..=) <$> Just (pBluetoothEmulationAddCharacteristicServiceId p),
    ("characteristicUuid" A..=) <$> Just (pBluetoothEmulationAddCharacteristicCharacteristicUuid p),
    ("properties" A..=) <$> Just (pBluetoothEmulationAddCharacteristicProperties p)
    ]
data BluetoothEmulationAddCharacteristic = BluetoothEmulationAddCharacteristic
  {
    -- | An identifier that uniquely represents this characteristic.
    bluetoothEmulationAddCharacteristicCharacteristicId :: T.Text
  }
  deriving (Eq, Show)
instance FromJSON BluetoothEmulationAddCharacteristic where
  parseJSON = A.withObject "BluetoothEmulationAddCharacteristic" $ \o -> BluetoothEmulationAddCharacteristic
    <$> o A..: "characteristicId"
instance Command PBluetoothEmulationAddCharacteristic where
  type CommandResponse PBluetoothEmulationAddCharacteristic = BluetoothEmulationAddCharacteristic
  commandName _ = "BluetoothEmulation.addCharacteristic"

-- | Removes the characteristic respresented by |characteristicId| from the
--   simulated central.

-- | Parameters of the 'BluetoothEmulation.removeCharacteristic' command.
data PBluetoothEmulationRemoveCharacteristic = PBluetoothEmulationRemoveCharacteristic
  {
    pBluetoothEmulationRemoveCharacteristicCharacteristicId :: T.Text
  }
  deriving (Eq, Show)
pBluetoothEmulationRemoveCharacteristic
  :: T.Text
  -> PBluetoothEmulationRemoveCharacteristic
pBluetoothEmulationRemoveCharacteristic
  arg_pBluetoothEmulationRemoveCharacteristicCharacteristicId
  = PBluetoothEmulationRemoveCharacteristic
    arg_pBluetoothEmulationRemoveCharacteristicCharacteristicId
instance ToJSON PBluetoothEmulationRemoveCharacteristic where
  toJSON p = A.object $ catMaybes [
    ("characteristicId" A..=) <$> Just (pBluetoothEmulationRemoveCharacteristicCharacteristicId p)
    ]
instance Command PBluetoothEmulationRemoveCharacteristic where
  type CommandResponse PBluetoothEmulationRemoveCharacteristic = ()
  commandName _ = "BluetoothEmulation.removeCharacteristic"
  fromJSON = const . A.Success . const ()

-- | Adds a descriptor with |descriptorUuid| to the characteristic respresented
--   by |characteristicId|.

-- | Parameters of the 'BluetoothEmulation.addDescriptor' command.
data PBluetoothEmulationAddDescriptor = PBluetoothEmulationAddDescriptor
  {
    pBluetoothEmulationAddDescriptorCharacteristicId :: T.Text,
    pBluetoothEmulationAddDescriptorDescriptorUuid :: T.Text
  }
  deriving (Eq, Show)
pBluetoothEmulationAddDescriptor
  :: T.Text
  -> T.Text
  -> PBluetoothEmulationAddDescriptor
pBluetoothEmulationAddDescriptor
  arg_pBluetoothEmulationAddDescriptorCharacteristicId
  arg_pBluetoothEmulationAddDescriptorDescriptorUuid
  = PBluetoothEmulationAddDescriptor
    arg_pBluetoothEmulationAddDescriptorCharacteristicId
    arg_pBluetoothEmulationAddDescriptorDescriptorUuid
instance ToJSON PBluetoothEmulationAddDescriptor where
  toJSON p = A.object $ catMaybes [
    ("characteristicId" A..=) <$> Just (pBluetoothEmulationAddDescriptorCharacteristicId p),
    ("descriptorUuid" A..=) <$> Just (pBluetoothEmulationAddDescriptorDescriptorUuid p)
    ]
data BluetoothEmulationAddDescriptor = BluetoothEmulationAddDescriptor
  {
    -- | An identifier that uniquely represents this descriptor.
    bluetoothEmulationAddDescriptorDescriptorId :: T.Text
  }
  deriving (Eq, Show)
instance FromJSON BluetoothEmulationAddDescriptor where
  parseJSON = A.withObject "BluetoothEmulationAddDescriptor" $ \o -> BluetoothEmulationAddDescriptor
    <$> o A..: "descriptorId"
instance Command PBluetoothEmulationAddDescriptor where
  type CommandResponse PBluetoothEmulationAddDescriptor = BluetoothEmulationAddDescriptor
  commandName _ = "BluetoothEmulation.addDescriptor"

-- | Removes the descriptor with |descriptorId| from the simulated central.

-- | Parameters of the 'BluetoothEmulation.removeDescriptor' command.
data PBluetoothEmulationRemoveDescriptor = PBluetoothEmulationRemoveDescriptor
  {
    pBluetoothEmulationRemoveDescriptorDescriptorId :: T.Text
  }
  deriving (Eq, Show)
pBluetoothEmulationRemoveDescriptor
  :: T.Text
  -> PBluetoothEmulationRemoveDescriptor
pBluetoothEmulationRemoveDescriptor
  arg_pBluetoothEmulationRemoveDescriptorDescriptorId
  = PBluetoothEmulationRemoveDescriptor
    arg_pBluetoothEmulationRemoveDescriptorDescriptorId
instance ToJSON PBluetoothEmulationRemoveDescriptor where
  toJSON p = A.object $ catMaybes [
    ("descriptorId" A..=) <$> Just (pBluetoothEmulationRemoveDescriptorDescriptorId p)
    ]
instance Command PBluetoothEmulationRemoveDescriptor where
  type CommandResponse PBluetoothEmulationRemoveDescriptor = ()
  commandName _ = "BluetoothEmulation.removeDescriptor"
  fromJSON = const . A.Success . const ()

-- | Simulates a GATT disconnection from the peripheral with |address|.

-- | Parameters of the 'BluetoothEmulation.simulateGATTDisconnection' command.
data PBluetoothEmulationSimulateGATTDisconnection = PBluetoothEmulationSimulateGATTDisconnection
  {
    pBluetoothEmulationSimulateGATTDisconnectionAddress :: T.Text
  }
  deriving (Eq, Show)
pBluetoothEmulationSimulateGATTDisconnection
  :: T.Text
  -> PBluetoothEmulationSimulateGATTDisconnection
pBluetoothEmulationSimulateGATTDisconnection
  arg_pBluetoothEmulationSimulateGATTDisconnectionAddress
  = PBluetoothEmulationSimulateGATTDisconnection
    arg_pBluetoothEmulationSimulateGATTDisconnectionAddress
instance ToJSON PBluetoothEmulationSimulateGATTDisconnection where
  toJSON p = A.object $ catMaybes [
    ("address" A..=) <$> Just (pBluetoothEmulationSimulateGATTDisconnectionAddress p)
    ]
instance Command PBluetoothEmulationSimulateGATTDisconnection where
  type CommandResponse PBluetoothEmulationSimulateGATTDisconnection = ()
  commandName _ = "BluetoothEmulation.simulateGATTDisconnection"
  fromJSON = const . A.Success . const ()

