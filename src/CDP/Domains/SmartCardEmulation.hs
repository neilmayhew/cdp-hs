{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}


{- |
= SmartCardEmulation

-}


module CDP.Domains.SmartCardEmulation (module CDP.Domains.SmartCardEmulation) where

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




-- | Type 'SmartCardEmulation.ResultCode'.
--   Indicates the PC/SC error code.
--   
--   This maps to:
--   PC/SC Lite: https://pcsclite.apdu.fr/api/group__ErrorCodes.html
--   Microsoft: https://learn.microsoft.com/en-us/windows/win32/secauthn/authentication-return-values
data SmartCardEmulationResultCode = SmartCardEmulationResultCodeSuccess | SmartCardEmulationResultCodeRemovedCard | SmartCardEmulationResultCodeResetCard | SmartCardEmulationResultCodeUnpoweredCard | SmartCardEmulationResultCodeUnresponsiveCard | SmartCardEmulationResultCodeUnsupportedCard | SmartCardEmulationResultCodeReaderUnavailable | SmartCardEmulationResultCodeSharingViolation | SmartCardEmulationResultCodeNotTransacted | SmartCardEmulationResultCodeNoSmartcard | SmartCardEmulationResultCodeProtoMismatch | SmartCardEmulationResultCodeSystemCancelled | SmartCardEmulationResultCodeNotReady | SmartCardEmulationResultCodeCancelled | SmartCardEmulationResultCodeInsufficientBuffer | SmartCardEmulationResultCodeInvalidHandle | SmartCardEmulationResultCodeInvalidParameter | SmartCardEmulationResultCodeInvalidValue | SmartCardEmulationResultCodeNoMemory | SmartCardEmulationResultCodeTimeout | SmartCardEmulationResultCodeUnknownReader | SmartCardEmulationResultCodeUnsupportedFeature | SmartCardEmulationResultCodeNoReadersAvailable | SmartCardEmulationResultCodeServiceStopped | SmartCardEmulationResultCodeNoService | SmartCardEmulationResultCodeCommError | SmartCardEmulationResultCodeInternalError | SmartCardEmulationResultCodeServerTooBusy | SmartCardEmulationResultCodeUnexpected | SmartCardEmulationResultCodeShutdown | SmartCardEmulationResultCodeUnknownCard | SmartCardEmulationResultCodeUnknown
  deriving (Ord, Eq, Show, Read)
instance FromJSON SmartCardEmulationResultCode where
  parseJSON = A.withText "SmartCardEmulationResultCode" $ \v -> case v of
    "success" -> pure SmartCardEmulationResultCodeSuccess
    "removed-card" -> pure SmartCardEmulationResultCodeRemovedCard
    "reset-card" -> pure SmartCardEmulationResultCodeResetCard
    "unpowered-card" -> pure SmartCardEmulationResultCodeUnpoweredCard
    "unresponsive-card" -> pure SmartCardEmulationResultCodeUnresponsiveCard
    "unsupported-card" -> pure SmartCardEmulationResultCodeUnsupportedCard
    "reader-unavailable" -> pure SmartCardEmulationResultCodeReaderUnavailable
    "sharing-violation" -> pure SmartCardEmulationResultCodeSharingViolation
    "not-transacted" -> pure SmartCardEmulationResultCodeNotTransacted
    "no-smartcard" -> pure SmartCardEmulationResultCodeNoSmartcard
    "proto-mismatch" -> pure SmartCardEmulationResultCodeProtoMismatch
    "system-cancelled" -> pure SmartCardEmulationResultCodeSystemCancelled
    "not-ready" -> pure SmartCardEmulationResultCodeNotReady
    "cancelled" -> pure SmartCardEmulationResultCodeCancelled
    "insufficient-buffer" -> pure SmartCardEmulationResultCodeInsufficientBuffer
    "invalid-handle" -> pure SmartCardEmulationResultCodeInvalidHandle
    "invalid-parameter" -> pure SmartCardEmulationResultCodeInvalidParameter
    "invalid-value" -> pure SmartCardEmulationResultCodeInvalidValue
    "no-memory" -> pure SmartCardEmulationResultCodeNoMemory
    "timeout" -> pure SmartCardEmulationResultCodeTimeout
    "unknown-reader" -> pure SmartCardEmulationResultCodeUnknownReader
    "unsupported-feature" -> pure SmartCardEmulationResultCodeUnsupportedFeature
    "no-readers-available" -> pure SmartCardEmulationResultCodeNoReadersAvailable
    "service-stopped" -> pure SmartCardEmulationResultCodeServiceStopped
    "no-service" -> pure SmartCardEmulationResultCodeNoService
    "comm-error" -> pure SmartCardEmulationResultCodeCommError
    "internal-error" -> pure SmartCardEmulationResultCodeInternalError
    "server-too-busy" -> pure SmartCardEmulationResultCodeServerTooBusy
    "unexpected" -> pure SmartCardEmulationResultCodeUnexpected
    "shutdown" -> pure SmartCardEmulationResultCodeShutdown
    "unknown-card" -> pure SmartCardEmulationResultCodeUnknownCard
    "unknown" -> pure SmartCardEmulationResultCodeUnknown
    "_" -> fail "failed to parse SmartCardEmulationResultCode"
instance ToJSON SmartCardEmulationResultCode where
  toJSON v = A.String $ case v of
    SmartCardEmulationResultCodeSuccess -> "success"
    SmartCardEmulationResultCodeRemovedCard -> "removed-card"
    SmartCardEmulationResultCodeResetCard -> "reset-card"
    SmartCardEmulationResultCodeUnpoweredCard -> "unpowered-card"
    SmartCardEmulationResultCodeUnresponsiveCard -> "unresponsive-card"
    SmartCardEmulationResultCodeUnsupportedCard -> "unsupported-card"
    SmartCardEmulationResultCodeReaderUnavailable -> "reader-unavailable"
    SmartCardEmulationResultCodeSharingViolation -> "sharing-violation"
    SmartCardEmulationResultCodeNotTransacted -> "not-transacted"
    SmartCardEmulationResultCodeNoSmartcard -> "no-smartcard"
    SmartCardEmulationResultCodeProtoMismatch -> "proto-mismatch"
    SmartCardEmulationResultCodeSystemCancelled -> "system-cancelled"
    SmartCardEmulationResultCodeNotReady -> "not-ready"
    SmartCardEmulationResultCodeCancelled -> "cancelled"
    SmartCardEmulationResultCodeInsufficientBuffer -> "insufficient-buffer"
    SmartCardEmulationResultCodeInvalidHandle -> "invalid-handle"
    SmartCardEmulationResultCodeInvalidParameter -> "invalid-parameter"
    SmartCardEmulationResultCodeInvalidValue -> "invalid-value"
    SmartCardEmulationResultCodeNoMemory -> "no-memory"
    SmartCardEmulationResultCodeTimeout -> "timeout"
    SmartCardEmulationResultCodeUnknownReader -> "unknown-reader"
    SmartCardEmulationResultCodeUnsupportedFeature -> "unsupported-feature"
    SmartCardEmulationResultCodeNoReadersAvailable -> "no-readers-available"
    SmartCardEmulationResultCodeServiceStopped -> "service-stopped"
    SmartCardEmulationResultCodeNoService -> "no-service"
    SmartCardEmulationResultCodeCommError -> "comm-error"
    SmartCardEmulationResultCodeInternalError -> "internal-error"
    SmartCardEmulationResultCodeServerTooBusy -> "server-too-busy"
    SmartCardEmulationResultCodeUnexpected -> "unexpected"
    SmartCardEmulationResultCodeShutdown -> "shutdown"
    SmartCardEmulationResultCodeUnknownCard -> "unknown-card"
    SmartCardEmulationResultCodeUnknown -> "unknown"

-- | Type 'SmartCardEmulation.ShareMode'.
--   Maps to the |SCARD_SHARE_*| values.
data SmartCardEmulationShareMode = SmartCardEmulationShareModeShared | SmartCardEmulationShareModeExclusive | SmartCardEmulationShareModeDirect
  deriving (Ord, Eq, Show, Read)
instance FromJSON SmartCardEmulationShareMode where
  parseJSON = A.withText "SmartCardEmulationShareMode" $ \v -> case v of
    "shared" -> pure SmartCardEmulationShareModeShared
    "exclusive" -> pure SmartCardEmulationShareModeExclusive
    "direct" -> pure SmartCardEmulationShareModeDirect
    "_" -> fail "failed to parse SmartCardEmulationShareMode"
instance ToJSON SmartCardEmulationShareMode where
  toJSON v = A.String $ case v of
    SmartCardEmulationShareModeShared -> "shared"
    SmartCardEmulationShareModeExclusive -> "exclusive"
    SmartCardEmulationShareModeDirect -> "direct"

-- | Type 'SmartCardEmulation.Disposition'.
--   Indicates what the reader should do with the card.
data SmartCardEmulationDisposition = SmartCardEmulationDispositionLeaveCard | SmartCardEmulationDispositionResetCard | SmartCardEmulationDispositionUnpowerCard | SmartCardEmulationDispositionEjectCard
  deriving (Ord, Eq, Show, Read)
instance FromJSON SmartCardEmulationDisposition where
  parseJSON = A.withText "SmartCardEmulationDisposition" $ \v -> case v of
    "leave-card" -> pure SmartCardEmulationDispositionLeaveCard
    "reset-card" -> pure SmartCardEmulationDispositionResetCard
    "unpower-card" -> pure SmartCardEmulationDispositionUnpowerCard
    "eject-card" -> pure SmartCardEmulationDispositionEjectCard
    "_" -> fail "failed to parse SmartCardEmulationDisposition"
instance ToJSON SmartCardEmulationDisposition where
  toJSON v = A.String $ case v of
    SmartCardEmulationDispositionLeaveCard -> "leave-card"
    SmartCardEmulationDispositionResetCard -> "reset-card"
    SmartCardEmulationDispositionUnpowerCard -> "unpower-card"
    SmartCardEmulationDispositionEjectCard -> "eject-card"

-- | Type 'SmartCardEmulation.ConnectionState'.
--   Maps to |SCARD_*| connection state values.
data SmartCardEmulationConnectionState = SmartCardEmulationConnectionStateAbsent | SmartCardEmulationConnectionStatePresent | SmartCardEmulationConnectionStateSwallowed | SmartCardEmulationConnectionStatePowered | SmartCardEmulationConnectionStateNegotiable | SmartCardEmulationConnectionStateSpecific
  deriving (Ord, Eq, Show, Read)
instance FromJSON SmartCardEmulationConnectionState where
  parseJSON = A.withText "SmartCardEmulationConnectionState" $ \v -> case v of
    "absent" -> pure SmartCardEmulationConnectionStateAbsent
    "present" -> pure SmartCardEmulationConnectionStatePresent
    "swallowed" -> pure SmartCardEmulationConnectionStateSwallowed
    "powered" -> pure SmartCardEmulationConnectionStatePowered
    "negotiable" -> pure SmartCardEmulationConnectionStateNegotiable
    "specific" -> pure SmartCardEmulationConnectionStateSpecific
    "_" -> fail "failed to parse SmartCardEmulationConnectionState"
instance ToJSON SmartCardEmulationConnectionState where
  toJSON v = A.String $ case v of
    SmartCardEmulationConnectionStateAbsent -> "absent"
    SmartCardEmulationConnectionStatePresent -> "present"
    SmartCardEmulationConnectionStateSwallowed -> "swallowed"
    SmartCardEmulationConnectionStatePowered -> "powered"
    SmartCardEmulationConnectionStateNegotiable -> "negotiable"
    SmartCardEmulationConnectionStateSpecific -> "specific"

-- | Type 'SmartCardEmulation.ReaderStateFlags'.
--   Maps to the |SCARD_STATE_*| flags.
data SmartCardEmulationReaderStateFlags = SmartCardEmulationReaderStateFlags
  {
    smartCardEmulationReaderStateFlagsUnaware :: Maybe Bool,
    smartCardEmulationReaderStateFlagsIgnore :: Maybe Bool,
    smartCardEmulationReaderStateFlagsChanged :: Maybe Bool,
    smartCardEmulationReaderStateFlagsUnknown :: Maybe Bool,
    smartCardEmulationReaderStateFlagsUnavailable :: Maybe Bool,
    smartCardEmulationReaderStateFlagsEmpty :: Maybe Bool,
    smartCardEmulationReaderStateFlagsPresent :: Maybe Bool,
    smartCardEmulationReaderStateFlagsExclusive :: Maybe Bool,
    smartCardEmulationReaderStateFlagsInuse :: Maybe Bool,
    smartCardEmulationReaderStateFlagsMute :: Maybe Bool,
    smartCardEmulationReaderStateFlagsUnpowered :: Maybe Bool
  }
  deriving (Eq, Show)
instance FromJSON SmartCardEmulationReaderStateFlags where
  parseJSON = A.withObject "SmartCardEmulationReaderStateFlags" $ \o -> SmartCardEmulationReaderStateFlags
    <$> o A..:? "unaware"
    <*> o A..:? "ignore"
    <*> o A..:? "changed"
    <*> o A..:? "unknown"
    <*> o A..:? "unavailable"
    <*> o A..:? "empty"
    <*> o A..:? "present"
    <*> o A..:? "exclusive"
    <*> o A..:? "inuse"
    <*> o A..:? "mute"
    <*> o A..:? "unpowered"
instance ToJSON SmartCardEmulationReaderStateFlags where
  toJSON p = A.object $ catMaybes [
    ("unaware" A..=) <$> (smartCardEmulationReaderStateFlagsUnaware p),
    ("ignore" A..=) <$> (smartCardEmulationReaderStateFlagsIgnore p),
    ("changed" A..=) <$> (smartCardEmulationReaderStateFlagsChanged p),
    ("unknown" A..=) <$> (smartCardEmulationReaderStateFlagsUnknown p),
    ("unavailable" A..=) <$> (smartCardEmulationReaderStateFlagsUnavailable p),
    ("empty" A..=) <$> (smartCardEmulationReaderStateFlagsEmpty p),
    ("present" A..=) <$> (smartCardEmulationReaderStateFlagsPresent p),
    ("exclusive" A..=) <$> (smartCardEmulationReaderStateFlagsExclusive p),
    ("inuse" A..=) <$> (smartCardEmulationReaderStateFlagsInuse p),
    ("mute" A..=) <$> (smartCardEmulationReaderStateFlagsMute p),
    ("unpowered" A..=) <$> (smartCardEmulationReaderStateFlagsUnpowered p)
    ]

-- | Type 'SmartCardEmulation.ProtocolSet'.
--   Maps to the |SCARD_PROTOCOL_*| flags.
data SmartCardEmulationProtocolSet = SmartCardEmulationProtocolSet
  {
    smartCardEmulationProtocolSetT0 :: Maybe Bool,
    smartCardEmulationProtocolSetT1 :: Maybe Bool,
    smartCardEmulationProtocolSetRaw :: Maybe Bool
  }
  deriving (Eq, Show)
instance FromJSON SmartCardEmulationProtocolSet where
  parseJSON = A.withObject "SmartCardEmulationProtocolSet" $ \o -> SmartCardEmulationProtocolSet
    <$> o A..:? "t0"
    <*> o A..:? "t1"
    <*> o A..:? "raw"
instance ToJSON SmartCardEmulationProtocolSet where
  toJSON p = A.object $ catMaybes [
    ("t0" A..=) <$> (smartCardEmulationProtocolSetT0 p),
    ("t1" A..=) <$> (smartCardEmulationProtocolSetT1 p),
    ("raw" A..=) <$> (smartCardEmulationProtocolSetRaw p)
    ]

-- | Type 'SmartCardEmulation.Protocol'.
--   Maps to the |SCARD_PROTOCOL_*| values.
data SmartCardEmulationProtocol = SmartCardEmulationProtocolT0 | SmartCardEmulationProtocolT1 | SmartCardEmulationProtocolRaw
  deriving (Ord, Eq, Show, Read)
instance FromJSON SmartCardEmulationProtocol where
  parseJSON = A.withText "SmartCardEmulationProtocol" $ \v -> case v of
    "t0" -> pure SmartCardEmulationProtocolT0
    "t1" -> pure SmartCardEmulationProtocolT1
    "raw" -> pure SmartCardEmulationProtocolRaw
    "_" -> fail "failed to parse SmartCardEmulationProtocol"
instance ToJSON SmartCardEmulationProtocol where
  toJSON v = A.String $ case v of
    SmartCardEmulationProtocolT0 -> "t0"
    SmartCardEmulationProtocolT1 -> "t1"
    SmartCardEmulationProtocolRaw -> "raw"

-- | Type 'SmartCardEmulation.ReaderStateIn'.
data SmartCardEmulationReaderStateIn = SmartCardEmulationReaderStateIn
  {
    smartCardEmulationReaderStateInReader :: T.Text,
    smartCardEmulationReaderStateInCurrentState :: SmartCardEmulationReaderStateFlags,
    smartCardEmulationReaderStateInCurrentInsertionCount :: Int
  }
  deriving (Eq, Show)
instance FromJSON SmartCardEmulationReaderStateIn where
  parseJSON = A.withObject "SmartCardEmulationReaderStateIn" $ \o -> SmartCardEmulationReaderStateIn
    <$> o A..: "reader"
    <*> o A..: "currentState"
    <*> o A..: "currentInsertionCount"
instance ToJSON SmartCardEmulationReaderStateIn where
  toJSON p = A.object $ catMaybes [
    ("reader" A..=) <$> Just (smartCardEmulationReaderStateInReader p),
    ("currentState" A..=) <$> Just (smartCardEmulationReaderStateInCurrentState p),
    ("currentInsertionCount" A..=) <$> Just (smartCardEmulationReaderStateInCurrentInsertionCount p)
    ]

-- | Type 'SmartCardEmulation.ReaderStateOut'.
data SmartCardEmulationReaderStateOut = SmartCardEmulationReaderStateOut
  {
    smartCardEmulationReaderStateOutReader :: T.Text,
    smartCardEmulationReaderStateOutEventState :: SmartCardEmulationReaderStateFlags,
    smartCardEmulationReaderStateOutEventCount :: Int,
    smartCardEmulationReaderStateOutAtr :: T.Text
  }
  deriving (Eq, Show)
instance FromJSON SmartCardEmulationReaderStateOut where
  parseJSON = A.withObject "SmartCardEmulationReaderStateOut" $ \o -> SmartCardEmulationReaderStateOut
    <$> o A..: "reader"
    <*> o A..: "eventState"
    <*> o A..: "eventCount"
    <*> o A..: "atr"
instance ToJSON SmartCardEmulationReaderStateOut where
  toJSON p = A.object $ catMaybes [
    ("reader" A..=) <$> Just (smartCardEmulationReaderStateOutReader p),
    ("eventState" A..=) <$> Just (smartCardEmulationReaderStateOutEventState p),
    ("eventCount" A..=) <$> Just (smartCardEmulationReaderStateOutEventCount p),
    ("atr" A..=) <$> Just (smartCardEmulationReaderStateOutAtr p)
    ]

-- | Type of the 'SmartCardEmulation.establishContextRequested' event.
data SmartCardEmulationEstablishContextRequested = SmartCardEmulationEstablishContextRequested
  {
    smartCardEmulationEstablishContextRequestedRequestId :: T.Text
  }
  deriving (Eq, Show)
instance FromJSON SmartCardEmulationEstablishContextRequested where
  parseJSON = A.withObject "SmartCardEmulationEstablishContextRequested" $ \o -> SmartCardEmulationEstablishContextRequested
    <$> o A..: "requestId"
instance Event SmartCardEmulationEstablishContextRequested where
  eventName _ = "SmartCardEmulation.establishContextRequested"

-- | Type of the 'SmartCardEmulation.releaseContextRequested' event.
data SmartCardEmulationReleaseContextRequested = SmartCardEmulationReleaseContextRequested
  {
    smartCardEmulationReleaseContextRequestedRequestId :: T.Text,
    smartCardEmulationReleaseContextRequestedContextId :: Int
  }
  deriving (Eq, Show)
instance FromJSON SmartCardEmulationReleaseContextRequested where
  parseJSON = A.withObject "SmartCardEmulationReleaseContextRequested" $ \o -> SmartCardEmulationReleaseContextRequested
    <$> o A..: "requestId"
    <*> o A..: "contextId"
instance Event SmartCardEmulationReleaseContextRequested where
  eventName _ = "SmartCardEmulation.releaseContextRequested"

-- | Type of the 'SmartCardEmulation.listReadersRequested' event.
data SmartCardEmulationListReadersRequested = SmartCardEmulationListReadersRequested
  {
    smartCardEmulationListReadersRequestedRequestId :: T.Text,
    smartCardEmulationListReadersRequestedContextId :: Int
  }
  deriving (Eq, Show)
instance FromJSON SmartCardEmulationListReadersRequested where
  parseJSON = A.withObject "SmartCardEmulationListReadersRequested" $ \o -> SmartCardEmulationListReadersRequested
    <$> o A..: "requestId"
    <*> o A..: "contextId"
instance Event SmartCardEmulationListReadersRequested where
  eventName _ = "SmartCardEmulation.listReadersRequested"

-- | Type of the 'SmartCardEmulation.getStatusChangeRequested' event.
data SmartCardEmulationGetStatusChangeRequested = SmartCardEmulationGetStatusChangeRequested
  {
    smartCardEmulationGetStatusChangeRequestedRequestId :: T.Text,
    smartCardEmulationGetStatusChangeRequestedContextId :: Int,
    smartCardEmulationGetStatusChangeRequestedReaderStates :: [SmartCardEmulationReaderStateIn],
    -- | in milliseconds, if absent, it means "infinite"
    smartCardEmulationGetStatusChangeRequestedTimeout :: Maybe Int
  }
  deriving (Eq, Show)
instance FromJSON SmartCardEmulationGetStatusChangeRequested where
  parseJSON = A.withObject "SmartCardEmulationGetStatusChangeRequested" $ \o -> SmartCardEmulationGetStatusChangeRequested
    <$> o A..: "requestId"
    <*> o A..: "contextId"
    <*> o A..: "readerStates"
    <*> o A..:? "timeout"
instance Event SmartCardEmulationGetStatusChangeRequested where
  eventName _ = "SmartCardEmulation.getStatusChangeRequested"

-- | Type of the 'SmartCardEmulation.cancelRequested' event.
data SmartCardEmulationCancelRequested = SmartCardEmulationCancelRequested
  {
    smartCardEmulationCancelRequestedRequestId :: T.Text,
    smartCardEmulationCancelRequestedContextId :: Int
  }
  deriving (Eq, Show)
instance FromJSON SmartCardEmulationCancelRequested where
  parseJSON = A.withObject "SmartCardEmulationCancelRequested" $ \o -> SmartCardEmulationCancelRequested
    <$> o A..: "requestId"
    <*> o A..: "contextId"
instance Event SmartCardEmulationCancelRequested where
  eventName _ = "SmartCardEmulation.cancelRequested"

-- | Type of the 'SmartCardEmulation.connectRequested' event.
data SmartCardEmulationConnectRequested = SmartCardEmulationConnectRequested
  {
    smartCardEmulationConnectRequestedRequestId :: T.Text,
    smartCardEmulationConnectRequestedContextId :: Int,
    smartCardEmulationConnectRequestedReader :: T.Text,
    smartCardEmulationConnectRequestedShareMode :: SmartCardEmulationShareMode,
    smartCardEmulationConnectRequestedPreferredProtocols :: SmartCardEmulationProtocolSet
  }
  deriving (Eq, Show)
instance FromJSON SmartCardEmulationConnectRequested where
  parseJSON = A.withObject "SmartCardEmulationConnectRequested" $ \o -> SmartCardEmulationConnectRequested
    <$> o A..: "requestId"
    <*> o A..: "contextId"
    <*> o A..: "reader"
    <*> o A..: "shareMode"
    <*> o A..: "preferredProtocols"
instance Event SmartCardEmulationConnectRequested where
  eventName _ = "SmartCardEmulation.connectRequested"

-- | Type of the 'SmartCardEmulation.disconnectRequested' event.
data SmartCardEmulationDisconnectRequested = SmartCardEmulationDisconnectRequested
  {
    smartCardEmulationDisconnectRequestedRequestId :: T.Text,
    smartCardEmulationDisconnectRequestedHandle :: Int,
    smartCardEmulationDisconnectRequestedDisposition :: SmartCardEmulationDisposition
  }
  deriving (Eq, Show)
instance FromJSON SmartCardEmulationDisconnectRequested where
  parseJSON = A.withObject "SmartCardEmulationDisconnectRequested" $ \o -> SmartCardEmulationDisconnectRequested
    <$> o A..: "requestId"
    <*> o A..: "handle"
    <*> o A..: "disposition"
instance Event SmartCardEmulationDisconnectRequested where
  eventName _ = "SmartCardEmulation.disconnectRequested"

-- | Type of the 'SmartCardEmulation.transmitRequested' event.
data SmartCardEmulationTransmitRequested = SmartCardEmulationTransmitRequested
  {
    smartCardEmulationTransmitRequestedRequestId :: T.Text,
    smartCardEmulationTransmitRequestedHandle :: Int,
    smartCardEmulationTransmitRequestedData :: T.Text,
    smartCardEmulationTransmitRequestedProtocol :: Maybe SmartCardEmulationProtocol
  }
  deriving (Eq, Show)
instance FromJSON SmartCardEmulationTransmitRequested where
  parseJSON = A.withObject "SmartCardEmulationTransmitRequested" $ \o -> SmartCardEmulationTransmitRequested
    <$> o A..: "requestId"
    <*> o A..: "handle"
    <*> o A..: "data"
    <*> o A..:? "protocol"
instance Event SmartCardEmulationTransmitRequested where
  eventName _ = "SmartCardEmulation.transmitRequested"

-- | Type of the 'SmartCardEmulation.controlRequested' event.
data SmartCardEmulationControlRequested = SmartCardEmulationControlRequested
  {
    smartCardEmulationControlRequestedRequestId :: T.Text,
    smartCardEmulationControlRequestedHandle :: Int,
    smartCardEmulationControlRequestedControlCode :: Int,
    smartCardEmulationControlRequestedData :: T.Text
  }
  deriving (Eq, Show)
instance FromJSON SmartCardEmulationControlRequested where
  parseJSON = A.withObject "SmartCardEmulationControlRequested" $ \o -> SmartCardEmulationControlRequested
    <$> o A..: "requestId"
    <*> o A..: "handle"
    <*> o A..: "controlCode"
    <*> o A..: "data"
instance Event SmartCardEmulationControlRequested where
  eventName _ = "SmartCardEmulation.controlRequested"

-- | Type of the 'SmartCardEmulation.getAttribRequested' event.
data SmartCardEmulationGetAttribRequested = SmartCardEmulationGetAttribRequested
  {
    smartCardEmulationGetAttribRequestedRequestId :: T.Text,
    smartCardEmulationGetAttribRequestedHandle :: Int,
    smartCardEmulationGetAttribRequestedAttribId :: Int
  }
  deriving (Eq, Show)
instance FromJSON SmartCardEmulationGetAttribRequested where
  parseJSON = A.withObject "SmartCardEmulationGetAttribRequested" $ \o -> SmartCardEmulationGetAttribRequested
    <$> o A..: "requestId"
    <*> o A..: "handle"
    <*> o A..: "attribId"
instance Event SmartCardEmulationGetAttribRequested where
  eventName _ = "SmartCardEmulation.getAttribRequested"

-- | Type of the 'SmartCardEmulation.setAttribRequested' event.
data SmartCardEmulationSetAttribRequested = SmartCardEmulationSetAttribRequested
  {
    smartCardEmulationSetAttribRequestedRequestId :: T.Text,
    smartCardEmulationSetAttribRequestedHandle :: Int,
    smartCardEmulationSetAttribRequestedAttribId :: Int,
    smartCardEmulationSetAttribRequestedData :: T.Text
  }
  deriving (Eq, Show)
instance FromJSON SmartCardEmulationSetAttribRequested where
  parseJSON = A.withObject "SmartCardEmulationSetAttribRequested" $ \o -> SmartCardEmulationSetAttribRequested
    <$> o A..: "requestId"
    <*> o A..: "handle"
    <*> o A..: "attribId"
    <*> o A..: "data"
instance Event SmartCardEmulationSetAttribRequested where
  eventName _ = "SmartCardEmulation.setAttribRequested"

-- | Type of the 'SmartCardEmulation.statusRequested' event.
data SmartCardEmulationStatusRequested = SmartCardEmulationStatusRequested
  {
    smartCardEmulationStatusRequestedRequestId :: T.Text,
    smartCardEmulationStatusRequestedHandle :: Int
  }
  deriving (Eq, Show)
instance FromJSON SmartCardEmulationStatusRequested where
  parseJSON = A.withObject "SmartCardEmulationStatusRequested" $ \o -> SmartCardEmulationStatusRequested
    <$> o A..: "requestId"
    <*> o A..: "handle"
instance Event SmartCardEmulationStatusRequested where
  eventName _ = "SmartCardEmulation.statusRequested"

-- | Type of the 'SmartCardEmulation.beginTransactionRequested' event.
data SmartCardEmulationBeginTransactionRequested = SmartCardEmulationBeginTransactionRequested
  {
    smartCardEmulationBeginTransactionRequestedRequestId :: T.Text,
    smartCardEmulationBeginTransactionRequestedHandle :: Int
  }
  deriving (Eq, Show)
instance FromJSON SmartCardEmulationBeginTransactionRequested where
  parseJSON = A.withObject "SmartCardEmulationBeginTransactionRequested" $ \o -> SmartCardEmulationBeginTransactionRequested
    <$> o A..: "requestId"
    <*> o A..: "handle"
instance Event SmartCardEmulationBeginTransactionRequested where
  eventName _ = "SmartCardEmulation.beginTransactionRequested"

-- | Type of the 'SmartCardEmulation.endTransactionRequested' event.
data SmartCardEmulationEndTransactionRequested = SmartCardEmulationEndTransactionRequested
  {
    smartCardEmulationEndTransactionRequestedRequestId :: T.Text,
    smartCardEmulationEndTransactionRequestedHandle :: Int,
    smartCardEmulationEndTransactionRequestedDisposition :: SmartCardEmulationDisposition
  }
  deriving (Eq, Show)
instance FromJSON SmartCardEmulationEndTransactionRequested where
  parseJSON = A.withObject "SmartCardEmulationEndTransactionRequested" $ \o -> SmartCardEmulationEndTransactionRequested
    <$> o A..: "requestId"
    <*> o A..: "handle"
    <*> o A..: "disposition"
instance Event SmartCardEmulationEndTransactionRequested where
  eventName _ = "SmartCardEmulation.endTransactionRequested"

-- | Enables the |SmartCardEmulation| domain.

-- | Parameters of the 'SmartCardEmulation.enable' command.
data PSmartCardEmulationEnable = PSmartCardEmulationEnable
  deriving (Eq, Show)
pSmartCardEmulationEnable
  :: PSmartCardEmulationEnable
pSmartCardEmulationEnable
  = PSmartCardEmulationEnable
instance ToJSON PSmartCardEmulationEnable where
  toJSON _ = A.Null
instance Command PSmartCardEmulationEnable where
  type CommandResponse PSmartCardEmulationEnable = ()
  commandName _ = "SmartCardEmulation.enable"
  fromJSON = const . A.Success . const ()

-- | Disables the |SmartCardEmulation| domain.

-- | Parameters of the 'SmartCardEmulation.disable' command.
data PSmartCardEmulationDisable = PSmartCardEmulationDisable
  deriving (Eq, Show)
pSmartCardEmulationDisable
  :: PSmartCardEmulationDisable
pSmartCardEmulationDisable
  = PSmartCardEmulationDisable
instance ToJSON PSmartCardEmulationDisable where
  toJSON _ = A.Null
instance Command PSmartCardEmulationDisable where
  type CommandResponse PSmartCardEmulationDisable = ()
  commandName _ = "SmartCardEmulation.disable"
  fromJSON = const . A.Success . const ()

-- | Reports the successful result of a |SCardEstablishContext| call.
--   
--   This maps to:
--   PC/SC Lite: https://pcsclite.apdu.fr/api/group__API.html#gaa1b8970169fd4883a6dc4a8f43f19b67
--   Microsoft: https://learn.microsoft.com/en-us/windows/win32/api/winscard/nf-winscard-scardestablishcontext

-- | Parameters of the 'SmartCardEmulation.reportEstablishContextResult' command.
data PSmartCardEmulationReportEstablishContextResult = PSmartCardEmulationReportEstablishContextResult
  {
    pSmartCardEmulationReportEstablishContextResultRequestId :: T.Text,
    pSmartCardEmulationReportEstablishContextResultContextId :: Int
  }
  deriving (Eq, Show)
pSmartCardEmulationReportEstablishContextResult
  :: T.Text
  -> Int
  -> PSmartCardEmulationReportEstablishContextResult
pSmartCardEmulationReportEstablishContextResult
  arg_pSmartCardEmulationReportEstablishContextResultRequestId
  arg_pSmartCardEmulationReportEstablishContextResultContextId
  = PSmartCardEmulationReportEstablishContextResult
    arg_pSmartCardEmulationReportEstablishContextResultRequestId
    arg_pSmartCardEmulationReportEstablishContextResultContextId
instance ToJSON PSmartCardEmulationReportEstablishContextResult where
  toJSON p = A.object $ catMaybes [
    ("requestId" A..=) <$> Just (pSmartCardEmulationReportEstablishContextResultRequestId p),
    ("contextId" A..=) <$> Just (pSmartCardEmulationReportEstablishContextResultContextId p)
    ]
instance Command PSmartCardEmulationReportEstablishContextResult where
  type CommandResponse PSmartCardEmulationReportEstablishContextResult = ()
  commandName _ = "SmartCardEmulation.reportEstablishContextResult"
  fromJSON = const . A.Success . const ()

-- | Reports the successful result of a |SCardReleaseContext| call.
--   
--   This maps to:
--   PC/SC Lite: https://pcsclite.apdu.fr/api/group__API.html#ga6aabcba7744c5c9419fdd6404f73a934
--   Microsoft: https://learn.microsoft.com/en-us/windows/win32/api/winscard/nf-winscard-scardreleasecontext

-- | Parameters of the 'SmartCardEmulation.reportReleaseContextResult' command.
data PSmartCardEmulationReportReleaseContextResult = PSmartCardEmulationReportReleaseContextResult
  {
    pSmartCardEmulationReportReleaseContextResultRequestId :: T.Text
  }
  deriving (Eq, Show)
pSmartCardEmulationReportReleaseContextResult
  :: T.Text
  -> PSmartCardEmulationReportReleaseContextResult
pSmartCardEmulationReportReleaseContextResult
  arg_pSmartCardEmulationReportReleaseContextResultRequestId
  = PSmartCardEmulationReportReleaseContextResult
    arg_pSmartCardEmulationReportReleaseContextResultRequestId
instance ToJSON PSmartCardEmulationReportReleaseContextResult where
  toJSON p = A.object $ catMaybes [
    ("requestId" A..=) <$> Just (pSmartCardEmulationReportReleaseContextResultRequestId p)
    ]
instance Command PSmartCardEmulationReportReleaseContextResult where
  type CommandResponse PSmartCardEmulationReportReleaseContextResult = ()
  commandName _ = "SmartCardEmulation.reportReleaseContextResult"
  fromJSON = const . A.Success . const ()

-- | Reports the successful result of a |SCardListReaders| call.
--   
--   This maps to:
--   PC/SC Lite: https://pcsclite.apdu.fr/api/group__API.html#ga93b07815789b3cf2629d439ecf20f0d9
--   Microsoft: https://learn.microsoft.com/en-us/windows/win32/api/winscard/nf-winscard-scardlistreadersa

-- | Parameters of the 'SmartCardEmulation.reportListReadersResult' command.
data PSmartCardEmulationReportListReadersResult = PSmartCardEmulationReportListReadersResult
  {
    pSmartCardEmulationReportListReadersResultRequestId :: T.Text,
    pSmartCardEmulationReportListReadersResultReaders :: [T.Text]
  }
  deriving (Eq, Show)
pSmartCardEmulationReportListReadersResult
  :: T.Text
  -> [T.Text]
  -> PSmartCardEmulationReportListReadersResult
pSmartCardEmulationReportListReadersResult
  arg_pSmartCardEmulationReportListReadersResultRequestId
  arg_pSmartCardEmulationReportListReadersResultReaders
  = PSmartCardEmulationReportListReadersResult
    arg_pSmartCardEmulationReportListReadersResultRequestId
    arg_pSmartCardEmulationReportListReadersResultReaders
instance ToJSON PSmartCardEmulationReportListReadersResult where
  toJSON p = A.object $ catMaybes [
    ("requestId" A..=) <$> Just (pSmartCardEmulationReportListReadersResultRequestId p),
    ("readers" A..=) <$> Just (pSmartCardEmulationReportListReadersResultReaders p)
    ]
instance Command PSmartCardEmulationReportListReadersResult where
  type CommandResponse PSmartCardEmulationReportListReadersResult = ()
  commandName _ = "SmartCardEmulation.reportListReadersResult"
  fromJSON = const . A.Success . const ()

-- | Reports the successful result of a |SCardGetStatusChange| call.
--   
--   This maps to:
--   PC/SC Lite: https://pcsclite.apdu.fr/api/group__API.html#ga33247d5d1257d59e55647c3bb717db24
--   Microsoft: https://learn.microsoft.com/en-us/windows/win32/api/winscard/nf-winscard-scardgetstatuschangea

-- | Parameters of the 'SmartCardEmulation.reportGetStatusChangeResult' command.
data PSmartCardEmulationReportGetStatusChangeResult = PSmartCardEmulationReportGetStatusChangeResult
  {
    pSmartCardEmulationReportGetStatusChangeResultRequestId :: T.Text,
    pSmartCardEmulationReportGetStatusChangeResultReaderStates :: [SmartCardEmulationReaderStateOut]
  }
  deriving (Eq, Show)
pSmartCardEmulationReportGetStatusChangeResult
  :: T.Text
  -> [SmartCardEmulationReaderStateOut]
  -> PSmartCardEmulationReportGetStatusChangeResult
pSmartCardEmulationReportGetStatusChangeResult
  arg_pSmartCardEmulationReportGetStatusChangeResultRequestId
  arg_pSmartCardEmulationReportGetStatusChangeResultReaderStates
  = PSmartCardEmulationReportGetStatusChangeResult
    arg_pSmartCardEmulationReportGetStatusChangeResultRequestId
    arg_pSmartCardEmulationReportGetStatusChangeResultReaderStates
instance ToJSON PSmartCardEmulationReportGetStatusChangeResult where
  toJSON p = A.object $ catMaybes [
    ("requestId" A..=) <$> Just (pSmartCardEmulationReportGetStatusChangeResultRequestId p),
    ("readerStates" A..=) <$> Just (pSmartCardEmulationReportGetStatusChangeResultReaderStates p)
    ]
instance Command PSmartCardEmulationReportGetStatusChangeResult where
  type CommandResponse PSmartCardEmulationReportGetStatusChangeResult = ()
  commandName _ = "SmartCardEmulation.reportGetStatusChangeResult"
  fromJSON = const . A.Success . const ()

-- | Reports the result of a |SCardBeginTransaction| call.
--   On success, this creates a new transaction object.
--   
--   This maps to:
--   PC/SC Lite: https://pcsclite.apdu.fr/api/group__API.html#gaddb835dce01a0da1d6ca02d33ee7d861
--   Microsoft: https://learn.microsoft.com/en-us/windows/win32/api/winscard/nf-winscard-scardbegintransaction

-- | Parameters of the 'SmartCardEmulation.reportBeginTransactionResult' command.
data PSmartCardEmulationReportBeginTransactionResult = PSmartCardEmulationReportBeginTransactionResult
  {
    pSmartCardEmulationReportBeginTransactionResultRequestId :: T.Text,
    pSmartCardEmulationReportBeginTransactionResultHandle :: Int
  }
  deriving (Eq, Show)
pSmartCardEmulationReportBeginTransactionResult
  :: T.Text
  -> Int
  -> PSmartCardEmulationReportBeginTransactionResult
pSmartCardEmulationReportBeginTransactionResult
  arg_pSmartCardEmulationReportBeginTransactionResultRequestId
  arg_pSmartCardEmulationReportBeginTransactionResultHandle
  = PSmartCardEmulationReportBeginTransactionResult
    arg_pSmartCardEmulationReportBeginTransactionResultRequestId
    arg_pSmartCardEmulationReportBeginTransactionResultHandle
instance ToJSON PSmartCardEmulationReportBeginTransactionResult where
  toJSON p = A.object $ catMaybes [
    ("requestId" A..=) <$> Just (pSmartCardEmulationReportBeginTransactionResultRequestId p),
    ("handle" A..=) <$> Just (pSmartCardEmulationReportBeginTransactionResultHandle p)
    ]
instance Command PSmartCardEmulationReportBeginTransactionResult where
  type CommandResponse PSmartCardEmulationReportBeginTransactionResult = ()
  commandName _ = "SmartCardEmulation.reportBeginTransactionResult"
  fromJSON = const . A.Success . const ()

-- | Reports the successful result of a call that returns only a result code.
--   Used for: |SCardCancel|, |SCardDisconnect|, |SCardSetAttrib|, |SCardEndTransaction|.
--   
--   This maps to:
--   1. SCardCancel
--      PC/SC Lite: https://pcsclite.apdu.fr/api/group__API.html#gaacbbc0c6d6c0cbbeb4f4debf6fbeeee6
--      Microsoft: https://learn.microsoft.com/en-us/windows/win32/api/winscard/nf-winscard-scardcancel
--   
--   2. SCardDisconnect
--      PC/SC Lite: https://pcsclite.apdu.fr/api/group__API.html#ga4be198045c73ec0deb79e66c0ca1738a
--      Microsoft: https://learn.microsoft.com/en-us/windows/win32/api/winscard/nf-winscard-scarddisconnect
--   
--   3. SCardSetAttrib
--      PC/SC Lite: https://pcsclite.apdu.fr/api/group__API.html#ga060f0038a4ddfd5dd2b8fadf3c3a2e4f
--      Microsoft: https://learn.microsoft.com/en-us/windows/win32/api/winscard/nf-winscard-scardsetattrib
--   
--   4. SCardEndTransaction
--      PC/SC Lite: https://pcsclite.apdu.fr/api/group__API.html#gae8742473b404363e5c587f570d7e2f3b
--      Microsoft: https://learn.microsoft.com/en-us/windows/win32/api/winscard/nf-winscard-scardendtransaction

-- | Parameters of the 'SmartCardEmulation.reportPlainResult' command.
data PSmartCardEmulationReportPlainResult = PSmartCardEmulationReportPlainResult
  {
    pSmartCardEmulationReportPlainResultRequestId :: T.Text
  }
  deriving (Eq, Show)
pSmartCardEmulationReportPlainResult
  :: T.Text
  -> PSmartCardEmulationReportPlainResult
pSmartCardEmulationReportPlainResult
  arg_pSmartCardEmulationReportPlainResultRequestId
  = PSmartCardEmulationReportPlainResult
    arg_pSmartCardEmulationReportPlainResultRequestId
instance ToJSON PSmartCardEmulationReportPlainResult where
  toJSON p = A.object $ catMaybes [
    ("requestId" A..=) <$> Just (pSmartCardEmulationReportPlainResultRequestId p)
    ]
instance Command PSmartCardEmulationReportPlainResult where
  type CommandResponse PSmartCardEmulationReportPlainResult = ()
  commandName _ = "SmartCardEmulation.reportPlainResult"
  fromJSON = const . A.Success . const ()

-- | Reports the successful result of a |SCardConnect| call.
--   
--   This maps to:
--   PC/SC Lite: https://pcsclite.apdu.fr/api/group__API.html#ga4e515829752e0a8dbc4d630696a8d6a5
--   Microsoft: https://learn.microsoft.com/en-us/windows/win32/api/winscard/nf-winscard-scardconnecta

-- | Parameters of the 'SmartCardEmulation.reportConnectResult' command.
data PSmartCardEmulationReportConnectResult = PSmartCardEmulationReportConnectResult
  {
    pSmartCardEmulationReportConnectResultRequestId :: T.Text,
    pSmartCardEmulationReportConnectResultHandle :: Int,
    pSmartCardEmulationReportConnectResultActiveProtocol :: Maybe SmartCardEmulationProtocol
  }
  deriving (Eq, Show)
pSmartCardEmulationReportConnectResult
  :: T.Text
  -> Int
  -> PSmartCardEmulationReportConnectResult
pSmartCardEmulationReportConnectResult
  arg_pSmartCardEmulationReportConnectResultRequestId
  arg_pSmartCardEmulationReportConnectResultHandle
  = PSmartCardEmulationReportConnectResult
    arg_pSmartCardEmulationReportConnectResultRequestId
    arg_pSmartCardEmulationReportConnectResultHandle
    Nothing
instance ToJSON PSmartCardEmulationReportConnectResult where
  toJSON p = A.object $ catMaybes [
    ("requestId" A..=) <$> Just (pSmartCardEmulationReportConnectResultRequestId p),
    ("handle" A..=) <$> Just (pSmartCardEmulationReportConnectResultHandle p),
    ("activeProtocol" A..=) <$> (pSmartCardEmulationReportConnectResultActiveProtocol p)
    ]
instance Command PSmartCardEmulationReportConnectResult where
  type CommandResponse PSmartCardEmulationReportConnectResult = ()
  commandName _ = "SmartCardEmulation.reportConnectResult"
  fromJSON = const . A.Success . const ()

-- | Reports the successful result of a call that sends back data on success.
--   Used for |SCardTransmit|, |SCardControl|, and |SCardGetAttrib|.
--   
--   This maps to:
--   1. SCardTransmit
--      PC/SC Lite: https://pcsclite.apdu.fr/api/group__API.html#ga9a2d77242a271310269065e64633ab99
--      Microsoft: https://learn.microsoft.com/en-us/windows/win32/api/winscard/nf-winscard-scardtransmit
--   
--   2. SCardControl
--      PC/SC Lite: https://pcsclite.apdu.fr/api/group__API.html#gac3454d4657110fd7f753b2d3d8f4e32f
--      Microsoft: https://learn.microsoft.com/en-us/windows/win32/api/winscard/nf-winscard-scardcontrol
--   
--   3. SCardGetAttrib
--      PC/SC Lite: https://pcsclite.apdu.fr/api/group__API.html#gaacfec51917255b7a25b94c5104961602
--      Microsoft: https://learn.microsoft.com/en-us/windows/win32/api/winscard/nf-winscard-scardgetattrib

-- | Parameters of the 'SmartCardEmulation.reportDataResult' command.
data PSmartCardEmulationReportDataResult = PSmartCardEmulationReportDataResult
  {
    pSmartCardEmulationReportDataResultRequestId :: T.Text,
    pSmartCardEmulationReportDataResultData :: T.Text
  }
  deriving (Eq, Show)
pSmartCardEmulationReportDataResult
  :: T.Text
  -> T.Text
  -> PSmartCardEmulationReportDataResult
pSmartCardEmulationReportDataResult
  arg_pSmartCardEmulationReportDataResultRequestId
  arg_pSmartCardEmulationReportDataResultData
  = PSmartCardEmulationReportDataResult
    arg_pSmartCardEmulationReportDataResultRequestId
    arg_pSmartCardEmulationReportDataResultData
instance ToJSON PSmartCardEmulationReportDataResult where
  toJSON p = A.object $ catMaybes [
    ("requestId" A..=) <$> Just (pSmartCardEmulationReportDataResultRequestId p),
    ("data" A..=) <$> Just (pSmartCardEmulationReportDataResultData p)
    ]
instance Command PSmartCardEmulationReportDataResult where
  type CommandResponse PSmartCardEmulationReportDataResult = ()
  commandName _ = "SmartCardEmulation.reportDataResult"
  fromJSON = const . A.Success . const ()

-- | Reports the successful result of a |SCardStatus| call.
--   
--   This maps to:
--   PC/SC Lite: https://pcsclite.apdu.fr/api/group__API.html#gae49c3c894ad7ac12a5b896bde70d0382
--   Microsoft: https://learn.microsoft.com/en-us/windows/win32/api/winscard/nf-winscard-scardstatusa

-- | Parameters of the 'SmartCardEmulation.reportStatusResult' command.
data PSmartCardEmulationReportStatusResult = PSmartCardEmulationReportStatusResult
  {
    pSmartCardEmulationReportStatusResultRequestId :: T.Text,
    pSmartCardEmulationReportStatusResultReaderName :: T.Text,
    pSmartCardEmulationReportStatusResultState :: SmartCardEmulationConnectionState,
    pSmartCardEmulationReportStatusResultAtr :: T.Text,
    pSmartCardEmulationReportStatusResultProtocol :: Maybe SmartCardEmulationProtocol
  }
  deriving (Eq, Show)
pSmartCardEmulationReportStatusResult
  :: T.Text
  -> T.Text
  -> SmartCardEmulationConnectionState
  -> T.Text
  -> PSmartCardEmulationReportStatusResult
pSmartCardEmulationReportStatusResult
  arg_pSmartCardEmulationReportStatusResultRequestId
  arg_pSmartCardEmulationReportStatusResultReaderName
  arg_pSmartCardEmulationReportStatusResultState
  arg_pSmartCardEmulationReportStatusResultAtr
  = PSmartCardEmulationReportStatusResult
    arg_pSmartCardEmulationReportStatusResultRequestId
    arg_pSmartCardEmulationReportStatusResultReaderName
    arg_pSmartCardEmulationReportStatusResultState
    arg_pSmartCardEmulationReportStatusResultAtr
    Nothing
instance ToJSON PSmartCardEmulationReportStatusResult where
  toJSON p = A.object $ catMaybes [
    ("requestId" A..=) <$> Just (pSmartCardEmulationReportStatusResultRequestId p),
    ("readerName" A..=) <$> Just (pSmartCardEmulationReportStatusResultReaderName p),
    ("state" A..=) <$> Just (pSmartCardEmulationReportStatusResultState p),
    ("atr" A..=) <$> Just (pSmartCardEmulationReportStatusResultAtr p),
    ("protocol" A..=) <$> (pSmartCardEmulationReportStatusResultProtocol p)
    ]
instance Command PSmartCardEmulationReportStatusResult where
  type CommandResponse PSmartCardEmulationReportStatusResult = ()
  commandName _ = "SmartCardEmulation.reportStatusResult"
  fromJSON = const . A.Success . const ()

-- | Reports an error result for the given request.

-- | Parameters of the 'SmartCardEmulation.reportError' command.
data PSmartCardEmulationReportError = PSmartCardEmulationReportError
  {
    pSmartCardEmulationReportErrorRequestId :: T.Text,
    pSmartCardEmulationReportErrorResultCode :: SmartCardEmulationResultCode
  }
  deriving (Eq, Show)
pSmartCardEmulationReportError
  :: T.Text
  -> SmartCardEmulationResultCode
  -> PSmartCardEmulationReportError
pSmartCardEmulationReportError
  arg_pSmartCardEmulationReportErrorRequestId
  arg_pSmartCardEmulationReportErrorResultCode
  = PSmartCardEmulationReportError
    arg_pSmartCardEmulationReportErrorRequestId
    arg_pSmartCardEmulationReportErrorResultCode
instance ToJSON PSmartCardEmulationReportError where
  toJSON p = A.object $ catMaybes [
    ("requestId" A..=) <$> Just (pSmartCardEmulationReportErrorRequestId p),
    ("resultCode" A..=) <$> Just (pSmartCardEmulationReportErrorResultCode p)
    ]
instance Command PSmartCardEmulationReportError where
  type CommandResponse PSmartCardEmulationReportError = ()
  commandName _ = "SmartCardEmulation.reportError"
  fromJSON = const . A.Success . const ()

