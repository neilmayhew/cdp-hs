{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}


{- |
= Storage

-}


module CDP.Domains.Storage (module CDP.Domains.Storage) where

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


import CDP.Domains.BrowserTarget as BrowserTarget
import CDP.Domains.DOMNetworkEmulationPageSecurity as DOMNetworkEmulationPageSecurity


-- | Type 'Storage.SerializedStorageKey'.
type StorageSerializedStorageKey = T.Text

-- | Type 'Storage.StorageType'.
--   Enum of possible storage types.
data StorageStorageType = StorageStorageTypeCookies | StorageStorageTypeFile_systems | StorageStorageTypeIndexeddb | StorageStorageTypeLocal_storage | StorageStorageTypeShader_cache | StorageStorageTypeWebsql | StorageStorageTypeService_workers | StorageStorageTypeCache_storage | StorageStorageTypeShared_storage | StorageStorageTypeStorage_buckets | StorageStorageTypeAll | StorageStorageTypeOther
  deriving (Ord, Eq, Show, Read)
instance FromJSON StorageStorageType where
  parseJSON = A.withText "StorageStorageType" $ \v -> case v of
    "cookies" -> pure StorageStorageTypeCookies
    "file_systems" -> pure StorageStorageTypeFile_systems
    "indexeddb" -> pure StorageStorageTypeIndexeddb
    "local_storage" -> pure StorageStorageTypeLocal_storage
    "shader_cache" -> pure StorageStorageTypeShader_cache
    "websql" -> pure StorageStorageTypeWebsql
    "service_workers" -> pure StorageStorageTypeService_workers
    "cache_storage" -> pure StorageStorageTypeCache_storage
    "shared_storage" -> pure StorageStorageTypeShared_storage
    "storage_buckets" -> pure StorageStorageTypeStorage_buckets
    "all" -> pure StorageStorageTypeAll
    "other" -> pure StorageStorageTypeOther
    "_" -> fail "failed to parse StorageStorageType"
instance ToJSON StorageStorageType where
  toJSON v = A.String $ case v of
    StorageStorageTypeCookies -> "cookies"
    StorageStorageTypeFile_systems -> "file_systems"
    StorageStorageTypeIndexeddb -> "indexeddb"
    StorageStorageTypeLocal_storage -> "local_storage"
    StorageStorageTypeShader_cache -> "shader_cache"
    StorageStorageTypeWebsql -> "websql"
    StorageStorageTypeService_workers -> "service_workers"
    StorageStorageTypeCache_storage -> "cache_storage"
    StorageStorageTypeShared_storage -> "shared_storage"
    StorageStorageTypeStorage_buckets -> "storage_buckets"
    StorageStorageTypeAll -> "all"
    StorageStorageTypeOther -> "other"

-- | Type 'Storage.UsageForType'.
--   Usage for a storage type.
data StorageUsageForType = StorageUsageForType
  {
    -- | Name of storage type.
    storageUsageForTypeStorageType :: StorageStorageType,
    -- | Storage usage (bytes).
    storageUsageForTypeUsage :: Double
  }
  deriving (Eq, Show)
instance FromJSON StorageUsageForType where
  parseJSON = A.withObject "StorageUsageForType" $ \o -> StorageUsageForType
    <$> o A..: "storageType"
    <*> o A..: "usage"
instance ToJSON StorageUsageForType where
  toJSON p = A.object $ catMaybes [
    ("storageType" A..=) <$> Just (storageUsageForTypeStorageType p),
    ("usage" A..=) <$> Just (storageUsageForTypeUsage p)
    ]

-- | Type 'Storage.TrustTokens'.
--   Pair of issuer origin and number of available (signed, but not used) Trust
--   Tokens from that issuer.
data StorageTrustTokens = StorageTrustTokens
  {
    storageTrustTokensIssuerOrigin :: T.Text,
    storageTrustTokensCount :: Double
  }
  deriving (Eq, Show)
instance FromJSON StorageTrustTokens where
  parseJSON = A.withObject "StorageTrustTokens" $ \o -> StorageTrustTokens
    <$> o A..: "issuerOrigin"
    <*> o A..: "count"
instance ToJSON StorageTrustTokens where
  toJSON p = A.object $ catMaybes [
    ("issuerOrigin" A..=) <$> Just (storageTrustTokensIssuerOrigin p),
    ("count" A..=) <$> Just (storageTrustTokensCount p)
    ]

-- | Type 'Storage.SharedStorageAccessScope'.
--   Enum of shared storage access scopes.
data StorageSharedStorageAccessScope = StorageSharedStorageAccessScopeWindow | StorageSharedStorageAccessScopeSharedStorageWorklet | StorageSharedStorageAccessScopeHeader
  deriving (Ord, Eq, Show, Read)
instance FromJSON StorageSharedStorageAccessScope where
  parseJSON = A.withText "StorageSharedStorageAccessScope" $ \v -> case v of
    "window" -> pure StorageSharedStorageAccessScopeWindow
    "sharedStorageWorklet" -> pure StorageSharedStorageAccessScopeSharedStorageWorklet
    "header" -> pure StorageSharedStorageAccessScopeHeader
    "_" -> fail "failed to parse StorageSharedStorageAccessScope"
instance ToJSON StorageSharedStorageAccessScope where
  toJSON v = A.String $ case v of
    StorageSharedStorageAccessScopeWindow -> "window"
    StorageSharedStorageAccessScopeSharedStorageWorklet -> "sharedStorageWorklet"
    StorageSharedStorageAccessScopeHeader -> "header"

-- | Type 'Storage.SharedStorageAccessMethod'.
--   Enum of shared storage access methods.
data StorageSharedStorageAccessMethod = StorageSharedStorageAccessMethodAddModule | StorageSharedStorageAccessMethodCreateWorklet | StorageSharedStorageAccessMethodSelectURL | StorageSharedStorageAccessMethodRun | StorageSharedStorageAccessMethodBatchUpdate | StorageSharedStorageAccessMethodSet | StorageSharedStorageAccessMethodAppend | StorageSharedStorageAccessMethodDelete | StorageSharedStorageAccessMethodClear | StorageSharedStorageAccessMethodGet | StorageSharedStorageAccessMethodKeys | StorageSharedStorageAccessMethodValues | StorageSharedStorageAccessMethodEntries | StorageSharedStorageAccessMethodLength | StorageSharedStorageAccessMethodRemainingBudget
  deriving (Ord, Eq, Show, Read)
instance FromJSON StorageSharedStorageAccessMethod where
  parseJSON = A.withText "StorageSharedStorageAccessMethod" $ \v -> case v of
    "addModule" -> pure StorageSharedStorageAccessMethodAddModule
    "createWorklet" -> pure StorageSharedStorageAccessMethodCreateWorklet
    "selectURL" -> pure StorageSharedStorageAccessMethodSelectURL
    "run" -> pure StorageSharedStorageAccessMethodRun
    "batchUpdate" -> pure StorageSharedStorageAccessMethodBatchUpdate
    "set" -> pure StorageSharedStorageAccessMethodSet
    "append" -> pure StorageSharedStorageAccessMethodAppend
    "delete" -> pure StorageSharedStorageAccessMethodDelete
    "clear" -> pure StorageSharedStorageAccessMethodClear
    "get" -> pure StorageSharedStorageAccessMethodGet
    "keys" -> pure StorageSharedStorageAccessMethodKeys
    "values" -> pure StorageSharedStorageAccessMethodValues
    "entries" -> pure StorageSharedStorageAccessMethodEntries
    "length" -> pure StorageSharedStorageAccessMethodLength
    "remainingBudget" -> pure StorageSharedStorageAccessMethodRemainingBudget
    "_" -> fail "failed to parse StorageSharedStorageAccessMethod"
instance ToJSON StorageSharedStorageAccessMethod where
  toJSON v = A.String $ case v of
    StorageSharedStorageAccessMethodAddModule -> "addModule"
    StorageSharedStorageAccessMethodCreateWorklet -> "createWorklet"
    StorageSharedStorageAccessMethodSelectURL -> "selectURL"
    StorageSharedStorageAccessMethodRun -> "run"
    StorageSharedStorageAccessMethodBatchUpdate -> "batchUpdate"
    StorageSharedStorageAccessMethodSet -> "set"
    StorageSharedStorageAccessMethodAppend -> "append"
    StorageSharedStorageAccessMethodDelete -> "delete"
    StorageSharedStorageAccessMethodClear -> "clear"
    StorageSharedStorageAccessMethodGet -> "get"
    StorageSharedStorageAccessMethodKeys -> "keys"
    StorageSharedStorageAccessMethodValues -> "values"
    StorageSharedStorageAccessMethodEntries -> "entries"
    StorageSharedStorageAccessMethodLength -> "length"
    StorageSharedStorageAccessMethodRemainingBudget -> "remainingBudget"

-- | Type 'Storage.SharedStorageEntry'.
--   Struct for a single key-value pair in an origin's shared storage.
data StorageSharedStorageEntry = StorageSharedStorageEntry
  {
    storageSharedStorageEntryKey :: T.Text,
    storageSharedStorageEntryValue :: T.Text
  }
  deriving (Eq, Show)
instance FromJSON StorageSharedStorageEntry where
  parseJSON = A.withObject "StorageSharedStorageEntry" $ \o -> StorageSharedStorageEntry
    <$> o A..: "key"
    <*> o A..: "value"
instance ToJSON StorageSharedStorageEntry where
  toJSON p = A.object $ catMaybes [
    ("key" A..=) <$> Just (storageSharedStorageEntryKey p),
    ("value" A..=) <$> Just (storageSharedStorageEntryValue p)
    ]

-- | Type 'Storage.SharedStorageMetadata'.
--   Details for an origin's shared storage.
data StorageSharedStorageMetadata = StorageSharedStorageMetadata
  {
    -- | Time when the origin's shared storage was last created.
    storageSharedStorageMetadataCreationTime :: DOMNetworkEmulationPageSecurity.NetworkTimeSinceEpoch,
    -- | Number of key-value pairs stored in origin's shared storage.
    storageSharedStorageMetadataLength :: Int,
    -- | Current amount of bits of entropy remaining in the navigation budget.
    storageSharedStorageMetadataRemainingBudget :: Double,
    -- | Total number of bytes stored as key-value pairs in origin's shared
    --   storage.
    storageSharedStorageMetadataBytesUsed :: Int
  }
  deriving (Eq, Show)
instance FromJSON StorageSharedStorageMetadata where
  parseJSON = A.withObject "StorageSharedStorageMetadata" $ \o -> StorageSharedStorageMetadata
    <$> o A..: "creationTime"
    <*> o A..: "length"
    <*> o A..: "remainingBudget"
    <*> o A..: "bytesUsed"
instance ToJSON StorageSharedStorageMetadata where
  toJSON p = A.object $ catMaybes [
    ("creationTime" A..=) <$> Just (storageSharedStorageMetadataCreationTime p),
    ("length" A..=) <$> Just (storageSharedStorageMetadataLength p),
    ("remainingBudget" A..=) <$> Just (storageSharedStorageMetadataRemainingBudget p),
    ("bytesUsed" A..=) <$> Just (storageSharedStorageMetadataBytesUsed p)
    ]

-- | Type 'Storage.SharedStoragePrivateAggregationConfig'.
--   Represents a dictionary object passed in as privateAggregationConfig to
--   run or selectURL.
data StorageSharedStoragePrivateAggregationConfig = StorageSharedStoragePrivateAggregationConfig
  {
    -- | The chosen aggregation service deployment.
    storageSharedStoragePrivateAggregationConfigAggregationCoordinatorOrigin :: Maybe T.Text,
    -- | The context ID provided.
    storageSharedStoragePrivateAggregationConfigContextId :: Maybe T.Text,
    -- | Configures the maximum size allowed for filtering IDs.
    storageSharedStoragePrivateAggregationConfigFilteringIdMaxBytes :: Int,
    -- | The limit on the number of contributions in the final report.
    storageSharedStoragePrivateAggregationConfigMaxContributions :: Maybe Int
  }
  deriving (Eq, Show)
instance FromJSON StorageSharedStoragePrivateAggregationConfig where
  parseJSON = A.withObject "StorageSharedStoragePrivateAggregationConfig" $ \o -> StorageSharedStoragePrivateAggregationConfig
    <$> o A..:? "aggregationCoordinatorOrigin"
    <*> o A..:? "contextId"
    <*> o A..: "filteringIdMaxBytes"
    <*> o A..:? "maxContributions"
instance ToJSON StorageSharedStoragePrivateAggregationConfig where
  toJSON p = A.object $ catMaybes [
    ("aggregationCoordinatorOrigin" A..=) <$> (storageSharedStoragePrivateAggregationConfigAggregationCoordinatorOrigin p),
    ("contextId" A..=) <$> (storageSharedStoragePrivateAggregationConfigContextId p),
    ("filteringIdMaxBytes" A..=) <$> Just (storageSharedStoragePrivateAggregationConfigFilteringIdMaxBytes p),
    ("maxContributions" A..=) <$> (storageSharedStoragePrivateAggregationConfigMaxContributions p)
    ]

-- | Type 'Storage.SharedStorageReportingMetadata'.
--   Pair of reporting metadata details for a candidate URL for `selectURL()`.
data StorageSharedStorageReportingMetadata = StorageSharedStorageReportingMetadata
  {
    storageSharedStorageReportingMetadataEventType :: T.Text,
    storageSharedStorageReportingMetadataReportingUrl :: T.Text
  }
  deriving (Eq, Show)
instance FromJSON StorageSharedStorageReportingMetadata where
  parseJSON = A.withObject "StorageSharedStorageReportingMetadata" $ \o -> StorageSharedStorageReportingMetadata
    <$> o A..: "eventType"
    <*> o A..: "reportingUrl"
instance ToJSON StorageSharedStorageReportingMetadata where
  toJSON p = A.object $ catMaybes [
    ("eventType" A..=) <$> Just (storageSharedStorageReportingMetadataEventType p),
    ("reportingUrl" A..=) <$> Just (storageSharedStorageReportingMetadataReportingUrl p)
    ]

-- | Type 'Storage.SharedStorageUrlWithMetadata'.
--   Bundles a candidate URL with its reporting metadata.
data StorageSharedStorageUrlWithMetadata = StorageSharedStorageUrlWithMetadata
  {
    -- | Spec of candidate URL.
    storageSharedStorageUrlWithMetadataUrl :: T.Text,
    -- | Any associated reporting metadata.
    storageSharedStorageUrlWithMetadataReportingMetadata :: [StorageSharedStorageReportingMetadata]
  }
  deriving (Eq, Show)
instance FromJSON StorageSharedStorageUrlWithMetadata where
  parseJSON = A.withObject "StorageSharedStorageUrlWithMetadata" $ \o -> StorageSharedStorageUrlWithMetadata
    <$> o A..: "url"
    <*> o A..: "reportingMetadata"
instance ToJSON StorageSharedStorageUrlWithMetadata where
  toJSON p = A.object $ catMaybes [
    ("url" A..=) <$> Just (storageSharedStorageUrlWithMetadataUrl p),
    ("reportingMetadata" A..=) <$> Just (storageSharedStorageUrlWithMetadataReportingMetadata p)
    ]

-- | Type 'Storage.SharedStorageAccessParams'.
--   Bundles the parameters for shared storage access events whose
--   presence/absence can vary according to SharedStorageAccessType.
data StorageSharedStorageAccessParams = StorageSharedStorageAccessParams
  {
    -- | Spec of the module script URL.
    --   Present only for SharedStorageAccessMethods: addModule and
    --   createWorklet.
    storageSharedStorageAccessParamsScriptSourceUrl :: Maybe T.Text,
    -- | String denoting "context-origin", "script-origin", or a custom
    --   origin to be used as the worklet's data origin.
    --   Present only for SharedStorageAccessMethod: createWorklet.
    storageSharedStorageAccessParamsDataOrigin :: Maybe T.Text,
    -- | Name of the registered operation to be run.
    --   Present only for SharedStorageAccessMethods: run and selectURL.
    storageSharedStorageAccessParamsOperationName :: Maybe T.Text,
    -- | ID of the operation call.
    --   Present only for SharedStorageAccessMethods: run and selectURL.
    storageSharedStorageAccessParamsOperationId :: Maybe T.Text,
    -- | Whether or not to keep the worket alive for future run or selectURL
    --   calls.
    --   Present only for SharedStorageAccessMethods: run and selectURL.
    storageSharedStorageAccessParamsKeepAlive :: Maybe Bool,
    -- | Configures the private aggregation options.
    --   Present only for SharedStorageAccessMethods: run and selectURL.
    storageSharedStorageAccessParamsPrivateAggregationConfig :: Maybe StorageSharedStoragePrivateAggregationConfig,
    -- | The operation's serialized data in bytes (converted to a string).
    --   Present only for SharedStorageAccessMethods: run and selectURL.
    --   TODO(crbug.com/401011862): Consider updating this parameter to binary.
    storageSharedStorageAccessParamsSerializedData :: Maybe T.Text,
    -- | Array of candidate URLs' specs, along with any associated metadata.
    --   Present only for SharedStorageAccessMethod: selectURL.
    storageSharedStorageAccessParamsUrlsWithMetadata :: Maybe [StorageSharedStorageUrlWithMetadata],
    -- | Spec of the URN:UUID generated for a selectURL call.
    --   Present only for SharedStorageAccessMethod: selectURL.
    storageSharedStorageAccessParamsUrnUuid :: Maybe T.Text,
    -- | Key for a specific entry in an origin's shared storage.
    --   Present only for SharedStorageAccessMethods: set, append, delete, and
    --   get.
    storageSharedStorageAccessParamsKey :: Maybe T.Text,
    -- | Value for a specific entry in an origin's shared storage.
    --   Present only for SharedStorageAccessMethods: set and append.
    storageSharedStorageAccessParamsValue :: Maybe T.Text,
    -- | Whether or not to set an entry for a key if that key is already present.
    --   Present only for SharedStorageAccessMethod: set.
    storageSharedStorageAccessParamsIgnoreIfPresent :: Maybe Bool,
    -- | A number denoting the (0-based) order of the worklet's
    --   creation relative to all other shared storage worklets created by
    --   documents using the current storage partition.
    --   Present only for SharedStorageAccessMethods: addModule, createWorklet.
    storageSharedStorageAccessParamsWorkletOrdinal :: Maybe Int,
    -- | Hex representation of the DevTools token used as the TargetID for the
    --   associated shared storage worklet.
    --   Present only for SharedStorageAccessMethods: addModule, createWorklet,
    --   run, selectURL, and any other SharedStorageAccessMethod when the
    --   SharedStorageAccessScope is sharedStorageWorklet.
    storageSharedStorageAccessParamsWorkletTargetId :: Maybe BrowserTarget.TargetTargetID,
    -- | Name of the lock to be acquired, if present.
    --   Optionally present only for SharedStorageAccessMethods: batchUpdate,
    --   set, append, delete, and clear.
    storageSharedStorageAccessParamsWithLock :: Maybe T.Text,
    -- | If the method has been called as part of a batchUpdate, then this
    --   number identifies the batch to which it belongs.
    --   Optionally present only for SharedStorageAccessMethods:
    --   batchUpdate (required), set, append, delete, and clear.
    storageSharedStorageAccessParamsBatchUpdateId :: Maybe T.Text,
    -- | Number of modifier methods sent in batch.
    --   Present only for SharedStorageAccessMethod: batchUpdate.
    storageSharedStorageAccessParamsBatchSize :: Maybe Int
  }
  deriving (Eq, Show)
instance FromJSON StorageSharedStorageAccessParams where
  parseJSON = A.withObject "StorageSharedStorageAccessParams" $ \o -> StorageSharedStorageAccessParams
    <$> o A..:? "scriptSourceUrl"
    <*> o A..:? "dataOrigin"
    <*> o A..:? "operationName"
    <*> o A..:? "operationId"
    <*> o A..:? "keepAlive"
    <*> o A..:? "privateAggregationConfig"
    <*> o A..:? "serializedData"
    <*> o A..:? "urlsWithMetadata"
    <*> o A..:? "urnUuid"
    <*> o A..:? "key"
    <*> o A..:? "value"
    <*> o A..:? "ignoreIfPresent"
    <*> o A..:? "workletOrdinal"
    <*> o A..:? "workletTargetId"
    <*> o A..:? "withLock"
    <*> o A..:? "batchUpdateId"
    <*> o A..:? "batchSize"
instance ToJSON StorageSharedStorageAccessParams where
  toJSON p = A.object $ catMaybes [
    ("scriptSourceUrl" A..=) <$> (storageSharedStorageAccessParamsScriptSourceUrl p),
    ("dataOrigin" A..=) <$> (storageSharedStorageAccessParamsDataOrigin p),
    ("operationName" A..=) <$> (storageSharedStorageAccessParamsOperationName p),
    ("operationId" A..=) <$> (storageSharedStorageAccessParamsOperationId p),
    ("keepAlive" A..=) <$> (storageSharedStorageAccessParamsKeepAlive p),
    ("privateAggregationConfig" A..=) <$> (storageSharedStorageAccessParamsPrivateAggregationConfig p),
    ("serializedData" A..=) <$> (storageSharedStorageAccessParamsSerializedData p),
    ("urlsWithMetadata" A..=) <$> (storageSharedStorageAccessParamsUrlsWithMetadata p),
    ("urnUuid" A..=) <$> (storageSharedStorageAccessParamsUrnUuid p),
    ("key" A..=) <$> (storageSharedStorageAccessParamsKey p),
    ("value" A..=) <$> (storageSharedStorageAccessParamsValue p),
    ("ignoreIfPresent" A..=) <$> (storageSharedStorageAccessParamsIgnoreIfPresent p),
    ("workletOrdinal" A..=) <$> (storageSharedStorageAccessParamsWorkletOrdinal p),
    ("workletTargetId" A..=) <$> (storageSharedStorageAccessParamsWorkletTargetId p),
    ("withLock" A..=) <$> (storageSharedStorageAccessParamsWithLock p),
    ("batchUpdateId" A..=) <$> (storageSharedStorageAccessParamsBatchUpdateId p),
    ("batchSize" A..=) <$> (storageSharedStorageAccessParamsBatchSize p)
    ]

-- | Type 'Storage.StorageBucketsDurability'.
data StorageStorageBucketsDurability = StorageStorageBucketsDurabilityRelaxed | StorageStorageBucketsDurabilityStrict
  deriving (Ord, Eq, Show, Read)
instance FromJSON StorageStorageBucketsDurability where
  parseJSON = A.withText "StorageStorageBucketsDurability" $ \v -> case v of
    "relaxed" -> pure StorageStorageBucketsDurabilityRelaxed
    "strict" -> pure StorageStorageBucketsDurabilityStrict
    "_" -> fail "failed to parse StorageStorageBucketsDurability"
instance ToJSON StorageStorageBucketsDurability where
  toJSON v = A.String $ case v of
    StorageStorageBucketsDurabilityRelaxed -> "relaxed"
    StorageStorageBucketsDurabilityStrict -> "strict"

-- | Type 'Storage.StorageBucket'.
data StorageStorageBucket = StorageStorageBucket
  {
    storageStorageBucketStorageKey :: StorageSerializedStorageKey,
    -- | If not specified, it is the default bucket of the storageKey.
    storageStorageBucketName :: Maybe T.Text
  }
  deriving (Eq, Show)
instance FromJSON StorageStorageBucket where
  parseJSON = A.withObject "StorageStorageBucket" $ \o -> StorageStorageBucket
    <$> o A..: "storageKey"
    <*> o A..:? "name"
instance ToJSON StorageStorageBucket where
  toJSON p = A.object $ catMaybes [
    ("storageKey" A..=) <$> Just (storageStorageBucketStorageKey p),
    ("name" A..=) <$> (storageStorageBucketName p)
    ]

-- | Type 'Storage.StorageBucketInfo'.
data StorageStorageBucketInfo = StorageStorageBucketInfo
  {
    storageStorageBucketInfoBucket :: StorageStorageBucket,
    storageStorageBucketInfoId :: T.Text,
    storageStorageBucketInfoExpiration :: DOMNetworkEmulationPageSecurity.NetworkTimeSinceEpoch,
    -- | Storage quota (bytes).
    storageStorageBucketInfoQuota :: Double,
    storageStorageBucketInfoPersistent :: Bool,
    storageStorageBucketInfoDurability :: StorageStorageBucketsDurability
  }
  deriving (Eq, Show)
instance FromJSON StorageStorageBucketInfo where
  parseJSON = A.withObject "StorageStorageBucketInfo" $ \o -> StorageStorageBucketInfo
    <$> o A..: "bucket"
    <*> o A..: "id"
    <*> o A..: "expiration"
    <*> o A..: "quota"
    <*> o A..: "persistent"
    <*> o A..: "durability"
instance ToJSON StorageStorageBucketInfo where
  toJSON p = A.object $ catMaybes [
    ("bucket" A..=) <$> Just (storageStorageBucketInfoBucket p),
    ("id" A..=) <$> Just (storageStorageBucketInfoId p),
    ("expiration" A..=) <$> Just (storageStorageBucketInfoExpiration p),
    ("quota" A..=) <$> Just (storageStorageBucketInfoQuota p),
    ("persistent" A..=) <$> Just (storageStorageBucketInfoPersistent p),
    ("durability" A..=) <$> Just (storageStorageBucketInfoDurability p)
    ]

-- | Type 'Storage.RelatedWebsiteSet'.
--   A single Related Website Set object.
data StorageRelatedWebsiteSet = StorageRelatedWebsiteSet
  {
    -- | The primary site of this set, along with the ccTLDs if there is any.
    storageRelatedWebsiteSetPrimarySites :: [T.Text],
    -- | The associated sites of this set, along with the ccTLDs if there is any.
    storageRelatedWebsiteSetAssociatedSites :: [T.Text],
    -- | The service sites of this set, along with the ccTLDs if there is any.
    storageRelatedWebsiteSetServiceSites :: [T.Text]
  }
  deriving (Eq, Show)
instance FromJSON StorageRelatedWebsiteSet where
  parseJSON = A.withObject "StorageRelatedWebsiteSet" $ \o -> StorageRelatedWebsiteSet
    <$> o A..: "primarySites"
    <*> o A..: "associatedSites"
    <*> o A..: "serviceSites"
instance ToJSON StorageRelatedWebsiteSet where
  toJSON p = A.object $ catMaybes [
    ("primarySites" A..=) <$> Just (storageRelatedWebsiteSetPrimarySites p),
    ("associatedSites" A..=) <$> Just (storageRelatedWebsiteSetAssociatedSites p),
    ("serviceSites" A..=) <$> Just (storageRelatedWebsiteSetServiceSites p)
    ]

-- | Type of the 'Storage.cacheStorageContentUpdated' event.
data StorageCacheStorageContentUpdated = StorageCacheStorageContentUpdated
  {
    -- | Origin to update.
    storageCacheStorageContentUpdatedOrigin :: T.Text,
    -- | Storage key to update.
    storageCacheStorageContentUpdatedStorageKey :: T.Text,
    -- | Storage bucket to update.
    storageCacheStorageContentUpdatedBucketId :: T.Text,
    -- | Name of cache in origin.
    storageCacheStorageContentUpdatedCacheName :: T.Text
  }
  deriving (Eq, Show)
instance FromJSON StorageCacheStorageContentUpdated where
  parseJSON = A.withObject "StorageCacheStorageContentUpdated" $ \o -> StorageCacheStorageContentUpdated
    <$> o A..: "origin"
    <*> o A..: "storageKey"
    <*> o A..: "bucketId"
    <*> o A..: "cacheName"
instance Event StorageCacheStorageContentUpdated where
  eventName _ = "Storage.cacheStorageContentUpdated"

-- | Type of the 'Storage.cacheStorageListUpdated' event.
data StorageCacheStorageListUpdated = StorageCacheStorageListUpdated
  {
    -- | Origin to update.
    storageCacheStorageListUpdatedOrigin :: T.Text,
    -- | Storage key to update.
    storageCacheStorageListUpdatedStorageKey :: T.Text,
    -- | Storage bucket to update.
    storageCacheStorageListUpdatedBucketId :: T.Text
  }
  deriving (Eq, Show)
instance FromJSON StorageCacheStorageListUpdated where
  parseJSON = A.withObject "StorageCacheStorageListUpdated" $ \o -> StorageCacheStorageListUpdated
    <$> o A..: "origin"
    <*> o A..: "storageKey"
    <*> o A..: "bucketId"
instance Event StorageCacheStorageListUpdated where
  eventName _ = "Storage.cacheStorageListUpdated"

-- | Type of the 'Storage.indexedDBContentUpdated' event.
data StorageIndexedDBContentUpdated = StorageIndexedDBContentUpdated
  {
    -- | Origin to update.
    storageIndexedDBContentUpdatedOrigin :: T.Text,
    -- | Storage key to update.
    storageIndexedDBContentUpdatedStorageKey :: T.Text,
    -- | Storage bucket to update.
    storageIndexedDBContentUpdatedBucketId :: T.Text,
    -- | Database to update.
    storageIndexedDBContentUpdatedDatabaseName :: T.Text,
    -- | ObjectStore to update.
    storageIndexedDBContentUpdatedObjectStoreName :: T.Text
  }
  deriving (Eq, Show)
instance FromJSON StorageIndexedDBContentUpdated where
  parseJSON = A.withObject "StorageIndexedDBContentUpdated" $ \o -> StorageIndexedDBContentUpdated
    <$> o A..: "origin"
    <*> o A..: "storageKey"
    <*> o A..: "bucketId"
    <*> o A..: "databaseName"
    <*> o A..: "objectStoreName"
instance Event StorageIndexedDBContentUpdated where
  eventName _ = "Storage.indexedDBContentUpdated"

-- | Type of the 'Storage.indexedDBListUpdated' event.
data StorageIndexedDBListUpdated = StorageIndexedDBListUpdated
  {
    -- | Origin to update.
    storageIndexedDBListUpdatedOrigin :: T.Text,
    -- | Storage key to update.
    storageIndexedDBListUpdatedStorageKey :: T.Text,
    -- | Storage bucket to update.
    storageIndexedDBListUpdatedBucketId :: T.Text
  }
  deriving (Eq, Show)
instance FromJSON StorageIndexedDBListUpdated where
  parseJSON = A.withObject "StorageIndexedDBListUpdated" $ \o -> StorageIndexedDBListUpdated
    <$> o A..: "origin"
    <*> o A..: "storageKey"
    <*> o A..: "bucketId"
instance Event StorageIndexedDBListUpdated where
  eventName _ = "Storage.indexedDBListUpdated"

-- | Type of the 'Storage.sharedStorageAccessed' event.
data StorageSharedStorageAccessed = StorageSharedStorageAccessed
  {
    -- | Time of the access.
    storageSharedStorageAccessedAccessTime :: DOMNetworkEmulationPageSecurity.NetworkTimeSinceEpoch,
    -- | Enum value indicating the access scope.
    storageSharedStorageAccessedScope :: StorageSharedStorageAccessScope,
    -- | Enum value indicating the Shared Storage API method invoked.
    storageSharedStorageAccessedMethod :: StorageSharedStorageAccessMethod,
    -- | DevTools Frame Token for the primary frame tree's root.
    storageSharedStorageAccessedMainFrameId :: DOMNetworkEmulationPageSecurity.PageFrameId,
    -- | Serialization of the origin owning the Shared Storage data.
    storageSharedStorageAccessedOwnerOrigin :: T.Text,
    -- | Serialization of the site owning the Shared Storage data.
    storageSharedStorageAccessedOwnerSite :: T.Text,
    -- | The sub-parameters wrapped by `params` are all optional and their
    --   presence/absence depends on `type`.
    storageSharedStorageAccessedParams :: StorageSharedStorageAccessParams
  }
  deriving (Eq, Show)
instance FromJSON StorageSharedStorageAccessed where
  parseJSON = A.withObject "StorageSharedStorageAccessed" $ \o -> StorageSharedStorageAccessed
    <$> o A..: "accessTime"
    <*> o A..: "scope"
    <*> o A..: "method"
    <*> o A..: "mainFrameId"
    <*> o A..: "ownerOrigin"
    <*> o A..: "ownerSite"
    <*> o A..: "params"
instance Event StorageSharedStorageAccessed where
  eventName _ = "Storage.sharedStorageAccessed"

-- | Type of the 'Storage.sharedStorageWorkletOperationExecutionFinished' event.
data StorageSharedStorageWorkletOperationExecutionFinished = StorageSharedStorageWorkletOperationExecutionFinished
  {
    -- | Time that the operation finished.
    storageSharedStorageWorkletOperationExecutionFinishedFinishedTime :: DOMNetworkEmulationPageSecurity.NetworkTimeSinceEpoch,
    -- | Time, in microseconds, from start of shared storage JS API call until
    --   end of operation execution in the worklet.
    storageSharedStorageWorkletOperationExecutionFinishedExecutionTime :: Int,
    -- | Enum value indicating the Shared Storage API method invoked.
    storageSharedStorageWorkletOperationExecutionFinishedMethod :: StorageSharedStorageAccessMethod,
    -- | ID of the operation call.
    storageSharedStorageWorkletOperationExecutionFinishedOperationId :: T.Text,
    -- | Hex representation of the DevTools token used as the TargetID for the
    --   associated shared storage worklet.
    storageSharedStorageWorkletOperationExecutionFinishedWorkletTargetId :: BrowserTarget.TargetTargetID,
    -- | DevTools Frame Token for the primary frame tree's root.
    storageSharedStorageWorkletOperationExecutionFinishedMainFrameId :: DOMNetworkEmulationPageSecurity.PageFrameId,
    -- | Serialization of the origin owning the Shared Storage data.
    storageSharedStorageWorkletOperationExecutionFinishedOwnerOrigin :: T.Text
  }
  deriving (Eq, Show)
instance FromJSON StorageSharedStorageWorkletOperationExecutionFinished where
  parseJSON = A.withObject "StorageSharedStorageWorkletOperationExecutionFinished" $ \o -> StorageSharedStorageWorkletOperationExecutionFinished
    <$> o A..: "finishedTime"
    <*> o A..: "executionTime"
    <*> o A..: "method"
    <*> o A..: "operationId"
    <*> o A..: "workletTargetId"
    <*> o A..: "mainFrameId"
    <*> o A..: "ownerOrigin"
instance Event StorageSharedStorageWorkletOperationExecutionFinished where
  eventName _ = "Storage.sharedStorageWorkletOperationExecutionFinished"

-- | Type of the 'Storage.storageBucketCreatedOrUpdated' event.
data StorageStorageBucketCreatedOrUpdated = StorageStorageBucketCreatedOrUpdated
  {
    storageStorageBucketCreatedOrUpdatedBucketInfo :: StorageStorageBucketInfo
  }
  deriving (Eq, Show)
instance FromJSON StorageStorageBucketCreatedOrUpdated where
  parseJSON = A.withObject "StorageStorageBucketCreatedOrUpdated" $ \o -> StorageStorageBucketCreatedOrUpdated
    <$> o A..: "bucketInfo"
instance Event StorageStorageBucketCreatedOrUpdated where
  eventName _ = "Storage.storageBucketCreatedOrUpdated"

-- | Type of the 'Storage.storageBucketDeleted' event.
data StorageStorageBucketDeleted = StorageStorageBucketDeleted
  {
    storageStorageBucketDeletedBucketId :: T.Text
  }
  deriving (Eq, Show)
instance FromJSON StorageStorageBucketDeleted where
  parseJSON = A.withObject "StorageStorageBucketDeleted" $ \o -> StorageStorageBucketDeleted
    <$> o A..: "bucketId"
instance Event StorageStorageBucketDeleted where
  eventName _ = "Storage.storageBucketDeleted"

-- | Returns storage key for the given frame. If no frame ID is provided,
--   the storage key of the target executing this command is returned.

-- | Parameters of the 'Storage.getStorageKey' command.
data PStorageGetStorageKey = PStorageGetStorageKey
  {
    pStorageGetStorageKeyFrameId :: Maybe DOMNetworkEmulationPageSecurity.PageFrameId
  }
  deriving (Eq, Show)
pStorageGetStorageKey
  :: PStorageGetStorageKey
pStorageGetStorageKey
  = PStorageGetStorageKey
    Nothing
instance ToJSON PStorageGetStorageKey where
  toJSON p = A.object $ catMaybes [
    ("frameId" A..=) <$> (pStorageGetStorageKeyFrameId p)
    ]
data StorageGetStorageKey = StorageGetStorageKey
  {
    storageGetStorageKeyStorageKey :: StorageSerializedStorageKey
  }
  deriving (Eq, Show)
instance FromJSON StorageGetStorageKey where
  parseJSON = A.withObject "StorageGetStorageKey" $ \o -> StorageGetStorageKey
    <$> o A..: "storageKey"
instance Command PStorageGetStorageKey where
  type CommandResponse PStorageGetStorageKey = StorageGetStorageKey
  commandName _ = "Storage.getStorageKey"

-- | Clears storage for origin.

-- | Parameters of the 'Storage.clearDataForOrigin' command.
data PStorageClearDataForOrigin = PStorageClearDataForOrigin
  {
    -- | Security origin.
    pStorageClearDataForOriginOrigin :: T.Text,
    -- | Comma separated list of StorageType to clear.
    pStorageClearDataForOriginStorageTypes :: T.Text
  }
  deriving (Eq, Show)
pStorageClearDataForOrigin
  {-
  -- | Security origin.
  -}
  :: T.Text
  {-
  -- | Comma separated list of StorageType to clear.
  -}
  -> T.Text
  -> PStorageClearDataForOrigin
pStorageClearDataForOrigin
  arg_pStorageClearDataForOriginOrigin
  arg_pStorageClearDataForOriginStorageTypes
  = PStorageClearDataForOrigin
    arg_pStorageClearDataForOriginOrigin
    arg_pStorageClearDataForOriginStorageTypes
instance ToJSON PStorageClearDataForOrigin where
  toJSON p = A.object $ catMaybes [
    ("origin" A..=) <$> Just (pStorageClearDataForOriginOrigin p),
    ("storageTypes" A..=) <$> Just (pStorageClearDataForOriginStorageTypes p)
    ]
instance Command PStorageClearDataForOrigin where
  type CommandResponse PStorageClearDataForOrigin = ()
  commandName _ = "Storage.clearDataForOrigin"
  fromJSON = const . A.Success . const ()

-- | Clears storage for storage key.

-- | Parameters of the 'Storage.clearDataForStorageKey' command.
data PStorageClearDataForStorageKey = PStorageClearDataForStorageKey
  {
    -- | Storage key.
    pStorageClearDataForStorageKeyStorageKey :: T.Text,
    -- | Comma separated list of StorageType to clear.
    pStorageClearDataForStorageKeyStorageTypes :: T.Text
  }
  deriving (Eq, Show)
pStorageClearDataForStorageKey
  {-
  -- | Storage key.
  -}
  :: T.Text
  {-
  -- | Comma separated list of StorageType to clear.
  -}
  -> T.Text
  -> PStorageClearDataForStorageKey
pStorageClearDataForStorageKey
  arg_pStorageClearDataForStorageKeyStorageKey
  arg_pStorageClearDataForStorageKeyStorageTypes
  = PStorageClearDataForStorageKey
    arg_pStorageClearDataForStorageKeyStorageKey
    arg_pStorageClearDataForStorageKeyStorageTypes
instance ToJSON PStorageClearDataForStorageKey where
  toJSON p = A.object $ catMaybes [
    ("storageKey" A..=) <$> Just (pStorageClearDataForStorageKeyStorageKey p),
    ("storageTypes" A..=) <$> Just (pStorageClearDataForStorageKeyStorageTypes p)
    ]
instance Command PStorageClearDataForStorageKey where
  type CommandResponse PStorageClearDataForStorageKey = ()
  commandName _ = "Storage.clearDataForStorageKey"
  fromJSON = const . A.Success . const ()

-- | Returns all browser cookies.

-- | Parameters of the 'Storage.getCookies' command.
data PStorageGetCookies = PStorageGetCookies
  {
    -- | Browser context to use when called on the browser endpoint.
    pStorageGetCookiesBrowserContextId :: Maybe BrowserTarget.BrowserBrowserContextID
  }
  deriving (Eq, Show)
pStorageGetCookies
  :: PStorageGetCookies
pStorageGetCookies
  = PStorageGetCookies
    Nothing
instance ToJSON PStorageGetCookies where
  toJSON p = A.object $ catMaybes [
    ("browserContextId" A..=) <$> (pStorageGetCookiesBrowserContextId p)
    ]
data StorageGetCookies = StorageGetCookies
  {
    -- | Array of cookie objects.
    storageGetCookiesCookies :: [DOMNetworkEmulationPageSecurity.NetworkCookie]
  }
  deriving (Eq, Show)
instance FromJSON StorageGetCookies where
  parseJSON = A.withObject "StorageGetCookies" $ \o -> StorageGetCookies
    <$> o A..: "cookies"
instance Command PStorageGetCookies where
  type CommandResponse PStorageGetCookies = StorageGetCookies
  commandName _ = "Storage.getCookies"

-- | Sets given cookies.

-- | Parameters of the 'Storage.setCookies' command.
data PStorageSetCookies = PStorageSetCookies
  {
    -- | Cookies to be set.
    pStorageSetCookiesCookies :: [DOMNetworkEmulationPageSecurity.NetworkCookieParam],
    -- | Browser context to use when called on the browser endpoint.
    pStorageSetCookiesBrowserContextId :: Maybe BrowserTarget.BrowserBrowserContextID
  }
  deriving (Eq, Show)
pStorageSetCookies
  {-
  -- | Cookies to be set.
  -}
  :: [DOMNetworkEmulationPageSecurity.NetworkCookieParam]
  -> PStorageSetCookies
pStorageSetCookies
  arg_pStorageSetCookiesCookies
  = PStorageSetCookies
    arg_pStorageSetCookiesCookies
    Nothing
instance ToJSON PStorageSetCookies where
  toJSON p = A.object $ catMaybes [
    ("cookies" A..=) <$> Just (pStorageSetCookiesCookies p),
    ("browserContextId" A..=) <$> (pStorageSetCookiesBrowserContextId p)
    ]
instance Command PStorageSetCookies where
  type CommandResponse PStorageSetCookies = ()
  commandName _ = "Storage.setCookies"
  fromJSON = const . A.Success . const ()

-- | Clears cookies.

-- | Parameters of the 'Storage.clearCookies' command.
data PStorageClearCookies = PStorageClearCookies
  {
    -- | Browser context to use when called on the browser endpoint.
    pStorageClearCookiesBrowserContextId :: Maybe BrowserTarget.BrowserBrowserContextID
  }
  deriving (Eq, Show)
pStorageClearCookies
  :: PStorageClearCookies
pStorageClearCookies
  = PStorageClearCookies
    Nothing
instance ToJSON PStorageClearCookies where
  toJSON p = A.object $ catMaybes [
    ("browserContextId" A..=) <$> (pStorageClearCookiesBrowserContextId p)
    ]
instance Command PStorageClearCookies where
  type CommandResponse PStorageClearCookies = ()
  commandName _ = "Storage.clearCookies"
  fromJSON = const . A.Success . const ()

-- | Returns usage and quota in bytes.

-- | Parameters of the 'Storage.getUsageAndQuota' command.
data PStorageGetUsageAndQuota = PStorageGetUsageAndQuota
  {
    -- | Security origin.
    pStorageGetUsageAndQuotaOrigin :: T.Text
  }
  deriving (Eq, Show)
pStorageGetUsageAndQuota
  {-
  -- | Security origin.
  -}
  :: T.Text
  -> PStorageGetUsageAndQuota
pStorageGetUsageAndQuota
  arg_pStorageGetUsageAndQuotaOrigin
  = PStorageGetUsageAndQuota
    arg_pStorageGetUsageAndQuotaOrigin
instance ToJSON PStorageGetUsageAndQuota where
  toJSON p = A.object $ catMaybes [
    ("origin" A..=) <$> Just (pStorageGetUsageAndQuotaOrigin p)
    ]
data StorageGetUsageAndQuota = StorageGetUsageAndQuota
  {
    -- | Storage usage (bytes).
    storageGetUsageAndQuotaUsage :: Double,
    -- | Storage quota (bytes).
    storageGetUsageAndQuotaQuota :: Double,
    -- | Whether or not the origin has an active storage quota override
    storageGetUsageAndQuotaOverrideActive :: Bool,
    -- | Storage usage per type (bytes).
    storageGetUsageAndQuotaUsageBreakdown :: [StorageUsageForType]
  }
  deriving (Eq, Show)
instance FromJSON StorageGetUsageAndQuota where
  parseJSON = A.withObject "StorageGetUsageAndQuota" $ \o -> StorageGetUsageAndQuota
    <$> o A..: "usage"
    <*> o A..: "quota"
    <*> o A..: "overrideActive"
    <*> o A..: "usageBreakdown"
instance Command PStorageGetUsageAndQuota where
  type CommandResponse PStorageGetUsageAndQuota = StorageGetUsageAndQuota
  commandName _ = "Storage.getUsageAndQuota"

-- | Override quota for the specified origin

-- | Parameters of the 'Storage.overrideQuotaForOrigin' command.
data PStorageOverrideQuotaForOrigin = PStorageOverrideQuotaForOrigin
  {
    -- | Security origin.
    pStorageOverrideQuotaForOriginOrigin :: T.Text,
    -- | The quota size (in bytes) to override the original quota with.
    --   If this is called multiple times, the overridden quota will be equal to
    --   the quotaSize provided in the final call. If this is called without
    --   specifying a quotaSize, the quota will be reset to the default value for
    --   the specified origin. If this is called multiple times with different
    --   origins, the override will be maintained for each origin until it is
    --   disabled (called without a quotaSize).
    pStorageOverrideQuotaForOriginQuotaSize :: Maybe Double
  }
  deriving (Eq, Show)
pStorageOverrideQuotaForOrigin
  {-
  -- | Security origin.
  -}
  :: T.Text
  -> PStorageOverrideQuotaForOrigin
pStorageOverrideQuotaForOrigin
  arg_pStorageOverrideQuotaForOriginOrigin
  = PStorageOverrideQuotaForOrigin
    arg_pStorageOverrideQuotaForOriginOrigin
    Nothing
instance ToJSON PStorageOverrideQuotaForOrigin where
  toJSON p = A.object $ catMaybes [
    ("origin" A..=) <$> Just (pStorageOverrideQuotaForOriginOrigin p),
    ("quotaSize" A..=) <$> (pStorageOverrideQuotaForOriginQuotaSize p)
    ]
instance Command PStorageOverrideQuotaForOrigin where
  type CommandResponse PStorageOverrideQuotaForOrigin = ()
  commandName _ = "Storage.overrideQuotaForOrigin"
  fromJSON = const . A.Success . const ()

-- | Registers origin to be notified when an update occurs to its cache storage list.

-- | Parameters of the 'Storage.trackCacheStorageForOrigin' command.
data PStorageTrackCacheStorageForOrigin = PStorageTrackCacheStorageForOrigin
  {
    -- | Security origin.
    pStorageTrackCacheStorageForOriginOrigin :: T.Text
  }
  deriving (Eq, Show)
pStorageTrackCacheStorageForOrigin
  {-
  -- | Security origin.
  -}
  :: T.Text
  -> PStorageTrackCacheStorageForOrigin
pStorageTrackCacheStorageForOrigin
  arg_pStorageTrackCacheStorageForOriginOrigin
  = PStorageTrackCacheStorageForOrigin
    arg_pStorageTrackCacheStorageForOriginOrigin
instance ToJSON PStorageTrackCacheStorageForOrigin where
  toJSON p = A.object $ catMaybes [
    ("origin" A..=) <$> Just (pStorageTrackCacheStorageForOriginOrigin p)
    ]
instance Command PStorageTrackCacheStorageForOrigin where
  type CommandResponse PStorageTrackCacheStorageForOrigin = ()
  commandName _ = "Storage.trackCacheStorageForOrigin"
  fromJSON = const . A.Success . const ()

-- | Registers storage key to be notified when an update occurs to its cache storage list.

-- | Parameters of the 'Storage.trackCacheStorageForStorageKey' command.
data PStorageTrackCacheStorageForStorageKey = PStorageTrackCacheStorageForStorageKey
  {
    -- | Storage key.
    pStorageTrackCacheStorageForStorageKeyStorageKey :: T.Text
  }
  deriving (Eq, Show)
pStorageTrackCacheStorageForStorageKey
  {-
  -- | Storage key.
  -}
  :: T.Text
  -> PStorageTrackCacheStorageForStorageKey
pStorageTrackCacheStorageForStorageKey
  arg_pStorageTrackCacheStorageForStorageKeyStorageKey
  = PStorageTrackCacheStorageForStorageKey
    arg_pStorageTrackCacheStorageForStorageKeyStorageKey
instance ToJSON PStorageTrackCacheStorageForStorageKey where
  toJSON p = A.object $ catMaybes [
    ("storageKey" A..=) <$> Just (pStorageTrackCacheStorageForStorageKeyStorageKey p)
    ]
instance Command PStorageTrackCacheStorageForStorageKey where
  type CommandResponse PStorageTrackCacheStorageForStorageKey = ()
  commandName _ = "Storage.trackCacheStorageForStorageKey"
  fromJSON = const . A.Success . const ()

-- | Registers origin to be notified when an update occurs to its IndexedDB.

-- | Parameters of the 'Storage.trackIndexedDBForOrigin' command.
data PStorageTrackIndexedDBForOrigin = PStorageTrackIndexedDBForOrigin
  {
    -- | Security origin.
    pStorageTrackIndexedDBForOriginOrigin :: T.Text
  }
  deriving (Eq, Show)
pStorageTrackIndexedDBForOrigin
  {-
  -- | Security origin.
  -}
  :: T.Text
  -> PStorageTrackIndexedDBForOrigin
pStorageTrackIndexedDBForOrigin
  arg_pStorageTrackIndexedDBForOriginOrigin
  = PStorageTrackIndexedDBForOrigin
    arg_pStorageTrackIndexedDBForOriginOrigin
instance ToJSON PStorageTrackIndexedDBForOrigin where
  toJSON p = A.object $ catMaybes [
    ("origin" A..=) <$> Just (pStorageTrackIndexedDBForOriginOrigin p)
    ]
instance Command PStorageTrackIndexedDBForOrigin where
  type CommandResponse PStorageTrackIndexedDBForOrigin = ()
  commandName _ = "Storage.trackIndexedDBForOrigin"
  fromJSON = const . A.Success . const ()

-- | Registers storage key to be notified when an update occurs to its IndexedDB.

-- | Parameters of the 'Storage.trackIndexedDBForStorageKey' command.
data PStorageTrackIndexedDBForStorageKey = PStorageTrackIndexedDBForStorageKey
  {
    -- | Storage key.
    pStorageTrackIndexedDBForStorageKeyStorageKey :: T.Text
  }
  deriving (Eq, Show)
pStorageTrackIndexedDBForStorageKey
  {-
  -- | Storage key.
  -}
  :: T.Text
  -> PStorageTrackIndexedDBForStorageKey
pStorageTrackIndexedDBForStorageKey
  arg_pStorageTrackIndexedDBForStorageKeyStorageKey
  = PStorageTrackIndexedDBForStorageKey
    arg_pStorageTrackIndexedDBForStorageKeyStorageKey
instance ToJSON PStorageTrackIndexedDBForStorageKey where
  toJSON p = A.object $ catMaybes [
    ("storageKey" A..=) <$> Just (pStorageTrackIndexedDBForStorageKeyStorageKey p)
    ]
instance Command PStorageTrackIndexedDBForStorageKey where
  type CommandResponse PStorageTrackIndexedDBForStorageKey = ()
  commandName _ = "Storage.trackIndexedDBForStorageKey"
  fromJSON = const . A.Success . const ()

-- | Unregisters origin from receiving notifications for cache storage.

-- | Parameters of the 'Storage.untrackCacheStorageForOrigin' command.
data PStorageUntrackCacheStorageForOrigin = PStorageUntrackCacheStorageForOrigin
  {
    -- | Security origin.
    pStorageUntrackCacheStorageForOriginOrigin :: T.Text
  }
  deriving (Eq, Show)
pStorageUntrackCacheStorageForOrigin
  {-
  -- | Security origin.
  -}
  :: T.Text
  -> PStorageUntrackCacheStorageForOrigin
pStorageUntrackCacheStorageForOrigin
  arg_pStorageUntrackCacheStorageForOriginOrigin
  = PStorageUntrackCacheStorageForOrigin
    arg_pStorageUntrackCacheStorageForOriginOrigin
instance ToJSON PStorageUntrackCacheStorageForOrigin where
  toJSON p = A.object $ catMaybes [
    ("origin" A..=) <$> Just (pStorageUntrackCacheStorageForOriginOrigin p)
    ]
instance Command PStorageUntrackCacheStorageForOrigin where
  type CommandResponse PStorageUntrackCacheStorageForOrigin = ()
  commandName _ = "Storage.untrackCacheStorageForOrigin"
  fromJSON = const . A.Success . const ()

-- | Unregisters storage key from receiving notifications for cache storage.

-- | Parameters of the 'Storage.untrackCacheStorageForStorageKey' command.
data PStorageUntrackCacheStorageForStorageKey = PStorageUntrackCacheStorageForStorageKey
  {
    -- | Storage key.
    pStorageUntrackCacheStorageForStorageKeyStorageKey :: T.Text
  }
  deriving (Eq, Show)
pStorageUntrackCacheStorageForStorageKey
  {-
  -- | Storage key.
  -}
  :: T.Text
  -> PStorageUntrackCacheStorageForStorageKey
pStorageUntrackCacheStorageForStorageKey
  arg_pStorageUntrackCacheStorageForStorageKeyStorageKey
  = PStorageUntrackCacheStorageForStorageKey
    arg_pStorageUntrackCacheStorageForStorageKeyStorageKey
instance ToJSON PStorageUntrackCacheStorageForStorageKey where
  toJSON p = A.object $ catMaybes [
    ("storageKey" A..=) <$> Just (pStorageUntrackCacheStorageForStorageKeyStorageKey p)
    ]
instance Command PStorageUntrackCacheStorageForStorageKey where
  type CommandResponse PStorageUntrackCacheStorageForStorageKey = ()
  commandName _ = "Storage.untrackCacheStorageForStorageKey"
  fromJSON = const . A.Success . const ()

-- | Unregisters origin from receiving notifications for IndexedDB.

-- | Parameters of the 'Storage.untrackIndexedDBForOrigin' command.
data PStorageUntrackIndexedDBForOrigin = PStorageUntrackIndexedDBForOrigin
  {
    -- | Security origin.
    pStorageUntrackIndexedDBForOriginOrigin :: T.Text
  }
  deriving (Eq, Show)
pStorageUntrackIndexedDBForOrigin
  {-
  -- | Security origin.
  -}
  :: T.Text
  -> PStorageUntrackIndexedDBForOrigin
pStorageUntrackIndexedDBForOrigin
  arg_pStorageUntrackIndexedDBForOriginOrigin
  = PStorageUntrackIndexedDBForOrigin
    arg_pStorageUntrackIndexedDBForOriginOrigin
instance ToJSON PStorageUntrackIndexedDBForOrigin where
  toJSON p = A.object $ catMaybes [
    ("origin" A..=) <$> Just (pStorageUntrackIndexedDBForOriginOrigin p)
    ]
instance Command PStorageUntrackIndexedDBForOrigin where
  type CommandResponse PStorageUntrackIndexedDBForOrigin = ()
  commandName _ = "Storage.untrackIndexedDBForOrigin"
  fromJSON = const . A.Success . const ()

-- | Unregisters storage key from receiving notifications for IndexedDB.

-- | Parameters of the 'Storage.untrackIndexedDBForStorageKey' command.
data PStorageUntrackIndexedDBForStorageKey = PStorageUntrackIndexedDBForStorageKey
  {
    -- | Storage key.
    pStorageUntrackIndexedDBForStorageKeyStorageKey :: T.Text
  }
  deriving (Eq, Show)
pStorageUntrackIndexedDBForStorageKey
  {-
  -- | Storage key.
  -}
  :: T.Text
  -> PStorageUntrackIndexedDBForStorageKey
pStorageUntrackIndexedDBForStorageKey
  arg_pStorageUntrackIndexedDBForStorageKeyStorageKey
  = PStorageUntrackIndexedDBForStorageKey
    arg_pStorageUntrackIndexedDBForStorageKeyStorageKey
instance ToJSON PStorageUntrackIndexedDBForStorageKey where
  toJSON p = A.object $ catMaybes [
    ("storageKey" A..=) <$> Just (pStorageUntrackIndexedDBForStorageKeyStorageKey p)
    ]
instance Command PStorageUntrackIndexedDBForStorageKey where
  type CommandResponse PStorageUntrackIndexedDBForStorageKey = ()
  commandName _ = "Storage.untrackIndexedDBForStorageKey"
  fromJSON = const . A.Success . const ()

-- | Returns the number of stored Trust Tokens per issuer for the
--   current browsing context.

-- | Parameters of the 'Storage.getTrustTokens' command.
data PStorageGetTrustTokens = PStorageGetTrustTokens
  deriving (Eq, Show)
pStorageGetTrustTokens
  :: PStorageGetTrustTokens
pStorageGetTrustTokens
  = PStorageGetTrustTokens
instance ToJSON PStorageGetTrustTokens where
  toJSON _ = A.Null
data StorageGetTrustTokens = StorageGetTrustTokens
  {
    storageGetTrustTokensTokens :: [StorageTrustTokens]
  }
  deriving (Eq, Show)
instance FromJSON StorageGetTrustTokens where
  parseJSON = A.withObject "StorageGetTrustTokens" $ \o -> StorageGetTrustTokens
    <$> o A..: "tokens"
instance Command PStorageGetTrustTokens where
  type CommandResponse PStorageGetTrustTokens = StorageGetTrustTokens
  commandName _ = "Storage.getTrustTokens"

-- | Removes all Trust Tokens issued by the provided issuerOrigin.
--   Leaves other stored data, including the issuer's Redemption Records, intact.

-- | Parameters of the 'Storage.clearTrustTokens' command.
data PStorageClearTrustTokens = PStorageClearTrustTokens
  {
    pStorageClearTrustTokensIssuerOrigin :: T.Text
  }
  deriving (Eq, Show)
pStorageClearTrustTokens
  :: T.Text
  -> PStorageClearTrustTokens
pStorageClearTrustTokens
  arg_pStorageClearTrustTokensIssuerOrigin
  = PStorageClearTrustTokens
    arg_pStorageClearTrustTokensIssuerOrigin
instance ToJSON PStorageClearTrustTokens where
  toJSON p = A.object $ catMaybes [
    ("issuerOrigin" A..=) <$> Just (pStorageClearTrustTokensIssuerOrigin p)
    ]
data StorageClearTrustTokens = StorageClearTrustTokens
  {
    -- | True if any tokens were deleted, false otherwise.
    storageClearTrustTokensDidDeleteTokens :: Bool
  }
  deriving (Eq, Show)
instance FromJSON StorageClearTrustTokens where
  parseJSON = A.withObject "StorageClearTrustTokens" $ \o -> StorageClearTrustTokens
    <$> o A..: "didDeleteTokens"
instance Command PStorageClearTrustTokens where
  type CommandResponse PStorageClearTrustTokens = StorageClearTrustTokens
  commandName _ = "Storage.clearTrustTokens"

-- | Gets metadata for an origin's shared storage.

-- | Parameters of the 'Storage.getSharedStorageMetadata' command.
data PStorageGetSharedStorageMetadata = PStorageGetSharedStorageMetadata
  {
    pStorageGetSharedStorageMetadataOwnerOrigin :: T.Text
  }
  deriving (Eq, Show)
pStorageGetSharedStorageMetadata
  :: T.Text
  -> PStorageGetSharedStorageMetadata
pStorageGetSharedStorageMetadata
  arg_pStorageGetSharedStorageMetadataOwnerOrigin
  = PStorageGetSharedStorageMetadata
    arg_pStorageGetSharedStorageMetadataOwnerOrigin
instance ToJSON PStorageGetSharedStorageMetadata where
  toJSON p = A.object $ catMaybes [
    ("ownerOrigin" A..=) <$> Just (pStorageGetSharedStorageMetadataOwnerOrigin p)
    ]
data StorageGetSharedStorageMetadata = StorageGetSharedStorageMetadata
  {
    storageGetSharedStorageMetadataMetadata :: StorageSharedStorageMetadata
  }
  deriving (Eq, Show)
instance FromJSON StorageGetSharedStorageMetadata where
  parseJSON = A.withObject "StorageGetSharedStorageMetadata" $ \o -> StorageGetSharedStorageMetadata
    <$> o A..: "metadata"
instance Command PStorageGetSharedStorageMetadata where
  type CommandResponse PStorageGetSharedStorageMetadata = StorageGetSharedStorageMetadata
  commandName _ = "Storage.getSharedStorageMetadata"

-- | Gets the entries in an given origin's shared storage.

-- | Parameters of the 'Storage.getSharedStorageEntries' command.
data PStorageGetSharedStorageEntries = PStorageGetSharedStorageEntries
  {
    pStorageGetSharedStorageEntriesOwnerOrigin :: T.Text
  }
  deriving (Eq, Show)
pStorageGetSharedStorageEntries
  :: T.Text
  -> PStorageGetSharedStorageEntries
pStorageGetSharedStorageEntries
  arg_pStorageGetSharedStorageEntriesOwnerOrigin
  = PStorageGetSharedStorageEntries
    arg_pStorageGetSharedStorageEntriesOwnerOrigin
instance ToJSON PStorageGetSharedStorageEntries where
  toJSON p = A.object $ catMaybes [
    ("ownerOrigin" A..=) <$> Just (pStorageGetSharedStorageEntriesOwnerOrigin p)
    ]
data StorageGetSharedStorageEntries = StorageGetSharedStorageEntries
  {
    storageGetSharedStorageEntriesEntries :: [StorageSharedStorageEntry]
  }
  deriving (Eq, Show)
instance FromJSON StorageGetSharedStorageEntries where
  parseJSON = A.withObject "StorageGetSharedStorageEntries" $ \o -> StorageGetSharedStorageEntries
    <$> o A..: "entries"
instance Command PStorageGetSharedStorageEntries where
  type CommandResponse PStorageGetSharedStorageEntries = StorageGetSharedStorageEntries
  commandName _ = "Storage.getSharedStorageEntries"

-- | Sets entry with `key` and `value` for a given origin's shared storage.

-- | Parameters of the 'Storage.setSharedStorageEntry' command.
data PStorageSetSharedStorageEntry = PStorageSetSharedStorageEntry
  {
    pStorageSetSharedStorageEntryOwnerOrigin :: T.Text,
    pStorageSetSharedStorageEntryKey :: T.Text,
    pStorageSetSharedStorageEntryValue :: T.Text,
    -- | If `ignoreIfPresent` is included and true, then only sets the entry if
    --   `key` doesn't already exist.
    pStorageSetSharedStorageEntryIgnoreIfPresent :: Maybe Bool
  }
  deriving (Eq, Show)
pStorageSetSharedStorageEntry
  :: T.Text
  -> T.Text
  -> T.Text
  -> PStorageSetSharedStorageEntry
pStorageSetSharedStorageEntry
  arg_pStorageSetSharedStorageEntryOwnerOrigin
  arg_pStorageSetSharedStorageEntryKey
  arg_pStorageSetSharedStorageEntryValue
  = PStorageSetSharedStorageEntry
    arg_pStorageSetSharedStorageEntryOwnerOrigin
    arg_pStorageSetSharedStorageEntryKey
    arg_pStorageSetSharedStorageEntryValue
    Nothing
instance ToJSON PStorageSetSharedStorageEntry where
  toJSON p = A.object $ catMaybes [
    ("ownerOrigin" A..=) <$> Just (pStorageSetSharedStorageEntryOwnerOrigin p),
    ("key" A..=) <$> Just (pStorageSetSharedStorageEntryKey p),
    ("value" A..=) <$> Just (pStorageSetSharedStorageEntryValue p),
    ("ignoreIfPresent" A..=) <$> (pStorageSetSharedStorageEntryIgnoreIfPresent p)
    ]
instance Command PStorageSetSharedStorageEntry where
  type CommandResponse PStorageSetSharedStorageEntry = ()
  commandName _ = "Storage.setSharedStorageEntry"
  fromJSON = const . A.Success . const ()

-- | Deletes entry for `key` (if it exists) for a given origin's shared storage.

-- | Parameters of the 'Storage.deleteSharedStorageEntry' command.
data PStorageDeleteSharedStorageEntry = PStorageDeleteSharedStorageEntry
  {
    pStorageDeleteSharedStorageEntryOwnerOrigin :: T.Text,
    pStorageDeleteSharedStorageEntryKey :: T.Text
  }
  deriving (Eq, Show)
pStorageDeleteSharedStorageEntry
  :: T.Text
  -> T.Text
  -> PStorageDeleteSharedStorageEntry
pStorageDeleteSharedStorageEntry
  arg_pStorageDeleteSharedStorageEntryOwnerOrigin
  arg_pStorageDeleteSharedStorageEntryKey
  = PStorageDeleteSharedStorageEntry
    arg_pStorageDeleteSharedStorageEntryOwnerOrigin
    arg_pStorageDeleteSharedStorageEntryKey
instance ToJSON PStorageDeleteSharedStorageEntry where
  toJSON p = A.object $ catMaybes [
    ("ownerOrigin" A..=) <$> Just (pStorageDeleteSharedStorageEntryOwnerOrigin p),
    ("key" A..=) <$> Just (pStorageDeleteSharedStorageEntryKey p)
    ]
instance Command PStorageDeleteSharedStorageEntry where
  type CommandResponse PStorageDeleteSharedStorageEntry = ()
  commandName _ = "Storage.deleteSharedStorageEntry"
  fromJSON = const . A.Success . const ()

-- | Clears all entries for a given origin's shared storage.

-- | Parameters of the 'Storage.clearSharedStorageEntries' command.
data PStorageClearSharedStorageEntries = PStorageClearSharedStorageEntries
  {
    pStorageClearSharedStorageEntriesOwnerOrigin :: T.Text
  }
  deriving (Eq, Show)
pStorageClearSharedStorageEntries
  :: T.Text
  -> PStorageClearSharedStorageEntries
pStorageClearSharedStorageEntries
  arg_pStorageClearSharedStorageEntriesOwnerOrigin
  = PStorageClearSharedStorageEntries
    arg_pStorageClearSharedStorageEntriesOwnerOrigin
instance ToJSON PStorageClearSharedStorageEntries where
  toJSON p = A.object $ catMaybes [
    ("ownerOrigin" A..=) <$> Just (pStorageClearSharedStorageEntriesOwnerOrigin p)
    ]
instance Command PStorageClearSharedStorageEntries where
  type CommandResponse PStorageClearSharedStorageEntries = ()
  commandName _ = "Storage.clearSharedStorageEntries"
  fromJSON = const . A.Success . const ()

-- | Resets the budget for `ownerOrigin` by clearing all budget withdrawals.

-- | Parameters of the 'Storage.resetSharedStorageBudget' command.
data PStorageResetSharedStorageBudget = PStorageResetSharedStorageBudget
  {
    pStorageResetSharedStorageBudgetOwnerOrigin :: T.Text
  }
  deriving (Eq, Show)
pStorageResetSharedStorageBudget
  :: T.Text
  -> PStorageResetSharedStorageBudget
pStorageResetSharedStorageBudget
  arg_pStorageResetSharedStorageBudgetOwnerOrigin
  = PStorageResetSharedStorageBudget
    arg_pStorageResetSharedStorageBudgetOwnerOrigin
instance ToJSON PStorageResetSharedStorageBudget where
  toJSON p = A.object $ catMaybes [
    ("ownerOrigin" A..=) <$> Just (pStorageResetSharedStorageBudgetOwnerOrigin p)
    ]
instance Command PStorageResetSharedStorageBudget where
  type CommandResponse PStorageResetSharedStorageBudget = ()
  commandName _ = "Storage.resetSharedStorageBudget"
  fromJSON = const . A.Success . const ()

-- | Enables/disables issuing of sharedStorageAccessed events.

-- | Parameters of the 'Storage.setSharedStorageTracking' command.
data PStorageSetSharedStorageTracking = PStorageSetSharedStorageTracking
  {
    pStorageSetSharedStorageTrackingEnable :: Bool
  }
  deriving (Eq, Show)
pStorageSetSharedStorageTracking
  :: Bool
  -> PStorageSetSharedStorageTracking
pStorageSetSharedStorageTracking
  arg_pStorageSetSharedStorageTrackingEnable
  = PStorageSetSharedStorageTracking
    arg_pStorageSetSharedStorageTrackingEnable
instance ToJSON PStorageSetSharedStorageTracking where
  toJSON p = A.object $ catMaybes [
    ("enable" A..=) <$> Just (pStorageSetSharedStorageTrackingEnable p)
    ]
instance Command PStorageSetSharedStorageTracking where
  type CommandResponse PStorageSetSharedStorageTracking = ()
  commandName _ = "Storage.setSharedStorageTracking"
  fromJSON = const . A.Success . const ()

-- | Set tracking for a storage key's buckets.

-- | Parameters of the 'Storage.setStorageBucketTracking' command.
data PStorageSetStorageBucketTracking = PStorageSetStorageBucketTracking
  {
    pStorageSetStorageBucketTrackingStorageKey :: T.Text,
    pStorageSetStorageBucketTrackingEnable :: Bool
  }
  deriving (Eq, Show)
pStorageSetStorageBucketTracking
  :: T.Text
  -> Bool
  -> PStorageSetStorageBucketTracking
pStorageSetStorageBucketTracking
  arg_pStorageSetStorageBucketTrackingStorageKey
  arg_pStorageSetStorageBucketTrackingEnable
  = PStorageSetStorageBucketTracking
    arg_pStorageSetStorageBucketTrackingStorageKey
    arg_pStorageSetStorageBucketTrackingEnable
instance ToJSON PStorageSetStorageBucketTracking where
  toJSON p = A.object $ catMaybes [
    ("storageKey" A..=) <$> Just (pStorageSetStorageBucketTrackingStorageKey p),
    ("enable" A..=) <$> Just (pStorageSetStorageBucketTrackingEnable p)
    ]
instance Command PStorageSetStorageBucketTracking where
  type CommandResponse PStorageSetStorageBucketTracking = ()
  commandName _ = "Storage.setStorageBucketTracking"
  fromJSON = const . A.Success . const ()

-- | Deletes the Storage Bucket with the given storage key and bucket name.

-- | Parameters of the 'Storage.deleteStorageBucket' command.
data PStorageDeleteStorageBucket = PStorageDeleteStorageBucket
  {
    pStorageDeleteStorageBucketBucket :: StorageStorageBucket
  }
  deriving (Eq, Show)
pStorageDeleteStorageBucket
  :: StorageStorageBucket
  -> PStorageDeleteStorageBucket
pStorageDeleteStorageBucket
  arg_pStorageDeleteStorageBucketBucket
  = PStorageDeleteStorageBucket
    arg_pStorageDeleteStorageBucketBucket
instance ToJSON PStorageDeleteStorageBucket where
  toJSON p = A.object $ catMaybes [
    ("bucket" A..=) <$> Just (pStorageDeleteStorageBucketBucket p)
    ]
instance Command PStorageDeleteStorageBucket where
  type CommandResponse PStorageDeleteStorageBucket = ()
  commandName _ = "Storage.deleteStorageBucket"
  fromJSON = const . A.Success . const ()

-- | Deletes state for sites identified as potential bounce trackers, immediately.

-- | Parameters of the 'Storage.runBounceTrackingMitigations' command.
data PStorageRunBounceTrackingMitigations = PStorageRunBounceTrackingMitigations
  deriving (Eq, Show)
pStorageRunBounceTrackingMitigations
  :: PStorageRunBounceTrackingMitigations
pStorageRunBounceTrackingMitigations
  = PStorageRunBounceTrackingMitigations
instance ToJSON PStorageRunBounceTrackingMitigations where
  toJSON _ = A.Null
data StorageRunBounceTrackingMitigations = StorageRunBounceTrackingMitigations
  {
    storageRunBounceTrackingMitigationsDeletedSites :: [T.Text]
  }
  deriving (Eq, Show)
instance FromJSON StorageRunBounceTrackingMitigations where
  parseJSON = A.withObject "StorageRunBounceTrackingMitigations" $ \o -> StorageRunBounceTrackingMitigations
    <$> o A..: "deletedSites"
instance Command PStorageRunBounceTrackingMitigations where
  type CommandResponse PStorageRunBounceTrackingMitigations = StorageRunBounceTrackingMitigations
  commandName _ = "Storage.runBounceTrackingMitigations"

-- | Returns the effective Related Website Sets in use by this profile for the browser
--   session. The effective Related Website Sets will not change during a browser session.

-- | Parameters of the 'Storage.getRelatedWebsiteSets' command.
data PStorageGetRelatedWebsiteSets = PStorageGetRelatedWebsiteSets
  deriving (Eq, Show)
pStorageGetRelatedWebsiteSets
  :: PStorageGetRelatedWebsiteSets
pStorageGetRelatedWebsiteSets
  = PStorageGetRelatedWebsiteSets
instance ToJSON PStorageGetRelatedWebsiteSets where
  toJSON _ = A.Null
data StorageGetRelatedWebsiteSets = StorageGetRelatedWebsiteSets
  {
    storageGetRelatedWebsiteSetsSets :: [StorageRelatedWebsiteSet]
  }
  deriving (Eq, Show)
instance FromJSON StorageGetRelatedWebsiteSets where
  parseJSON = A.withObject "StorageGetRelatedWebsiteSets" $ \o -> StorageGetRelatedWebsiteSets
    <$> o A..: "sets"
instance Command PStorageGetRelatedWebsiteSets where
  type CommandResponse PStorageGetRelatedWebsiteSets = StorageGetRelatedWebsiteSets
  commandName _ = "Storage.getRelatedWebsiteSets"

