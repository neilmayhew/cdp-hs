{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}


{- |
= Extensions

Defines commands and events for browser extensions.
-}


module CDP.Domains.Extensions (module CDP.Domains.Extensions) where

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




-- | Type 'Extensions.StorageArea'.
--   Storage areas.
data ExtensionsStorageArea = ExtensionsStorageAreaSession | ExtensionsStorageAreaLocal | ExtensionsStorageAreaSync | ExtensionsStorageAreaManaged
  deriving (Ord, Eq, Show, Read)
instance FromJSON ExtensionsStorageArea where
  parseJSON = A.withText "ExtensionsStorageArea" $ \v -> case v of
    "session" -> pure ExtensionsStorageAreaSession
    "local" -> pure ExtensionsStorageAreaLocal
    "sync" -> pure ExtensionsStorageAreaSync
    "managed" -> pure ExtensionsStorageAreaManaged
    "_" -> fail "failed to parse ExtensionsStorageArea"
instance ToJSON ExtensionsStorageArea where
  toJSON v = A.String $ case v of
    ExtensionsStorageAreaSession -> "session"
    ExtensionsStorageAreaLocal -> "local"
    ExtensionsStorageAreaSync -> "sync"
    ExtensionsStorageAreaManaged -> "managed"

-- | Type 'Extensions.ExtensionInfo'.
--   Detailed information about an extension.
data ExtensionsExtensionInfo = ExtensionsExtensionInfo
  {
    -- | Extension id.
    extensionsExtensionInfoId :: T.Text,
    -- | Extension name.
    extensionsExtensionInfoName :: T.Text,
    -- | Extension version.
    extensionsExtensionInfoVersion :: T.Text,
    -- | The path from which the extension was loaded.
    extensionsExtensionInfoPath :: T.Text,
    -- | Extension enabled status.
    extensionsExtensionInfoEnabled :: Bool
  }
  deriving (Eq, Show)
instance FromJSON ExtensionsExtensionInfo where
  parseJSON = A.withObject "ExtensionsExtensionInfo" $ \o -> ExtensionsExtensionInfo
    <$> o A..: "id"
    <*> o A..: "name"
    <*> o A..: "version"
    <*> o A..: "path"
    <*> o A..: "enabled"
instance ToJSON ExtensionsExtensionInfo where
  toJSON p = A.object $ catMaybes [
    ("id" A..=) <$> Just (extensionsExtensionInfoId p),
    ("name" A..=) <$> Just (extensionsExtensionInfoName p),
    ("version" A..=) <$> Just (extensionsExtensionInfoVersion p),
    ("path" A..=) <$> Just (extensionsExtensionInfoPath p),
    ("enabled" A..=) <$> Just (extensionsExtensionInfoEnabled p)
    ]

-- | Runs an extension default action.

-- | Parameters of the 'Extensions.triggerAction' command.
data PExtensionsTriggerAction = PExtensionsTriggerAction
  {
    -- | Extension id.
    pExtensionsTriggerActionId :: T.Text,
    -- | A tab target ID to trigger the default extension action on.
    pExtensionsTriggerActionTargetId :: T.Text
  }
  deriving (Eq, Show)
pExtensionsTriggerAction
  {-
  -- | Extension id.
  -}
  :: T.Text
  {-
  -- | A tab target ID to trigger the default extension action on.
  -}
  -> T.Text
  -> PExtensionsTriggerAction
pExtensionsTriggerAction
  arg_pExtensionsTriggerActionId
  arg_pExtensionsTriggerActionTargetId
  = PExtensionsTriggerAction
    arg_pExtensionsTriggerActionId
    arg_pExtensionsTriggerActionTargetId
instance ToJSON PExtensionsTriggerAction where
  toJSON p = A.object $ catMaybes [
    ("id" A..=) <$> Just (pExtensionsTriggerActionId p),
    ("targetId" A..=) <$> Just (pExtensionsTriggerActionTargetId p)
    ]
instance Command PExtensionsTriggerAction where
  type CommandResponse PExtensionsTriggerAction = ()
  commandName _ = "Extensions.triggerAction"
  fromJSON = const . A.Success . const ()

-- | Installs an unpacked extension from the filesystem similar to
--   --load-extension CLI flags. Returns extension ID once the extension
--   has been installed.

-- | Parameters of the 'Extensions.loadUnpacked' command.
data PExtensionsLoadUnpacked = PExtensionsLoadUnpacked
  {
    -- | Absolute file path.
    pExtensionsLoadUnpackedPath :: T.Text,
    -- | Enable the extension in incognito
    pExtensionsLoadUnpackedEnableInIncognito :: Maybe Bool
  }
  deriving (Eq, Show)
pExtensionsLoadUnpacked
  {-
  -- | Absolute file path.
  -}
  :: T.Text
  -> PExtensionsLoadUnpacked
pExtensionsLoadUnpacked
  arg_pExtensionsLoadUnpackedPath
  = PExtensionsLoadUnpacked
    arg_pExtensionsLoadUnpackedPath
    Nothing
instance ToJSON PExtensionsLoadUnpacked where
  toJSON p = A.object $ catMaybes [
    ("path" A..=) <$> Just (pExtensionsLoadUnpackedPath p),
    ("enableInIncognito" A..=) <$> (pExtensionsLoadUnpackedEnableInIncognito p)
    ]
data ExtensionsLoadUnpacked = ExtensionsLoadUnpacked
  {
    -- | Extension id.
    extensionsLoadUnpackedId :: T.Text
  }
  deriving (Eq, Show)
instance FromJSON ExtensionsLoadUnpacked where
  parseJSON = A.withObject "ExtensionsLoadUnpacked" $ \o -> ExtensionsLoadUnpacked
    <$> o A..: "id"
instance Command PExtensionsLoadUnpacked where
  type CommandResponse PExtensionsLoadUnpacked = ExtensionsLoadUnpacked
  commandName _ = "Extensions.loadUnpacked"

-- | Gets a list of all unpacked extensions.

-- | Parameters of the 'Extensions.getExtensions' command.
data PExtensionsGetExtensions = PExtensionsGetExtensions
  deriving (Eq, Show)
pExtensionsGetExtensions
  :: PExtensionsGetExtensions
pExtensionsGetExtensions
  = PExtensionsGetExtensions
instance ToJSON PExtensionsGetExtensions where
  toJSON _ = A.Null
data ExtensionsGetExtensions = ExtensionsGetExtensions
  {
    extensionsGetExtensionsExtensions :: [ExtensionsExtensionInfo]
  }
  deriving (Eq, Show)
instance FromJSON ExtensionsGetExtensions where
  parseJSON = A.withObject "ExtensionsGetExtensions" $ \o -> ExtensionsGetExtensions
    <$> o A..: "extensions"
instance Command PExtensionsGetExtensions where
  type CommandResponse PExtensionsGetExtensions = ExtensionsGetExtensions
  commandName _ = "Extensions.getExtensions"

-- | Uninstalls an unpacked extension (others not supported) from the profile.

-- | Parameters of the 'Extensions.uninstall' command.
data PExtensionsUninstall = PExtensionsUninstall
  {
    -- | Extension id.
    pExtensionsUninstallId :: T.Text
  }
  deriving (Eq, Show)
pExtensionsUninstall
  {-
  -- | Extension id.
  -}
  :: T.Text
  -> PExtensionsUninstall
pExtensionsUninstall
  arg_pExtensionsUninstallId
  = PExtensionsUninstall
    arg_pExtensionsUninstallId
instance ToJSON PExtensionsUninstall where
  toJSON p = A.object $ catMaybes [
    ("id" A..=) <$> Just (pExtensionsUninstallId p)
    ]
instance Command PExtensionsUninstall where
  type CommandResponse PExtensionsUninstall = ()
  commandName _ = "Extensions.uninstall"
  fromJSON = const . A.Success . const ()

-- | Gets data from extension storage in the given `storageArea`. If `keys` is
--   specified, these are used to filter the result.

-- | Parameters of the 'Extensions.getStorageItems' command.
data PExtensionsGetStorageItems = PExtensionsGetStorageItems
  {
    -- | ID of extension.
    pExtensionsGetStorageItemsId :: T.Text,
    -- | StorageArea to retrieve data from.
    pExtensionsGetStorageItemsStorageArea :: ExtensionsStorageArea,
    -- | Keys to retrieve.
    pExtensionsGetStorageItemsKeys :: Maybe [T.Text]
  }
  deriving (Eq, Show)
pExtensionsGetStorageItems
  {-
  -- | ID of extension.
  -}
  :: T.Text
  {-
  -- | StorageArea to retrieve data from.
  -}
  -> ExtensionsStorageArea
  -> PExtensionsGetStorageItems
pExtensionsGetStorageItems
  arg_pExtensionsGetStorageItemsId
  arg_pExtensionsGetStorageItemsStorageArea
  = PExtensionsGetStorageItems
    arg_pExtensionsGetStorageItemsId
    arg_pExtensionsGetStorageItemsStorageArea
    Nothing
instance ToJSON PExtensionsGetStorageItems where
  toJSON p = A.object $ catMaybes [
    ("id" A..=) <$> Just (pExtensionsGetStorageItemsId p),
    ("storageArea" A..=) <$> Just (pExtensionsGetStorageItemsStorageArea p),
    ("keys" A..=) <$> (pExtensionsGetStorageItemsKeys p)
    ]
data ExtensionsGetStorageItems = ExtensionsGetStorageItems
  {
    extensionsGetStorageItemsData :: [(T.Text, T.Text)]
  }
  deriving (Eq, Show)
instance FromJSON ExtensionsGetStorageItems where
  parseJSON = A.withObject "ExtensionsGetStorageItems" $ \o -> ExtensionsGetStorageItems
    <$> o A..: "data"
instance Command PExtensionsGetStorageItems where
  type CommandResponse PExtensionsGetStorageItems = ExtensionsGetStorageItems
  commandName _ = "Extensions.getStorageItems"

-- | Removes `keys` from extension storage in the given `storageArea`.

-- | Parameters of the 'Extensions.removeStorageItems' command.
data PExtensionsRemoveStorageItems = PExtensionsRemoveStorageItems
  {
    -- | ID of extension.
    pExtensionsRemoveStorageItemsId :: T.Text,
    -- | StorageArea to remove data from.
    pExtensionsRemoveStorageItemsStorageArea :: ExtensionsStorageArea,
    -- | Keys to remove.
    pExtensionsRemoveStorageItemsKeys :: [T.Text]
  }
  deriving (Eq, Show)
pExtensionsRemoveStorageItems
  {-
  -- | ID of extension.
  -}
  :: T.Text
  {-
  -- | StorageArea to remove data from.
  -}
  -> ExtensionsStorageArea
  {-
  -- | Keys to remove.
  -}
  -> [T.Text]
  -> PExtensionsRemoveStorageItems
pExtensionsRemoveStorageItems
  arg_pExtensionsRemoveStorageItemsId
  arg_pExtensionsRemoveStorageItemsStorageArea
  arg_pExtensionsRemoveStorageItemsKeys
  = PExtensionsRemoveStorageItems
    arg_pExtensionsRemoveStorageItemsId
    arg_pExtensionsRemoveStorageItemsStorageArea
    arg_pExtensionsRemoveStorageItemsKeys
instance ToJSON PExtensionsRemoveStorageItems where
  toJSON p = A.object $ catMaybes [
    ("id" A..=) <$> Just (pExtensionsRemoveStorageItemsId p),
    ("storageArea" A..=) <$> Just (pExtensionsRemoveStorageItemsStorageArea p),
    ("keys" A..=) <$> Just (pExtensionsRemoveStorageItemsKeys p)
    ]
instance Command PExtensionsRemoveStorageItems where
  type CommandResponse PExtensionsRemoveStorageItems = ()
  commandName _ = "Extensions.removeStorageItems"
  fromJSON = const . A.Success . const ()

-- | Clears extension storage in the given `storageArea`.

-- | Parameters of the 'Extensions.clearStorageItems' command.
data PExtensionsClearStorageItems = PExtensionsClearStorageItems
  {
    -- | ID of extension.
    pExtensionsClearStorageItemsId :: T.Text,
    -- | StorageArea to remove data from.
    pExtensionsClearStorageItemsStorageArea :: ExtensionsStorageArea
  }
  deriving (Eq, Show)
pExtensionsClearStorageItems
  {-
  -- | ID of extension.
  -}
  :: T.Text
  {-
  -- | StorageArea to remove data from.
  -}
  -> ExtensionsStorageArea
  -> PExtensionsClearStorageItems
pExtensionsClearStorageItems
  arg_pExtensionsClearStorageItemsId
  arg_pExtensionsClearStorageItemsStorageArea
  = PExtensionsClearStorageItems
    arg_pExtensionsClearStorageItemsId
    arg_pExtensionsClearStorageItemsStorageArea
instance ToJSON PExtensionsClearStorageItems where
  toJSON p = A.object $ catMaybes [
    ("id" A..=) <$> Just (pExtensionsClearStorageItemsId p),
    ("storageArea" A..=) <$> Just (pExtensionsClearStorageItemsStorageArea p)
    ]
instance Command PExtensionsClearStorageItems where
  type CommandResponse PExtensionsClearStorageItems = ()
  commandName _ = "Extensions.clearStorageItems"
  fromJSON = const . A.Success . const ()

-- | Sets `values` in extension storage in the given `storageArea`. The provided `values`
--   will be merged with existing values in the storage area.

-- | Parameters of the 'Extensions.setStorageItems' command.
data PExtensionsSetStorageItems = PExtensionsSetStorageItems
  {
    -- | ID of extension.
    pExtensionsSetStorageItemsId :: T.Text,
    -- | StorageArea to set data in.
    pExtensionsSetStorageItemsStorageArea :: ExtensionsStorageArea,
    -- | Values to set.
    pExtensionsSetStorageItemsValues :: [(T.Text, T.Text)]
  }
  deriving (Eq, Show)
pExtensionsSetStorageItems
  {-
  -- | ID of extension.
  -}
  :: T.Text
  {-
  -- | StorageArea to set data in.
  -}
  -> ExtensionsStorageArea
  {-
  -- | Values to set.
  -}
  -> [(T.Text, T.Text)]
  -> PExtensionsSetStorageItems
pExtensionsSetStorageItems
  arg_pExtensionsSetStorageItemsId
  arg_pExtensionsSetStorageItemsStorageArea
  arg_pExtensionsSetStorageItemsValues
  = PExtensionsSetStorageItems
    arg_pExtensionsSetStorageItemsId
    arg_pExtensionsSetStorageItemsStorageArea
    arg_pExtensionsSetStorageItemsValues
instance ToJSON PExtensionsSetStorageItems where
  toJSON p = A.object $ catMaybes [
    ("id" A..=) <$> Just (pExtensionsSetStorageItemsId p),
    ("storageArea" A..=) <$> Just (pExtensionsSetStorageItemsStorageArea p),
    ("values" A..=) <$> Just (pExtensionsSetStorageItemsValues p)
    ]
instance Command PExtensionsSetStorageItems where
  type CommandResponse PExtensionsSetStorageItems = ()
  commandName _ = "Extensions.setStorageItems"
  fromJSON = const . A.Success . const ()

