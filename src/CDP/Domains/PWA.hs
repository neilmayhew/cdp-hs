{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}


{- |
= PWA

This domain allows interacting with the browser to control PWAs.
-}


module CDP.Domains.PWA (module CDP.Domains.PWA) where

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


-- | Type 'PWA.FileHandlerAccept'.
--   The following types are the replica of
--   https://crsrc.org/c/chrome/browser/web_applications/proto/web_app_os_integration_state.proto;drc=9910d3be894c8f142c977ba1023f30a656bc13fc;l=67
data PWAFileHandlerAccept = PWAFileHandlerAccept
  {
    -- | New name of the mimetype according to
    --   https://www.iana.org/assignments/media-types/media-types.xhtml
    pWAFileHandlerAcceptMediaType :: T.Text,
    pWAFileHandlerAcceptFileExtensions :: [T.Text]
  }
  deriving (Eq, Show)
instance FromJSON PWAFileHandlerAccept where
  parseJSON = A.withObject "PWAFileHandlerAccept" $ \o -> PWAFileHandlerAccept
    <$> o A..: "mediaType"
    <*> o A..: "fileExtensions"
instance ToJSON PWAFileHandlerAccept where
  toJSON p = A.object $ catMaybes [
    ("mediaType" A..=) <$> Just (pWAFileHandlerAcceptMediaType p),
    ("fileExtensions" A..=) <$> Just (pWAFileHandlerAcceptFileExtensions p)
    ]

-- | Type 'PWA.FileHandler'.
data PWAFileHandler = PWAFileHandler
  {
    pWAFileHandlerAction :: T.Text,
    pWAFileHandlerAccepts :: [PWAFileHandlerAccept],
    pWAFileHandlerDisplayName :: T.Text
  }
  deriving (Eq, Show)
instance FromJSON PWAFileHandler where
  parseJSON = A.withObject "PWAFileHandler" $ \o -> PWAFileHandler
    <$> o A..: "action"
    <*> o A..: "accepts"
    <*> o A..: "displayName"
instance ToJSON PWAFileHandler where
  toJSON p = A.object $ catMaybes [
    ("action" A..=) <$> Just (pWAFileHandlerAction p),
    ("accepts" A..=) <$> Just (pWAFileHandlerAccepts p),
    ("displayName" A..=) <$> Just (pWAFileHandlerDisplayName p)
    ]

-- | Type 'PWA.DisplayMode'.
--   If user prefers opening the app in browser or an app window.
data PWADisplayMode = PWADisplayModeStandalone | PWADisplayModeBrowser
  deriving (Ord, Eq, Show, Read)
instance FromJSON PWADisplayMode where
  parseJSON = A.withText "PWADisplayMode" $ \v -> case v of
    "standalone" -> pure PWADisplayModeStandalone
    "browser" -> pure PWADisplayModeBrowser
    "_" -> fail "failed to parse PWADisplayMode"
instance ToJSON PWADisplayMode where
  toJSON v = A.String $ case v of
    PWADisplayModeStandalone -> "standalone"
    PWADisplayModeBrowser -> "browser"

-- | Returns the following OS state for the given manifest id.

-- | Parameters of the 'PWA.getOsAppState' command.
data PPWAGetOsAppState = PPWAGetOsAppState
  {
    -- | The id from the webapp's manifest file, commonly it's the url of the
    --   site installing the webapp. See
    --   https://web.dev/learn/pwa/web-app-manifest.
    pPWAGetOsAppStateManifestId :: T.Text
  }
  deriving (Eq, Show)
pPWAGetOsAppState
  {-
  -- | The id from the webapp's manifest file, commonly it's the url of the
  --   site installing the webapp. See
  --   https://web.dev/learn/pwa/web-app-manifest.
  -}
  :: T.Text
  -> PPWAGetOsAppState
pPWAGetOsAppState
  arg_pPWAGetOsAppStateManifestId
  = PPWAGetOsAppState
    arg_pPWAGetOsAppStateManifestId
instance ToJSON PPWAGetOsAppState where
  toJSON p = A.object $ catMaybes [
    ("manifestId" A..=) <$> Just (pPWAGetOsAppStateManifestId p)
    ]
data PWAGetOsAppState = PWAGetOsAppState
  {
    pWAGetOsAppStateBadgeCount :: Int,
    pWAGetOsAppStateFileHandlers :: [PWAFileHandler]
  }
  deriving (Eq, Show)
instance FromJSON PWAGetOsAppState where
  parseJSON = A.withObject "PWAGetOsAppState" $ \o -> PWAGetOsAppState
    <$> o A..: "badgeCount"
    <*> o A..: "fileHandlers"
instance Command PPWAGetOsAppState where
  type CommandResponse PPWAGetOsAppState = PWAGetOsAppState
  commandName _ = "PWA.getOsAppState"

-- | Installs the given manifest identity, optionally using the given installUrlOrBundleUrl
--   
--   IWA-specific install description:
--   manifestId corresponds to isolated-app:// + web_package::SignedWebBundleId
--   
--   File installation mode:
--   The installUrlOrBundleUrl can be either file:// or http(s):// pointing
--   to a signed web bundle (.swbn). In this case SignedWebBundleId must correspond to
--   The .swbn file's signing key.
--   
--   Dev proxy installation mode:
--   installUrlOrBundleUrl must be http(s):// that serves dev mode IWA.
--   web_package::SignedWebBundleId must be of type dev proxy.
--   
--   The advantage of dev proxy mode is that all changes to IWA
--   automatically will be reflected in the running app without
--   reinstallation.
--   
--   To generate bundle id for proxy mode:
--   1. Generate 32 random bytes.
--   2. Add a specific suffix at the end following the documentation
--      https://github.com/WICG/isolated-web-apps/blob/main/Scheme.md#suffix
--   3. Encode the entire sequence using Base32 without padding.
--   
--   If Chrome is not in IWA dev
--   mode, the installation will fail, regardless of the state of the allowlist.

-- | Parameters of the 'PWA.install' command.
data PPWAInstall = PPWAInstall
  {
    pPWAInstallManifestId :: T.Text,
    -- | The location of the app or bundle overriding the one derived from the
    --   manifestId.
    pPWAInstallInstallUrlOrBundleUrl :: Maybe T.Text
  }
  deriving (Eq, Show)
pPWAInstall
  :: T.Text
  -> PPWAInstall
pPWAInstall
  arg_pPWAInstallManifestId
  = PPWAInstall
    arg_pPWAInstallManifestId
    Nothing
instance ToJSON PPWAInstall where
  toJSON p = A.object $ catMaybes [
    ("manifestId" A..=) <$> Just (pPWAInstallManifestId p),
    ("installUrlOrBundleUrl" A..=) <$> (pPWAInstallInstallUrlOrBundleUrl p)
    ]
instance Command PPWAInstall where
  type CommandResponse PPWAInstall = ()
  commandName _ = "PWA.install"
  fromJSON = const . A.Success . const ()

-- | Uninstalls the given manifest_id and closes any opened app windows.

-- | Parameters of the 'PWA.uninstall' command.
data PPWAUninstall = PPWAUninstall
  {
    pPWAUninstallManifestId :: T.Text
  }
  deriving (Eq, Show)
pPWAUninstall
  :: T.Text
  -> PPWAUninstall
pPWAUninstall
  arg_pPWAUninstallManifestId
  = PPWAUninstall
    arg_pPWAUninstallManifestId
instance ToJSON PPWAUninstall where
  toJSON p = A.object $ catMaybes [
    ("manifestId" A..=) <$> Just (pPWAUninstallManifestId p)
    ]
instance Command PPWAUninstall where
  type CommandResponse PPWAUninstall = ()
  commandName _ = "PWA.uninstall"
  fromJSON = const . A.Success . const ()

-- | Launches the installed web app, or an url in the same web app instead of the
--   default start url if it is provided. Returns a page Target.TargetID which
--   can be used to attach to via Target.attachToTarget or similar APIs.

-- | Parameters of the 'PWA.launch' command.
data PPWALaunch = PPWALaunch
  {
    pPWALaunchManifestId :: T.Text,
    pPWALaunchUrl :: Maybe T.Text
  }
  deriving (Eq, Show)
pPWALaunch
  :: T.Text
  -> PPWALaunch
pPWALaunch
  arg_pPWALaunchManifestId
  = PPWALaunch
    arg_pPWALaunchManifestId
    Nothing
instance ToJSON PPWALaunch where
  toJSON p = A.object $ catMaybes [
    ("manifestId" A..=) <$> Just (pPWALaunchManifestId p),
    ("url" A..=) <$> (pPWALaunchUrl p)
    ]
data PWALaunch = PWALaunch
  {
    -- | ID of the tab target created as a result.
    pWALaunchTargetId :: BrowserTarget.TargetTargetID
  }
  deriving (Eq, Show)
instance FromJSON PWALaunch where
  parseJSON = A.withObject "PWALaunch" $ \o -> PWALaunch
    <$> o A..: "targetId"
instance Command PPWALaunch where
  type CommandResponse PPWALaunch = PWALaunch
  commandName _ = "PWA.launch"

-- | Opens one or more local files from an installed web app identified by its
--   manifestId. The web app needs to have file handlers registered to process
--   the files. The API returns one or more page Target.TargetIDs which can be
--   used to attach to via Target.attachToTarget or similar APIs.
--   If some files in the parameters cannot be handled by the web app, they will
--   be ignored. If none of the files can be handled, this API returns an error.
--   If no files are provided as the parameter, this API also returns an error.
--   
--   According to the definition of the file handlers in the manifest file, one
--   Target.TargetID may represent a page handling one or more files. The order
--   of the returned Target.TargetIDs is not guaranteed.
--   
--   TODO(crbug.com/339454034): Check the existences of the input files.

-- | Parameters of the 'PWA.launchFilesInApp' command.
data PPWALaunchFilesInApp = PPWALaunchFilesInApp
  {
    pPWALaunchFilesInAppManifestId :: T.Text,
    pPWALaunchFilesInAppFiles :: [T.Text]
  }
  deriving (Eq, Show)
pPWALaunchFilesInApp
  :: T.Text
  -> [T.Text]
  -> PPWALaunchFilesInApp
pPWALaunchFilesInApp
  arg_pPWALaunchFilesInAppManifestId
  arg_pPWALaunchFilesInAppFiles
  = PPWALaunchFilesInApp
    arg_pPWALaunchFilesInAppManifestId
    arg_pPWALaunchFilesInAppFiles
instance ToJSON PPWALaunchFilesInApp where
  toJSON p = A.object $ catMaybes [
    ("manifestId" A..=) <$> Just (pPWALaunchFilesInAppManifestId p),
    ("files" A..=) <$> Just (pPWALaunchFilesInAppFiles p)
    ]
data PWALaunchFilesInApp = PWALaunchFilesInApp
  {
    -- | IDs of the tab targets created as the result.
    pWALaunchFilesInAppTargetIds :: [BrowserTarget.TargetTargetID]
  }
  deriving (Eq, Show)
instance FromJSON PWALaunchFilesInApp where
  parseJSON = A.withObject "PWALaunchFilesInApp" $ \o -> PWALaunchFilesInApp
    <$> o A..: "targetIds"
instance Command PPWALaunchFilesInApp where
  type CommandResponse PPWALaunchFilesInApp = PWALaunchFilesInApp
  commandName _ = "PWA.launchFilesInApp"

-- | Opens the current page in its web app identified by the manifest id, needs
--   to be called on a page target. This function returns immediately without
--   waiting for the app to finish loading.

-- | Parameters of the 'PWA.openCurrentPageInApp' command.
data PPWAOpenCurrentPageInApp = PPWAOpenCurrentPageInApp
  {
    pPWAOpenCurrentPageInAppManifestId :: T.Text
  }
  deriving (Eq, Show)
pPWAOpenCurrentPageInApp
  :: T.Text
  -> PPWAOpenCurrentPageInApp
pPWAOpenCurrentPageInApp
  arg_pPWAOpenCurrentPageInAppManifestId
  = PPWAOpenCurrentPageInApp
    arg_pPWAOpenCurrentPageInAppManifestId
instance ToJSON PPWAOpenCurrentPageInApp where
  toJSON p = A.object $ catMaybes [
    ("manifestId" A..=) <$> Just (pPWAOpenCurrentPageInAppManifestId p)
    ]
instance Command PPWAOpenCurrentPageInApp where
  type CommandResponse PPWAOpenCurrentPageInApp = ()
  commandName _ = "PWA.openCurrentPageInApp"
  fromJSON = const . A.Success . const ()

-- | Changes user settings of the web app identified by its manifestId. If the
--   app was not installed, this command returns an error. Unset parameters will
--   be ignored; unrecognized values will cause an error.
--   
--   Unlike the ones defined in the manifest files of the web apps, these
--   settings are provided by the browser and controlled by the users, they
--   impact the way the browser handling the web apps.
--   
--   See the comment of each parameter.

-- | Parameters of the 'PWA.changeAppUserSettings' command.
data PPWAChangeAppUserSettings = PPWAChangeAppUserSettings
  {
    pPWAChangeAppUserSettingsManifestId :: T.Text,
    -- | If user allows the links clicked on by the user in the app's scope, or
    --   extended scope if the manifest has scope extensions and the flags
    --   `DesktopPWAsLinkCapturingWithScopeExtensions` and
    --   `WebAppEnableScopeExtensions` are enabled.
    --   
    --   Note, the API does not support resetting the linkCapturing to the
    --   initial value, uninstalling and installing the web app again will reset
    --   it.
    --   
    --   TODO(crbug.com/339453269): Setting this value on ChromeOS is not
    --   supported yet.
    pPWAChangeAppUserSettingsLinkCapturing :: Maybe Bool,
    pPWAChangeAppUserSettingsDisplayMode :: Maybe PWADisplayMode
  }
  deriving (Eq, Show)
pPWAChangeAppUserSettings
  :: T.Text
  -> PPWAChangeAppUserSettings
pPWAChangeAppUserSettings
  arg_pPWAChangeAppUserSettingsManifestId
  = PPWAChangeAppUserSettings
    arg_pPWAChangeAppUserSettingsManifestId
    Nothing
    Nothing
instance ToJSON PPWAChangeAppUserSettings where
  toJSON p = A.object $ catMaybes [
    ("manifestId" A..=) <$> Just (pPWAChangeAppUserSettingsManifestId p),
    ("linkCapturing" A..=) <$> (pPWAChangeAppUserSettingsLinkCapturing p),
    ("displayMode" A..=) <$> (pPWAChangeAppUserSettingsDisplayMode p)
    ]
instance Command PPWAChangeAppUserSettings where
  type CommandResponse PPWAChangeAppUserSettings = ()
  commandName _ = "PWA.changeAppUserSettings"
  fromJSON = const . A.Success . const ()

