{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}


{- |
= Audits

Audits domain allows investigation of page violations and possible improvements.
-}


module CDP.Domains.Audits (module CDP.Domains.Audits) where

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
import CDP.Domains.Runtime as Runtime


-- | Type 'Audits.AffectedCookie'.
--   Information about a cookie that is affected by an inspector issue.
data AuditsAffectedCookie = AuditsAffectedCookie
  {
    -- | The following three properties uniquely identify a cookie
    auditsAffectedCookieName :: T.Text,
    auditsAffectedCookiePath :: T.Text,
    auditsAffectedCookieDomain :: T.Text
  }
  deriving (Eq, Show)
instance FromJSON AuditsAffectedCookie where
  parseJSON = A.withObject "AuditsAffectedCookie" $ \o -> AuditsAffectedCookie
    <$> o A..: "name"
    <*> o A..: "path"
    <*> o A..: "domain"
instance ToJSON AuditsAffectedCookie where
  toJSON p = A.object $ catMaybes [
    ("name" A..=) <$> Just (auditsAffectedCookieName p),
    ("path" A..=) <$> Just (auditsAffectedCookiePath p),
    ("domain" A..=) <$> Just (auditsAffectedCookieDomain p)
    ]

-- | Type 'Audits.AffectedRequest'.
--   Information about a request that is affected by an inspector issue.
data AuditsAffectedRequest = AuditsAffectedRequest
  {
    -- | The unique request id.
    auditsAffectedRequestRequestId :: Maybe DOMNetworkEmulationPageSecurity.NetworkRequestId,
    auditsAffectedRequestUrl :: T.Text
  }
  deriving (Eq, Show)
instance FromJSON AuditsAffectedRequest where
  parseJSON = A.withObject "AuditsAffectedRequest" $ \o -> AuditsAffectedRequest
    <$> o A..:? "requestId"
    <*> o A..: "url"
instance ToJSON AuditsAffectedRequest where
  toJSON p = A.object $ catMaybes [
    ("requestId" A..=) <$> (auditsAffectedRequestRequestId p),
    ("url" A..=) <$> Just (auditsAffectedRequestUrl p)
    ]

-- | Type 'Audits.AffectedFrame'.
--   Information about the frame affected by an inspector issue.
data AuditsAffectedFrame = AuditsAffectedFrame
  {
    auditsAffectedFrameFrameId :: DOMNetworkEmulationPageSecurity.PageFrameId
  }
  deriving (Eq, Show)
instance FromJSON AuditsAffectedFrame where
  parseJSON = A.withObject "AuditsAffectedFrame" $ \o -> AuditsAffectedFrame
    <$> o A..: "frameId"
instance ToJSON AuditsAffectedFrame where
  toJSON p = A.object $ catMaybes [
    ("frameId" A..=) <$> Just (auditsAffectedFrameFrameId p)
    ]

-- | Type 'Audits.CookieExclusionReason'.
data AuditsCookieExclusionReason = AuditsCookieExclusionReasonExcludeSameSiteUnspecifiedTreatedAsLax | AuditsCookieExclusionReasonExcludeSameSiteNoneInsecure | AuditsCookieExclusionReasonExcludeSameSiteLax | AuditsCookieExclusionReasonExcludeSameSiteStrict | AuditsCookieExclusionReasonExcludeDomainNonASCII | AuditsCookieExclusionReasonExcludeThirdPartyCookieBlockedInFirstPartySet | AuditsCookieExclusionReasonExcludeThirdPartyPhaseout | AuditsCookieExclusionReasonExcludePortMismatch | AuditsCookieExclusionReasonExcludeSchemeMismatch
  deriving (Ord, Eq, Show, Read)
instance FromJSON AuditsCookieExclusionReason where
  parseJSON = A.withText "AuditsCookieExclusionReason" $ \v -> case v of
    "ExcludeSameSiteUnspecifiedTreatedAsLax" -> pure AuditsCookieExclusionReasonExcludeSameSiteUnspecifiedTreatedAsLax
    "ExcludeSameSiteNoneInsecure" -> pure AuditsCookieExclusionReasonExcludeSameSiteNoneInsecure
    "ExcludeSameSiteLax" -> pure AuditsCookieExclusionReasonExcludeSameSiteLax
    "ExcludeSameSiteStrict" -> pure AuditsCookieExclusionReasonExcludeSameSiteStrict
    "ExcludeDomainNonASCII" -> pure AuditsCookieExclusionReasonExcludeDomainNonASCII
    "ExcludeThirdPartyCookieBlockedInFirstPartySet" -> pure AuditsCookieExclusionReasonExcludeThirdPartyCookieBlockedInFirstPartySet
    "ExcludeThirdPartyPhaseout" -> pure AuditsCookieExclusionReasonExcludeThirdPartyPhaseout
    "ExcludePortMismatch" -> pure AuditsCookieExclusionReasonExcludePortMismatch
    "ExcludeSchemeMismatch" -> pure AuditsCookieExclusionReasonExcludeSchemeMismatch
    "_" -> fail "failed to parse AuditsCookieExclusionReason"
instance ToJSON AuditsCookieExclusionReason where
  toJSON v = A.String $ case v of
    AuditsCookieExclusionReasonExcludeSameSiteUnspecifiedTreatedAsLax -> "ExcludeSameSiteUnspecifiedTreatedAsLax"
    AuditsCookieExclusionReasonExcludeSameSiteNoneInsecure -> "ExcludeSameSiteNoneInsecure"
    AuditsCookieExclusionReasonExcludeSameSiteLax -> "ExcludeSameSiteLax"
    AuditsCookieExclusionReasonExcludeSameSiteStrict -> "ExcludeSameSiteStrict"
    AuditsCookieExclusionReasonExcludeDomainNonASCII -> "ExcludeDomainNonASCII"
    AuditsCookieExclusionReasonExcludeThirdPartyCookieBlockedInFirstPartySet -> "ExcludeThirdPartyCookieBlockedInFirstPartySet"
    AuditsCookieExclusionReasonExcludeThirdPartyPhaseout -> "ExcludeThirdPartyPhaseout"
    AuditsCookieExclusionReasonExcludePortMismatch -> "ExcludePortMismatch"
    AuditsCookieExclusionReasonExcludeSchemeMismatch -> "ExcludeSchemeMismatch"

-- | Type 'Audits.CookieWarningReason'.
data AuditsCookieWarningReason = AuditsCookieWarningReasonWarnSameSiteUnspecifiedCrossSiteContext | AuditsCookieWarningReasonWarnSameSiteNoneInsecure | AuditsCookieWarningReasonWarnSameSiteUnspecifiedLaxAllowUnsafe | AuditsCookieWarningReasonWarnSameSiteStrictLaxDowngradeStrict | AuditsCookieWarningReasonWarnSameSiteStrictCrossDowngradeStrict | AuditsCookieWarningReasonWarnSameSiteStrictCrossDowngradeLax | AuditsCookieWarningReasonWarnSameSiteLaxCrossDowngradeStrict | AuditsCookieWarningReasonWarnSameSiteLaxCrossDowngradeLax | AuditsCookieWarningReasonWarnAttributeValueExceedsMaxSize | AuditsCookieWarningReasonWarnDomainNonASCII | AuditsCookieWarningReasonWarnThirdPartyPhaseout | AuditsCookieWarningReasonWarnCrossSiteRedirectDowngradeChangesInclusion | AuditsCookieWarningReasonWarnDeprecationTrialMetadata | AuditsCookieWarningReasonWarnThirdPartyCookieHeuristic
  deriving (Ord, Eq, Show, Read)
instance FromJSON AuditsCookieWarningReason where
  parseJSON = A.withText "AuditsCookieWarningReason" $ \v -> case v of
    "WarnSameSiteUnspecifiedCrossSiteContext" -> pure AuditsCookieWarningReasonWarnSameSiteUnspecifiedCrossSiteContext
    "WarnSameSiteNoneInsecure" -> pure AuditsCookieWarningReasonWarnSameSiteNoneInsecure
    "WarnSameSiteUnspecifiedLaxAllowUnsafe" -> pure AuditsCookieWarningReasonWarnSameSiteUnspecifiedLaxAllowUnsafe
    "WarnSameSiteStrictLaxDowngradeStrict" -> pure AuditsCookieWarningReasonWarnSameSiteStrictLaxDowngradeStrict
    "WarnSameSiteStrictCrossDowngradeStrict" -> pure AuditsCookieWarningReasonWarnSameSiteStrictCrossDowngradeStrict
    "WarnSameSiteStrictCrossDowngradeLax" -> pure AuditsCookieWarningReasonWarnSameSiteStrictCrossDowngradeLax
    "WarnSameSiteLaxCrossDowngradeStrict" -> pure AuditsCookieWarningReasonWarnSameSiteLaxCrossDowngradeStrict
    "WarnSameSiteLaxCrossDowngradeLax" -> pure AuditsCookieWarningReasonWarnSameSiteLaxCrossDowngradeLax
    "WarnAttributeValueExceedsMaxSize" -> pure AuditsCookieWarningReasonWarnAttributeValueExceedsMaxSize
    "WarnDomainNonASCII" -> pure AuditsCookieWarningReasonWarnDomainNonASCII
    "WarnThirdPartyPhaseout" -> pure AuditsCookieWarningReasonWarnThirdPartyPhaseout
    "WarnCrossSiteRedirectDowngradeChangesInclusion" -> pure AuditsCookieWarningReasonWarnCrossSiteRedirectDowngradeChangesInclusion
    "WarnDeprecationTrialMetadata" -> pure AuditsCookieWarningReasonWarnDeprecationTrialMetadata
    "WarnThirdPartyCookieHeuristic" -> pure AuditsCookieWarningReasonWarnThirdPartyCookieHeuristic
    "_" -> fail "failed to parse AuditsCookieWarningReason"
instance ToJSON AuditsCookieWarningReason where
  toJSON v = A.String $ case v of
    AuditsCookieWarningReasonWarnSameSiteUnspecifiedCrossSiteContext -> "WarnSameSiteUnspecifiedCrossSiteContext"
    AuditsCookieWarningReasonWarnSameSiteNoneInsecure -> "WarnSameSiteNoneInsecure"
    AuditsCookieWarningReasonWarnSameSiteUnspecifiedLaxAllowUnsafe -> "WarnSameSiteUnspecifiedLaxAllowUnsafe"
    AuditsCookieWarningReasonWarnSameSiteStrictLaxDowngradeStrict -> "WarnSameSiteStrictLaxDowngradeStrict"
    AuditsCookieWarningReasonWarnSameSiteStrictCrossDowngradeStrict -> "WarnSameSiteStrictCrossDowngradeStrict"
    AuditsCookieWarningReasonWarnSameSiteStrictCrossDowngradeLax -> "WarnSameSiteStrictCrossDowngradeLax"
    AuditsCookieWarningReasonWarnSameSiteLaxCrossDowngradeStrict -> "WarnSameSiteLaxCrossDowngradeStrict"
    AuditsCookieWarningReasonWarnSameSiteLaxCrossDowngradeLax -> "WarnSameSiteLaxCrossDowngradeLax"
    AuditsCookieWarningReasonWarnAttributeValueExceedsMaxSize -> "WarnAttributeValueExceedsMaxSize"
    AuditsCookieWarningReasonWarnDomainNonASCII -> "WarnDomainNonASCII"
    AuditsCookieWarningReasonWarnThirdPartyPhaseout -> "WarnThirdPartyPhaseout"
    AuditsCookieWarningReasonWarnCrossSiteRedirectDowngradeChangesInclusion -> "WarnCrossSiteRedirectDowngradeChangesInclusion"
    AuditsCookieWarningReasonWarnDeprecationTrialMetadata -> "WarnDeprecationTrialMetadata"
    AuditsCookieWarningReasonWarnThirdPartyCookieHeuristic -> "WarnThirdPartyCookieHeuristic"

-- | Type 'Audits.CookieOperation'.
data AuditsCookieOperation = AuditsCookieOperationSetCookie | AuditsCookieOperationReadCookie
  deriving (Ord, Eq, Show, Read)
instance FromJSON AuditsCookieOperation where
  parseJSON = A.withText "AuditsCookieOperation" $ \v -> case v of
    "SetCookie" -> pure AuditsCookieOperationSetCookie
    "ReadCookie" -> pure AuditsCookieOperationReadCookie
    "_" -> fail "failed to parse AuditsCookieOperation"
instance ToJSON AuditsCookieOperation where
  toJSON v = A.String $ case v of
    AuditsCookieOperationSetCookie -> "SetCookie"
    AuditsCookieOperationReadCookie -> "ReadCookie"

-- | Type 'Audits.InsightType'.
--   Represents the category of insight that a cookie issue falls under.
data AuditsInsightType = AuditsInsightTypeGitHubResource | AuditsInsightTypeGracePeriod | AuditsInsightTypeHeuristics
  deriving (Ord, Eq, Show, Read)
instance FromJSON AuditsInsightType where
  parseJSON = A.withText "AuditsInsightType" $ \v -> case v of
    "GitHubResource" -> pure AuditsInsightTypeGitHubResource
    "GracePeriod" -> pure AuditsInsightTypeGracePeriod
    "Heuristics" -> pure AuditsInsightTypeHeuristics
    "_" -> fail "failed to parse AuditsInsightType"
instance ToJSON AuditsInsightType where
  toJSON v = A.String $ case v of
    AuditsInsightTypeGitHubResource -> "GitHubResource"
    AuditsInsightTypeGracePeriod -> "GracePeriod"
    AuditsInsightTypeHeuristics -> "Heuristics"

-- | Type 'Audits.CookieIssueInsight'.
--   Information about the suggested solution to a cookie issue.
data AuditsCookieIssueInsight = AuditsCookieIssueInsight
  {
    auditsCookieIssueInsightType :: AuditsInsightType,
    -- | Link to table entry in third-party cookie migration readiness list.
    auditsCookieIssueInsightTableEntryUrl :: Maybe T.Text
  }
  deriving (Eq, Show)
instance FromJSON AuditsCookieIssueInsight where
  parseJSON = A.withObject "AuditsCookieIssueInsight" $ \o -> AuditsCookieIssueInsight
    <$> o A..: "type"
    <*> o A..:? "tableEntryUrl"
instance ToJSON AuditsCookieIssueInsight where
  toJSON p = A.object $ catMaybes [
    ("type" A..=) <$> Just (auditsCookieIssueInsightType p),
    ("tableEntryUrl" A..=) <$> (auditsCookieIssueInsightTableEntryUrl p)
    ]

-- | Type 'Audits.CookieIssueDetails'.
--   This information is currently necessary, as the front-end has a difficult
--   time finding a specific cookie. With this, we can convey specific error
--   information without the cookie.
data AuditsCookieIssueDetails = AuditsCookieIssueDetails
  {
    -- | If AffectedCookie is not set then rawCookieLine contains the raw
    --   Set-Cookie header string. This hints at a problem where the
    --   cookie line is syntactically or semantically malformed in a way
    --   that no valid cookie could be created.
    auditsCookieIssueDetailsCookie :: Maybe AuditsAffectedCookie,
    auditsCookieIssueDetailsRawCookieLine :: Maybe T.Text,
    auditsCookieIssueDetailsCookieWarningReasons :: [AuditsCookieWarningReason],
    auditsCookieIssueDetailsCookieExclusionReasons :: [AuditsCookieExclusionReason],
    -- | Optionally identifies the site-for-cookies and the cookie url, which
    --   may be used by the front-end as additional context.
    auditsCookieIssueDetailsOperation :: AuditsCookieOperation,
    auditsCookieIssueDetailsSiteForCookies :: Maybe T.Text,
    auditsCookieIssueDetailsCookieUrl :: Maybe T.Text,
    auditsCookieIssueDetailsRequest :: Maybe AuditsAffectedRequest,
    -- | The recommended solution to the issue.
    auditsCookieIssueDetailsInsight :: Maybe AuditsCookieIssueInsight
  }
  deriving (Eq, Show)
instance FromJSON AuditsCookieIssueDetails where
  parseJSON = A.withObject "AuditsCookieIssueDetails" $ \o -> AuditsCookieIssueDetails
    <$> o A..:? "cookie"
    <*> o A..:? "rawCookieLine"
    <*> o A..: "cookieWarningReasons"
    <*> o A..: "cookieExclusionReasons"
    <*> o A..: "operation"
    <*> o A..:? "siteForCookies"
    <*> o A..:? "cookieUrl"
    <*> o A..:? "request"
    <*> o A..:? "insight"
instance ToJSON AuditsCookieIssueDetails where
  toJSON p = A.object $ catMaybes [
    ("cookie" A..=) <$> (auditsCookieIssueDetailsCookie p),
    ("rawCookieLine" A..=) <$> (auditsCookieIssueDetailsRawCookieLine p),
    ("cookieWarningReasons" A..=) <$> Just (auditsCookieIssueDetailsCookieWarningReasons p),
    ("cookieExclusionReasons" A..=) <$> Just (auditsCookieIssueDetailsCookieExclusionReasons p),
    ("operation" A..=) <$> Just (auditsCookieIssueDetailsOperation p),
    ("siteForCookies" A..=) <$> (auditsCookieIssueDetailsSiteForCookies p),
    ("cookieUrl" A..=) <$> (auditsCookieIssueDetailsCookieUrl p),
    ("request" A..=) <$> (auditsCookieIssueDetailsRequest p),
    ("insight" A..=) <$> (auditsCookieIssueDetailsInsight p)
    ]

-- | Type 'Audits.PerformanceIssueType'.
data AuditsPerformanceIssueType = AuditsPerformanceIssueTypeDocumentCookie
  deriving (Ord, Eq, Show, Read)
instance FromJSON AuditsPerformanceIssueType where
  parseJSON = A.withText "AuditsPerformanceIssueType" $ \v -> case v of
    "DocumentCookie" -> pure AuditsPerformanceIssueTypeDocumentCookie
    "_" -> fail "failed to parse AuditsPerformanceIssueType"
instance ToJSON AuditsPerformanceIssueType where
  toJSON v = A.String $ case v of
    AuditsPerformanceIssueTypeDocumentCookie -> "DocumentCookie"

-- | Type 'Audits.PerformanceIssueDetails'.
--   Details for a performance issue.
data AuditsPerformanceIssueDetails = AuditsPerformanceIssueDetails
  {
    auditsPerformanceIssueDetailsPerformanceIssueType :: AuditsPerformanceIssueType,
    auditsPerformanceIssueDetailsSourceCodeLocation :: Maybe AuditsSourceCodeLocation
  }
  deriving (Eq, Show)
instance FromJSON AuditsPerformanceIssueDetails where
  parseJSON = A.withObject "AuditsPerformanceIssueDetails" $ \o -> AuditsPerformanceIssueDetails
    <$> o A..: "performanceIssueType"
    <*> o A..:? "sourceCodeLocation"
instance ToJSON AuditsPerformanceIssueDetails where
  toJSON p = A.object $ catMaybes [
    ("performanceIssueType" A..=) <$> Just (auditsPerformanceIssueDetailsPerformanceIssueType p),
    ("sourceCodeLocation" A..=) <$> (auditsPerformanceIssueDetailsSourceCodeLocation p)
    ]

-- | Type 'Audits.MixedContentResolutionStatus'.
data AuditsMixedContentResolutionStatus = AuditsMixedContentResolutionStatusMixedContentBlocked | AuditsMixedContentResolutionStatusMixedContentAutomaticallyUpgraded | AuditsMixedContentResolutionStatusMixedContentWarning
  deriving (Ord, Eq, Show, Read)
instance FromJSON AuditsMixedContentResolutionStatus where
  parseJSON = A.withText "AuditsMixedContentResolutionStatus" $ \v -> case v of
    "MixedContentBlocked" -> pure AuditsMixedContentResolutionStatusMixedContentBlocked
    "MixedContentAutomaticallyUpgraded" -> pure AuditsMixedContentResolutionStatusMixedContentAutomaticallyUpgraded
    "MixedContentWarning" -> pure AuditsMixedContentResolutionStatusMixedContentWarning
    "_" -> fail "failed to parse AuditsMixedContentResolutionStatus"
instance ToJSON AuditsMixedContentResolutionStatus where
  toJSON v = A.String $ case v of
    AuditsMixedContentResolutionStatusMixedContentBlocked -> "MixedContentBlocked"
    AuditsMixedContentResolutionStatusMixedContentAutomaticallyUpgraded -> "MixedContentAutomaticallyUpgraded"
    AuditsMixedContentResolutionStatusMixedContentWarning -> "MixedContentWarning"

-- | Type 'Audits.MixedContentResourceType'.
data AuditsMixedContentResourceType = AuditsMixedContentResourceTypeAudio | AuditsMixedContentResourceTypeBeacon | AuditsMixedContentResourceTypeCSPReport | AuditsMixedContentResourceTypeDownload | AuditsMixedContentResourceTypeEventSource | AuditsMixedContentResourceTypeFavicon | AuditsMixedContentResourceTypeFont | AuditsMixedContentResourceTypeForm | AuditsMixedContentResourceTypeFrame | AuditsMixedContentResourceTypeImage | AuditsMixedContentResourceTypeImport | AuditsMixedContentResourceTypeJSON | AuditsMixedContentResourceTypeManifest | AuditsMixedContentResourceTypePing | AuditsMixedContentResourceTypePluginData | AuditsMixedContentResourceTypePluginResource | AuditsMixedContentResourceTypePrefetch | AuditsMixedContentResourceTypeResource | AuditsMixedContentResourceTypeScript | AuditsMixedContentResourceTypeServiceWorker | AuditsMixedContentResourceTypeSharedWorker | AuditsMixedContentResourceTypeSpeculationRules | AuditsMixedContentResourceTypeStylesheet | AuditsMixedContentResourceTypeTrack | AuditsMixedContentResourceTypeVideo | AuditsMixedContentResourceTypeWorker | AuditsMixedContentResourceTypeXMLHttpRequest | AuditsMixedContentResourceTypeXSLT
  deriving (Ord, Eq, Show, Read)
instance FromJSON AuditsMixedContentResourceType where
  parseJSON = A.withText "AuditsMixedContentResourceType" $ \v -> case v of
    "Audio" -> pure AuditsMixedContentResourceTypeAudio
    "Beacon" -> pure AuditsMixedContentResourceTypeBeacon
    "CSPReport" -> pure AuditsMixedContentResourceTypeCSPReport
    "Download" -> pure AuditsMixedContentResourceTypeDownload
    "EventSource" -> pure AuditsMixedContentResourceTypeEventSource
    "Favicon" -> pure AuditsMixedContentResourceTypeFavicon
    "Font" -> pure AuditsMixedContentResourceTypeFont
    "Form" -> pure AuditsMixedContentResourceTypeForm
    "Frame" -> pure AuditsMixedContentResourceTypeFrame
    "Image" -> pure AuditsMixedContentResourceTypeImage
    "Import" -> pure AuditsMixedContentResourceTypeImport
    "JSON" -> pure AuditsMixedContentResourceTypeJSON
    "Manifest" -> pure AuditsMixedContentResourceTypeManifest
    "Ping" -> pure AuditsMixedContentResourceTypePing
    "PluginData" -> pure AuditsMixedContentResourceTypePluginData
    "PluginResource" -> pure AuditsMixedContentResourceTypePluginResource
    "Prefetch" -> pure AuditsMixedContentResourceTypePrefetch
    "Resource" -> pure AuditsMixedContentResourceTypeResource
    "Script" -> pure AuditsMixedContentResourceTypeScript
    "ServiceWorker" -> pure AuditsMixedContentResourceTypeServiceWorker
    "SharedWorker" -> pure AuditsMixedContentResourceTypeSharedWorker
    "SpeculationRules" -> pure AuditsMixedContentResourceTypeSpeculationRules
    "Stylesheet" -> pure AuditsMixedContentResourceTypeStylesheet
    "Track" -> pure AuditsMixedContentResourceTypeTrack
    "Video" -> pure AuditsMixedContentResourceTypeVideo
    "Worker" -> pure AuditsMixedContentResourceTypeWorker
    "XMLHttpRequest" -> pure AuditsMixedContentResourceTypeXMLHttpRequest
    "XSLT" -> pure AuditsMixedContentResourceTypeXSLT
    "_" -> fail "failed to parse AuditsMixedContentResourceType"
instance ToJSON AuditsMixedContentResourceType where
  toJSON v = A.String $ case v of
    AuditsMixedContentResourceTypeAudio -> "Audio"
    AuditsMixedContentResourceTypeBeacon -> "Beacon"
    AuditsMixedContentResourceTypeCSPReport -> "CSPReport"
    AuditsMixedContentResourceTypeDownload -> "Download"
    AuditsMixedContentResourceTypeEventSource -> "EventSource"
    AuditsMixedContentResourceTypeFavicon -> "Favicon"
    AuditsMixedContentResourceTypeFont -> "Font"
    AuditsMixedContentResourceTypeForm -> "Form"
    AuditsMixedContentResourceTypeFrame -> "Frame"
    AuditsMixedContentResourceTypeImage -> "Image"
    AuditsMixedContentResourceTypeImport -> "Import"
    AuditsMixedContentResourceTypeJSON -> "JSON"
    AuditsMixedContentResourceTypeManifest -> "Manifest"
    AuditsMixedContentResourceTypePing -> "Ping"
    AuditsMixedContentResourceTypePluginData -> "PluginData"
    AuditsMixedContentResourceTypePluginResource -> "PluginResource"
    AuditsMixedContentResourceTypePrefetch -> "Prefetch"
    AuditsMixedContentResourceTypeResource -> "Resource"
    AuditsMixedContentResourceTypeScript -> "Script"
    AuditsMixedContentResourceTypeServiceWorker -> "ServiceWorker"
    AuditsMixedContentResourceTypeSharedWorker -> "SharedWorker"
    AuditsMixedContentResourceTypeSpeculationRules -> "SpeculationRules"
    AuditsMixedContentResourceTypeStylesheet -> "Stylesheet"
    AuditsMixedContentResourceTypeTrack -> "Track"
    AuditsMixedContentResourceTypeVideo -> "Video"
    AuditsMixedContentResourceTypeWorker -> "Worker"
    AuditsMixedContentResourceTypeXMLHttpRequest -> "XMLHttpRequest"
    AuditsMixedContentResourceTypeXSLT -> "XSLT"

-- | Type 'Audits.MixedContentIssueDetails'.
data AuditsMixedContentIssueDetails = AuditsMixedContentIssueDetails
  {
    -- | The type of resource causing the mixed content issue (css, js, iframe,
    --   form,...). Marked as optional because it is mapped to from
    --   blink::mojom::RequestContextType, which will be replaced
    --   by network::mojom::RequestDestination
    auditsMixedContentIssueDetailsResourceType :: Maybe AuditsMixedContentResourceType,
    -- | The way the mixed content issue is being resolved.
    auditsMixedContentIssueDetailsResolutionStatus :: AuditsMixedContentResolutionStatus,
    -- | The unsafe http url causing the mixed content issue.
    auditsMixedContentIssueDetailsInsecureURL :: T.Text,
    -- | The url responsible for the call to an unsafe url.
    auditsMixedContentIssueDetailsMainResourceURL :: T.Text,
    -- | The mixed content request.
    --   Does not always exist (e.g. for unsafe form submission urls).
    auditsMixedContentIssueDetailsRequest :: Maybe AuditsAffectedRequest,
    -- | Optional because not every mixed content issue is necessarily linked to a frame.
    auditsMixedContentIssueDetailsFrame :: Maybe AuditsAffectedFrame
  }
  deriving (Eq, Show)
instance FromJSON AuditsMixedContentIssueDetails where
  parseJSON = A.withObject "AuditsMixedContentIssueDetails" $ \o -> AuditsMixedContentIssueDetails
    <$> o A..:? "resourceType"
    <*> o A..: "resolutionStatus"
    <*> o A..: "insecureURL"
    <*> o A..: "mainResourceURL"
    <*> o A..:? "request"
    <*> o A..:? "frame"
instance ToJSON AuditsMixedContentIssueDetails where
  toJSON p = A.object $ catMaybes [
    ("resourceType" A..=) <$> (auditsMixedContentIssueDetailsResourceType p),
    ("resolutionStatus" A..=) <$> Just (auditsMixedContentIssueDetailsResolutionStatus p),
    ("insecureURL" A..=) <$> Just (auditsMixedContentIssueDetailsInsecureURL p),
    ("mainResourceURL" A..=) <$> Just (auditsMixedContentIssueDetailsMainResourceURL p),
    ("request" A..=) <$> (auditsMixedContentIssueDetailsRequest p),
    ("frame" A..=) <$> (auditsMixedContentIssueDetailsFrame p)
    ]

-- | Type 'Audits.BlockedByResponseReason'.
--   Enum indicating the reason a response has been blocked. These reasons are
--   refinements of the net error BLOCKED_BY_RESPONSE.
data AuditsBlockedByResponseReason = AuditsBlockedByResponseReasonCoepFrameResourceNeedsCoepHeader | AuditsBlockedByResponseReasonCoopSandboxedIFrameCannotNavigateToCoopPage | AuditsBlockedByResponseReasonCorpNotSameOrigin | AuditsBlockedByResponseReasonCorpNotSameOriginAfterDefaultedToSameOriginByCoep | AuditsBlockedByResponseReasonCorpNotSameOriginAfterDefaultedToSameOriginByDip | AuditsBlockedByResponseReasonCorpNotSameOriginAfterDefaultedToSameOriginByCoepAndDip | AuditsBlockedByResponseReasonCorpNotSameSite | AuditsBlockedByResponseReasonSRIMessageSignatureMismatch
  deriving (Ord, Eq, Show, Read)
instance FromJSON AuditsBlockedByResponseReason where
  parseJSON = A.withText "AuditsBlockedByResponseReason" $ \v -> case v of
    "CoepFrameResourceNeedsCoepHeader" -> pure AuditsBlockedByResponseReasonCoepFrameResourceNeedsCoepHeader
    "CoopSandboxedIFrameCannotNavigateToCoopPage" -> pure AuditsBlockedByResponseReasonCoopSandboxedIFrameCannotNavigateToCoopPage
    "CorpNotSameOrigin" -> pure AuditsBlockedByResponseReasonCorpNotSameOrigin
    "CorpNotSameOriginAfterDefaultedToSameOriginByCoep" -> pure AuditsBlockedByResponseReasonCorpNotSameOriginAfterDefaultedToSameOriginByCoep
    "CorpNotSameOriginAfterDefaultedToSameOriginByDip" -> pure AuditsBlockedByResponseReasonCorpNotSameOriginAfterDefaultedToSameOriginByDip
    "CorpNotSameOriginAfterDefaultedToSameOriginByCoepAndDip" -> pure AuditsBlockedByResponseReasonCorpNotSameOriginAfterDefaultedToSameOriginByCoepAndDip
    "CorpNotSameSite" -> pure AuditsBlockedByResponseReasonCorpNotSameSite
    "SRIMessageSignatureMismatch" -> pure AuditsBlockedByResponseReasonSRIMessageSignatureMismatch
    "_" -> fail "failed to parse AuditsBlockedByResponseReason"
instance ToJSON AuditsBlockedByResponseReason where
  toJSON v = A.String $ case v of
    AuditsBlockedByResponseReasonCoepFrameResourceNeedsCoepHeader -> "CoepFrameResourceNeedsCoepHeader"
    AuditsBlockedByResponseReasonCoopSandboxedIFrameCannotNavigateToCoopPage -> "CoopSandboxedIFrameCannotNavigateToCoopPage"
    AuditsBlockedByResponseReasonCorpNotSameOrigin -> "CorpNotSameOrigin"
    AuditsBlockedByResponseReasonCorpNotSameOriginAfterDefaultedToSameOriginByCoep -> "CorpNotSameOriginAfterDefaultedToSameOriginByCoep"
    AuditsBlockedByResponseReasonCorpNotSameOriginAfterDefaultedToSameOriginByDip -> "CorpNotSameOriginAfterDefaultedToSameOriginByDip"
    AuditsBlockedByResponseReasonCorpNotSameOriginAfterDefaultedToSameOriginByCoepAndDip -> "CorpNotSameOriginAfterDefaultedToSameOriginByCoepAndDip"
    AuditsBlockedByResponseReasonCorpNotSameSite -> "CorpNotSameSite"
    AuditsBlockedByResponseReasonSRIMessageSignatureMismatch -> "SRIMessageSignatureMismatch"

-- | Type 'Audits.BlockedByResponseIssueDetails'.
--   Details for a request that has been blocked with the BLOCKED_BY_RESPONSE
--   code. Currently only used for COEP/COOP, but may be extended to include
--   some CSP errors in the future.
data AuditsBlockedByResponseIssueDetails = AuditsBlockedByResponseIssueDetails
  {
    auditsBlockedByResponseIssueDetailsRequest :: AuditsAffectedRequest,
    auditsBlockedByResponseIssueDetailsParentFrame :: Maybe AuditsAffectedFrame,
    auditsBlockedByResponseIssueDetailsBlockedFrame :: Maybe AuditsAffectedFrame,
    auditsBlockedByResponseIssueDetailsReason :: AuditsBlockedByResponseReason
  }
  deriving (Eq, Show)
instance FromJSON AuditsBlockedByResponseIssueDetails where
  parseJSON = A.withObject "AuditsBlockedByResponseIssueDetails" $ \o -> AuditsBlockedByResponseIssueDetails
    <$> o A..: "request"
    <*> o A..:? "parentFrame"
    <*> o A..:? "blockedFrame"
    <*> o A..: "reason"
instance ToJSON AuditsBlockedByResponseIssueDetails where
  toJSON p = A.object $ catMaybes [
    ("request" A..=) <$> Just (auditsBlockedByResponseIssueDetailsRequest p),
    ("parentFrame" A..=) <$> (auditsBlockedByResponseIssueDetailsParentFrame p),
    ("blockedFrame" A..=) <$> (auditsBlockedByResponseIssueDetailsBlockedFrame p),
    ("reason" A..=) <$> Just (auditsBlockedByResponseIssueDetailsReason p)
    ]

-- | Type 'Audits.HeavyAdResolutionStatus'.
data AuditsHeavyAdResolutionStatus = AuditsHeavyAdResolutionStatusHeavyAdBlocked | AuditsHeavyAdResolutionStatusHeavyAdWarning
  deriving (Ord, Eq, Show, Read)
instance FromJSON AuditsHeavyAdResolutionStatus where
  parseJSON = A.withText "AuditsHeavyAdResolutionStatus" $ \v -> case v of
    "HeavyAdBlocked" -> pure AuditsHeavyAdResolutionStatusHeavyAdBlocked
    "HeavyAdWarning" -> pure AuditsHeavyAdResolutionStatusHeavyAdWarning
    "_" -> fail "failed to parse AuditsHeavyAdResolutionStatus"
instance ToJSON AuditsHeavyAdResolutionStatus where
  toJSON v = A.String $ case v of
    AuditsHeavyAdResolutionStatusHeavyAdBlocked -> "HeavyAdBlocked"
    AuditsHeavyAdResolutionStatusHeavyAdWarning -> "HeavyAdWarning"

-- | Type 'Audits.HeavyAdReason'.
data AuditsHeavyAdReason = AuditsHeavyAdReasonNetworkTotalLimit | AuditsHeavyAdReasonCpuTotalLimit | AuditsHeavyAdReasonCpuPeakLimit
  deriving (Ord, Eq, Show, Read)
instance FromJSON AuditsHeavyAdReason where
  parseJSON = A.withText "AuditsHeavyAdReason" $ \v -> case v of
    "NetworkTotalLimit" -> pure AuditsHeavyAdReasonNetworkTotalLimit
    "CpuTotalLimit" -> pure AuditsHeavyAdReasonCpuTotalLimit
    "CpuPeakLimit" -> pure AuditsHeavyAdReasonCpuPeakLimit
    "_" -> fail "failed to parse AuditsHeavyAdReason"
instance ToJSON AuditsHeavyAdReason where
  toJSON v = A.String $ case v of
    AuditsHeavyAdReasonNetworkTotalLimit -> "NetworkTotalLimit"
    AuditsHeavyAdReasonCpuTotalLimit -> "CpuTotalLimit"
    AuditsHeavyAdReasonCpuPeakLimit -> "CpuPeakLimit"

-- | Type 'Audits.HeavyAdIssueDetails'.
data AuditsHeavyAdIssueDetails = AuditsHeavyAdIssueDetails
  {
    -- | The resolution status, either blocking the content or warning.
    auditsHeavyAdIssueDetailsResolution :: AuditsHeavyAdResolutionStatus,
    -- | The reason the ad was blocked, total network or cpu or peak cpu.
    auditsHeavyAdIssueDetailsReason :: AuditsHeavyAdReason,
    -- | The frame that was blocked.
    auditsHeavyAdIssueDetailsFrame :: AuditsAffectedFrame
  }
  deriving (Eq, Show)
instance FromJSON AuditsHeavyAdIssueDetails where
  parseJSON = A.withObject "AuditsHeavyAdIssueDetails" $ \o -> AuditsHeavyAdIssueDetails
    <$> o A..: "resolution"
    <*> o A..: "reason"
    <*> o A..: "frame"
instance ToJSON AuditsHeavyAdIssueDetails where
  toJSON p = A.object $ catMaybes [
    ("resolution" A..=) <$> Just (auditsHeavyAdIssueDetailsResolution p),
    ("reason" A..=) <$> Just (auditsHeavyAdIssueDetailsReason p),
    ("frame" A..=) <$> Just (auditsHeavyAdIssueDetailsFrame p)
    ]

-- | Type 'Audits.ContentSecurityPolicyViolationType'.
data AuditsContentSecurityPolicyViolationType = AuditsContentSecurityPolicyViolationTypeKInlineViolation | AuditsContentSecurityPolicyViolationTypeKEvalViolation | AuditsContentSecurityPolicyViolationTypeKURLViolation | AuditsContentSecurityPolicyViolationTypeKSRIViolation | AuditsContentSecurityPolicyViolationTypeKTrustedTypesSinkViolation | AuditsContentSecurityPolicyViolationTypeKTrustedTypesPolicyViolation | AuditsContentSecurityPolicyViolationTypeKWasmEvalViolation
  deriving (Ord, Eq, Show, Read)
instance FromJSON AuditsContentSecurityPolicyViolationType where
  parseJSON = A.withText "AuditsContentSecurityPolicyViolationType" $ \v -> case v of
    "kInlineViolation" -> pure AuditsContentSecurityPolicyViolationTypeKInlineViolation
    "kEvalViolation" -> pure AuditsContentSecurityPolicyViolationTypeKEvalViolation
    "kURLViolation" -> pure AuditsContentSecurityPolicyViolationTypeKURLViolation
    "kSRIViolation" -> pure AuditsContentSecurityPolicyViolationTypeKSRIViolation
    "kTrustedTypesSinkViolation" -> pure AuditsContentSecurityPolicyViolationTypeKTrustedTypesSinkViolation
    "kTrustedTypesPolicyViolation" -> pure AuditsContentSecurityPolicyViolationTypeKTrustedTypesPolicyViolation
    "kWasmEvalViolation" -> pure AuditsContentSecurityPolicyViolationTypeKWasmEvalViolation
    "_" -> fail "failed to parse AuditsContentSecurityPolicyViolationType"
instance ToJSON AuditsContentSecurityPolicyViolationType where
  toJSON v = A.String $ case v of
    AuditsContentSecurityPolicyViolationTypeKInlineViolation -> "kInlineViolation"
    AuditsContentSecurityPolicyViolationTypeKEvalViolation -> "kEvalViolation"
    AuditsContentSecurityPolicyViolationTypeKURLViolation -> "kURLViolation"
    AuditsContentSecurityPolicyViolationTypeKSRIViolation -> "kSRIViolation"
    AuditsContentSecurityPolicyViolationTypeKTrustedTypesSinkViolation -> "kTrustedTypesSinkViolation"
    AuditsContentSecurityPolicyViolationTypeKTrustedTypesPolicyViolation -> "kTrustedTypesPolicyViolation"
    AuditsContentSecurityPolicyViolationTypeKWasmEvalViolation -> "kWasmEvalViolation"

-- | Type 'Audits.SourceCodeLocation'.
data AuditsSourceCodeLocation = AuditsSourceCodeLocation
  {
    auditsSourceCodeLocationScriptId :: Maybe Runtime.RuntimeScriptId,
    auditsSourceCodeLocationUrl :: T.Text,
    auditsSourceCodeLocationLineNumber :: Int,
    auditsSourceCodeLocationColumnNumber :: Int
  }
  deriving (Eq, Show)
instance FromJSON AuditsSourceCodeLocation where
  parseJSON = A.withObject "AuditsSourceCodeLocation" $ \o -> AuditsSourceCodeLocation
    <$> o A..:? "scriptId"
    <*> o A..: "url"
    <*> o A..: "lineNumber"
    <*> o A..: "columnNumber"
instance ToJSON AuditsSourceCodeLocation where
  toJSON p = A.object $ catMaybes [
    ("scriptId" A..=) <$> (auditsSourceCodeLocationScriptId p),
    ("url" A..=) <$> Just (auditsSourceCodeLocationUrl p),
    ("lineNumber" A..=) <$> Just (auditsSourceCodeLocationLineNumber p),
    ("columnNumber" A..=) <$> Just (auditsSourceCodeLocationColumnNumber p)
    ]

-- | Type 'Audits.ContentSecurityPolicyIssueDetails'.
data AuditsContentSecurityPolicyIssueDetails = AuditsContentSecurityPolicyIssueDetails
  {
    -- | The url not included in allowed sources.
    auditsContentSecurityPolicyIssueDetailsBlockedURL :: Maybe T.Text,
    -- | Specific directive that is violated, causing the CSP issue.
    auditsContentSecurityPolicyIssueDetailsViolatedDirective :: T.Text,
    auditsContentSecurityPolicyIssueDetailsIsReportOnly :: Bool,
    auditsContentSecurityPolicyIssueDetailsContentSecurityPolicyViolationType :: AuditsContentSecurityPolicyViolationType,
    auditsContentSecurityPolicyIssueDetailsFrameAncestor :: Maybe AuditsAffectedFrame,
    auditsContentSecurityPolicyIssueDetailsSourceCodeLocation :: Maybe AuditsSourceCodeLocation,
    auditsContentSecurityPolicyIssueDetailsViolatingNodeId :: Maybe DOMNetworkEmulationPageSecurity.DOMBackendNodeId
  }
  deriving (Eq, Show)
instance FromJSON AuditsContentSecurityPolicyIssueDetails where
  parseJSON = A.withObject "AuditsContentSecurityPolicyIssueDetails" $ \o -> AuditsContentSecurityPolicyIssueDetails
    <$> o A..:? "blockedURL"
    <*> o A..: "violatedDirective"
    <*> o A..: "isReportOnly"
    <*> o A..: "contentSecurityPolicyViolationType"
    <*> o A..:? "frameAncestor"
    <*> o A..:? "sourceCodeLocation"
    <*> o A..:? "violatingNodeId"
instance ToJSON AuditsContentSecurityPolicyIssueDetails where
  toJSON p = A.object $ catMaybes [
    ("blockedURL" A..=) <$> (auditsContentSecurityPolicyIssueDetailsBlockedURL p),
    ("violatedDirective" A..=) <$> Just (auditsContentSecurityPolicyIssueDetailsViolatedDirective p),
    ("isReportOnly" A..=) <$> Just (auditsContentSecurityPolicyIssueDetailsIsReportOnly p),
    ("contentSecurityPolicyViolationType" A..=) <$> Just (auditsContentSecurityPolicyIssueDetailsContentSecurityPolicyViolationType p),
    ("frameAncestor" A..=) <$> (auditsContentSecurityPolicyIssueDetailsFrameAncestor p),
    ("sourceCodeLocation" A..=) <$> (auditsContentSecurityPolicyIssueDetailsSourceCodeLocation p),
    ("violatingNodeId" A..=) <$> (auditsContentSecurityPolicyIssueDetailsViolatingNodeId p)
    ]

-- | Type 'Audits.SharedArrayBufferIssueType'.
data AuditsSharedArrayBufferIssueType = AuditsSharedArrayBufferIssueTypeTransferIssue | AuditsSharedArrayBufferIssueTypeCreationIssue
  deriving (Ord, Eq, Show, Read)
instance FromJSON AuditsSharedArrayBufferIssueType where
  parseJSON = A.withText "AuditsSharedArrayBufferIssueType" $ \v -> case v of
    "TransferIssue" -> pure AuditsSharedArrayBufferIssueTypeTransferIssue
    "CreationIssue" -> pure AuditsSharedArrayBufferIssueTypeCreationIssue
    "_" -> fail "failed to parse AuditsSharedArrayBufferIssueType"
instance ToJSON AuditsSharedArrayBufferIssueType where
  toJSON v = A.String $ case v of
    AuditsSharedArrayBufferIssueTypeTransferIssue -> "TransferIssue"
    AuditsSharedArrayBufferIssueTypeCreationIssue -> "CreationIssue"

-- | Type 'Audits.SharedArrayBufferIssueDetails'.
--   Details for a issue arising from an SAB being instantiated in, or
--   transferred to a context that is not cross-origin isolated.
data AuditsSharedArrayBufferIssueDetails = AuditsSharedArrayBufferIssueDetails
  {
    auditsSharedArrayBufferIssueDetailsSourceCodeLocation :: AuditsSourceCodeLocation,
    auditsSharedArrayBufferIssueDetailsIsWarning :: Bool,
    auditsSharedArrayBufferIssueDetailsType :: AuditsSharedArrayBufferIssueType
  }
  deriving (Eq, Show)
instance FromJSON AuditsSharedArrayBufferIssueDetails where
  parseJSON = A.withObject "AuditsSharedArrayBufferIssueDetails" $ \o -> AuditsSharedArrayBufferIssueDetails
    <$> o A..: "sourceCodeLocation"
    <*> o A..: "isWarning"
    <*> o A..: "type"
instance ToJSON AuditsSharedArrayBufferIssueDetails where
  toJSON p = A.object $ catMaybes [
    ("sourceCodeLocation" A..=) <$> Just (auditsSharedArrayBufferIssueDetailsSourceCodeLocation p),
    ("isWarning" A..=) <$> Just (auditsSharedArrayBufferIssueDetailsIsWarning p),
    ("type" A..=) <$> Just (auditsSharedArrayBufferIssueDetailsType p)
    ]

-- | Type 'Audits.CorsIssueDetails'.
--   Details for a CORS related issue, e.g. a warning or error related to
--   CORS RFC1918 enforcement.
data AuditsCorsIssueDetails = AuditsCorsIssueDetails
  {
    auditsCorsIssueDetailsCorsErrorStatus :: DOMNetworkEmulationPageSecurity.NetworkCorsErrorStatus,
    auditsCorsIssueDetailsIsWarning :: Bool,
    auditsCorsIssueDetailsRequest :: AuditsAffectedRequest,
    auditsCorsIssueDetailsLocation :: Maybe AuditsSourceCodeLocation,
    auditsCorsIssueDetailsInitiatorOrigin :: Maybe T.Text,
    auditsCorsIssueDetailsResourceIPAddressSpace :: Maybe DOMNetworkEmulationPageSecurity.NetworkIPAddressSpace,
    auditsCorsIssueDetailsClientSecurityState :: Maybe DOMNetworkEmulationPageSecurity.NetworkClientSecurityState
  }
  deriving (Eq, Show)
instance FromJSON AuditsCorsIssueDetails where
  parseJSON = A.withObject "AuditsCorsIssueDetails" $ \o -> AuditsCorsIssueDetails
    <$> o A..: "corsErrorStatus"
    <*> o A..: "isWarning"
    <*> o A..: "request"
    <*> o A..:? "location"
    <*> o A..:? "initiatorOrigin"
    <*> o A..:? "resourceIPAddressSpace"
    <*> o A..:? "clientSecurityState"
instance ToJSON AuditsCorsIssueDetails where
  toJSON p = A.object $ catMaybes [
    ("corsErrorStatus" A..=) <$> Just (auditsCorsIssueDetailsCorsErrorStatus p),
    ("isWarning" A..=) <$> Just (auditsCorsIssueDetailsIsWarning p),
    ("request" A..=) <$> Just (auditsCorsIssueDetailsRequest p),
    ("location" A..=) <$> (auditsCorsIssueDetailsLocation p),
    ("initiatorOrigin" A..=) <$> (auditsCorsIssueDetailsInitiatorOrigin p),
    ("resourceIPAddressSpace" A..=) <$> (auditsCorsIssueDetailsResourceIPAddressSpace p),
    ("clientSecurityState" A..=) <$> (auditsCorsIssueDetailsClientSecurityState p)
    ]

-- | Type 'Audits.SharedDictionaryError'.
data AuditsSharedDictionaryError = AuditsSharedDictionaryErrorUseErrorCrossOriginNoCorsRequest | AuditsSharedDictionaryErrorUseErrorDictionaryLoadFailure | AuditsSharedDictionaryErrorUseErrorMatchingDictionaryNotUsed | AuditsSharedDictionaryErrorUseErrorUnexpectedContentDictionaryHeader | AuditsSharedDictionaryErrorWriteErrorCossOriginNoCorsRequest | AuditsSharedDictionaryErrorWriteErrorDisallowedBySettings | AuditsSharedDictionaryErrorWriteErrorExpiredResponse | AuditsSharedDictionaryErrorWriteErrorFeatureDisabled | AuditsSharedDictionaryErrorWriteErrorInsufficientResources | AuditsSharedDictionaryErrorWriteErrorInvalidMatchField | AuditsSharedDictionaryErrorWriteErrorInvalidStructuredHeader | AuditsSharedDictionaryErrorWriteErrorInvalidTTLField | AuditsSharedDictionaryErrorWriteErrorNavigationRequest | AuditsSharedDictionaryErrorWriteErrorNoMatchField | AuditsSharedDictionaryErrorWriteErrorNonIntegerTTLField | AuditsSharedDictionaryErrorWriteErrorNonListMatchDestField | AuditsSharedDictionaryErrorWriteErrorNonSecureContext | AuditsSharedDictionaryErrorWriteErrorNonStringIdField | AuditsSharedDictionaryErrorWriteErrorNonStringInMatchDestList | AuditsSharedDictionaryErrorWriteErrorInvalidMatchDestList | AuditsSharedDictionaryErrorWriteErrorNonStringMatchField | AuditsSharedDictionaryErrorWriteErrorNonTokenTypeField | AuditsSharedDictionaryErrorWriteErrorRequestAborted | AuditsSharedDictionaryErrorWriteErrorShuttingDown | AuditsSharedDictionaryErrorWriteErrorTooLongIdField | AuditsSharedDictionaryErrorWriteErrorUnsupportedType
  deriving (Ord, Eq, Show, Read)
instance FromJSON AuditsSharedDictionaryError where
  parseJSON = A.withText "AuditsSharedDictionaryError" $ \v -> case v of
    "UseErrorCrossOriginNoCorsRequest" -> pure AuditsSharedDictionaryErrorUseErrorCrossOriginNoCorsRequest
    "UseErrorDictionaryLoadFailure" -> pure AuditsSharedDictionaryErrorUseErrorDictionaryLoadFailure
    "UseErrorMatchingDictionaryNotUsed" -> pure AuditsSharedDictionaryErrorUseErrorMatchingDictionaryNotUsed
    "UseErrorUnexpectedContentDictionaryHeader" -> pure AuditsSharedDictionaryErrorUseErrorUnexpectedContentDictionaryHeader
    "WriteErrorCossOriginNoCorsRequest" -> pure AuditsSharedDictionaryErrorWriteErrorCossOriginNoCorsRequest
    "WriteErrorDisallowedBySettings" -> pure AuditsSharedDictionaryErrorWriteErrorDisallowedBySettings
    "WriteErrorExpiredResponse" -> pure AuditsSharedDictionaryErrorWriteErrorExpiredResponse
    "WriteErrorFeatureDisabled" -> pure AuditsSharedDictionaryErrorWriteErrorFeatureDisabled
    "WriteErrorInsufficientResources" -> pure AuditsSharedDictionaryErrorWriteErrorInsufficientResources
    "WriteErrorInvalidMatchField" -> pure AuditsSharedDictionaryErrorWriteErrorInvalidMatchField
    "WriteErrorInvalidStructuredHeader" -> pure AuditsSharedDictionaryErrorWriteErrorInvalidStructuredHeader
    "WriteErrorInvalidTTLField" -> pure AuditsSharedDictionaryErrorWriteErrorInvalidTTLField
    "WriteErrorNavigationRequest" -> pure AuditsSharedDictionaryErrorWriteErrorNavigationRequest
    "WriteErrorNoMatchField" -> pure AuditsSharedDictionaryErrorWriteErrorNoMatchField
    "WriteErrorNonIntegerTTLField" -> pure AuditsSharedDictionaryErrorWriteErrorNonIntegerTTLField
    "WriteErrorNonListMatchDestField" -> pure AuditsSharedDictionaryErrorWriteErrorNonListMatchDestField
    "WriteErrorNonSecureContext" -> pure AuditsSharedDictionaryErrorWriteErrorNonSecureContext
    "WriteErrorNonStringIdField" -> pure AuditsSharedDictionaryErrorWriteErrorNonStringIdField
    "WriteErrorNonStringInMatchDestList" -> pure AuditsSharedDictionaryErrorWriteErrorNonStringInMatchDestList
    "WriteErrorInvalidMatchDestList" -> pure AuditsSharedDictionaryErrorWriteErrorInvalidMatchDestList
    "WriteErrorNonStringMatchField" -> pure AuditsSharedDictionaryErrorWriteErrorNonStringMatchField
    "WriteErrorNonTokenTypeField" -> pure AuditsSharedDictionaryErrorWriteErrorNonTokenTypeField
    "WriteErrorRequestAborted" -> pure AuditsSharedDictionaryErrorWriteErrorRequestAborted
    "WriteErrorShuttingDown" -> pure AuditsSharedDictionaryErrorWriteErrorShuttingDown
    "WriteErrorTooLongIdField" -> pure AuditsSharedDictionaryErrorWriteErrorTooLongIdField
    "WriteErrorUnsupportedType" -> pure AuditsSharedDictionaryErrorWriteErrorUnsupportedType
    "_" -> fail "failed to parse AuditsSharedDictionaryError"
instance ToJSON AuditsSharedDictionaryError where
  toJSON v = A.String $ case v of
    AuditsSharedDictionaryErrorUseErrorCrossOriginNoCorsRequest -> "UseErrorCrossOriginNoCorsRequest"
    AuditsSharedDictionaryErrorUseErrorDictionaryLoadFailure -> "UseErrorDictionaryLoadFailure"
    AuditsSharedDictionaryErrorUseErrorMatchingDictionaryNotUsed -> "UseErrorMatchingDictionaryNotUsed"
    AuditsSharedDictionaryErrorUseErrorUnexpectedContentDictionaryHeader -> "UseErrorUnexpectedContentDictionaryHeader"
    AuditsSharedDictionaryErrorWriteErrorCossOriginNoCorsRequest -> "WriteErrorCossOriginNoCorsRequest"
    AuditsSharedDictionaryErrorWriteErrorDisallowedBySettings -> "WriteErrorDisallowedBySettings"
    AuditsSharedDictionaryErrorWriteErrorExpiredResponse -> "WriteErrorExpiredResponse"
    AuditsSharedDictionaryErrorWriteErrorFeatureDisabled -> "WriteErrorFeatureDisabled"
    AuditsSharedDictionaryErrorWriteErrorInsufficientResources -> "WriteErrorInsufficientResources"
    AuditsSharedDictionaryErrorWriteErrorInvalidMatchField -> "WriteErrorInvalidMatchField"
    AuditsSharedDictionaryErrorWriteErrorInvalidStructuredHeader -> "WriteErrorInvalidStructuredHeader"
    AuditsSharedDictionaryErrorWriteErrorInvalidTTLField -> "WriteErrorInvalidTTLField"
    AuditsSharedDictionaryErrorWriteErrorNavigationRequest -> "WriteErrorNavigationRequest"
    AuditsSharedDictionaryErrorWriteErrorNoMatchField -> "WriteErrorNoMatchField"
    AuditsSharedDictionaryErrorWriteErrorNonIntegerTTLField -> "WriteErrorNonIntegerTTLField"
    AuditsSharedDictionaryErrorWriteErrorNonListMatchDestField -> "WriteErrorNonListMatchDestField"
    AuditsSharedDictionaryErrorWriteErrorNonSecureContext -> "WriteErrorNonSecureContext"
    AuditsSharedDictionaryErrorWriteErrorNonStringIdField -> "WriteErrorNonStringIdField"
    AuditsSharedDictionaryErrorWriteErrorNonStringInMatchDestList -> "WriteErrorNonStringInMatchDestList"
    AuditsSharedDictionaryErrorWriteErrorInvalidMatchDestList -> "WriteErrorInvalidMatchDestList"
    AuditsSharedDictionaryErrorWriteErrorNonStringMatchField -> "WriteErrorNonStringMatchField"
    AuditsSharedDictionaryErrorWriteErrorNonTokenTypeField -> "WriteErrorNonTokenTypeField"
    AuditsSharedDictionaryErrorWriteErrorRequestAborted -> "WriteErrorRequestAborted"
    AuditsSharedDictionaryErrorWriteErrorShuttingDown -> "WriteErrorShuttingDown"
    AuditsSharedDictionaryErrorWriteErrorTooLongIdField -> "WriteErrorTooLongIdField"
    AuditsSharedDictionaryErrorWriteErrorUnsupportedType -> "WriteErrorUnsupportedType"

-- | Type 'Audits.SRIMessageSignatureError'.
data AuditsSRIMessageSignatureError = AuditsSRIMessageSignatureErrorMissingSignatureHeader | AuditsSRIMessageSignatureErrorMissingSignatureInputHeader | AuditsSRIMessageSignatureErrorInvalidSignatureHeader | AuditsSRIMessageSignatureErrorInvalidSignatureInputHeader | AuditsSRIMessageSignatureErrorSignatureHeaderValueIsNotByteSequence | AuditsSRIMessageSignatureErrorSignatureHeaderValueIsParameterized | AuditsSRIMessageSignatureErrorSignatureHeaderValueIsIncorrectLength | AuditsSRIMessageSignatureErrorSignatureInputHeaderMissingLabel | AuditsSRIMessageSignatureErrorSignatureInputHeaderValueNotInnerList | AuditsSRIMessageSignatureErrorSignatureInputHeaderValueMissingComponents | AuditsSRIMessageSignatureErrorSignatureInputHeaderInvalidComponentType | AuditsSRIMessageSignatureErrorSignatureInputHeaderInvalidComponentName | AuditsSRIMessageSignatureErrorSignatureInputHeaderInvalidHeaderComponentParameter | AuditsSRIMessageSignatureErrorSignatureInputHeaderInvalidDerivedComponentParameter | AuditsSRIMessageSignatureErrorSignatureInputHeaderKeyIdLength | AuditsSRIMessageSignatureErrorSignatureInputHeaderInvalidParameter | AuditsSRIMessageSignatureErrorSignatureInputHeaderMissingRequiredParameters | AuditsSRIMessageSignatureErrorValidationFailedSignatureExpired | AuditsSRIMessageSignatureErrorValidationFailedInvalidLength | AuditsSRIMessageSignatureErrorValidationFailedSignatureMismatch | AuditsSRIMessageSignatureErrorValidationFailedIntegrityMismatch | AuditsSRIMessageSignatureErrorSignatureBaseUnknownDerivedComponent | AuditsSRIMessageSignatureErrorSignatureBaseMissingHeader | AuditsSRIMessageSignatureErrorSignatureBaseInvalidUnencodedDigest | AuditsSRIMessageSignatureErrorSignatureBaseUnsupportedComponent
  deriving (Ord, Eq, Show, Read)
instance FromJSON AuditsSRIMessageSignatureError where
  parseJSON = A.withText "AuditsSRIMessageSignatureError" $ \v -> case v of
    "MissingSignatureHeader" -> pure AuditsSRIMessageSignatureErrorMissingSignatureHeader
    "MissingSignatureInputHeader" -> pure AuditsSRIMessageSignatureErrorMissingSignatureInputHeader
    "InvalidSignatureHeader" -> pure AuditsSRIMessageSignatureErrorInvalidSignatureHeader
    "InvalidSignatureInputHeader" -> pure AuditsSRIMessageSignatureErrorInvalidSignatureInputHeader
    "SignatureHeaderValueIsNotByteSequence" -> pure AuditsSRIMessageSignatureErrorSignatureHeaderValueIsNotByteSequence
    "SignatureHeaderValueIsParameterized" -> pure AuditsSRIMessageSignatureErrorSignatureHeaderValueIsParameterized
    "SignatureHeaderValueIsIncorrectLength" -> pure AuditsSRIMessageSignatureErrorSignatureHeaderValueIsIncorrectLength
    "SignatureInputHeaderMissingLabel" -> pure AuditsSRIMessageSignatureErrorSignatureInputHeaderMissingLabel
    "SignatureInputHeaderValueNotInnerList" -> pure AuditsSRIMessageSignatureErrorSignatureInputHeaderValueNotInnerList
    "SignatureInputHeaderValueMissingComponents" -> pure AuditsSRIMessageSignatureErrorSignatureInputHeaderValueMissingComponents
    "SignatureInputHeaderInvalidComponentType" -> pure AuditsSRIMessageSignatureErrorSignatureInputHeaderInvalidComponentType
    "SignatureInputHeaderInvalidComponentName" -> pure AuditsSRIMessageSignatureErrorSignatureInputHeaderInvalidComponentName
    "SignatureInputHeaderInvalidHeaderComponentParameter" -> pure AuditsSRIMessageSignatureErrorSignatureInputHeaderInvalidHeaderComponentParameter
    "SignatureInputHeaderInvalidDerivedComponentParameter" -> pure AuditsSRIMessageSignatureErrorSignatureInputHeaderInvalidDerivedComponentParameter
    "SignatureInputHeaderKeyIdLength" -> pure AuditsSRIMessageSignatureErrorSignatureInputHeaderKeyIdLength
    "SignatureInputHeaderInvalidParameter" -> pure AuditsSRIMessageSignatureErrorSignatureInputHeaderInvalidParameter
    "SignatureInputHeaderMissingRequiredParameters" -> pure AuditsSRIMessageSignatureErrorSignatureInputHeaderMissingRequiredParameters
    "ValidationFailedSignatureExpired" -> pure AuditsSRIMessageSignatureErrorValidationFailedSignatureExpired
    "ValidationFailedInvalidLength" -> pure AuditsSRIMessageSignatureErrorValidationFailedInvalidLength
    "ValidationFailedSignatureMismatch" -> pure AuditsSRIMessageSignatureErrorValidationFailedSignatureMismatch
    "ValidationFailedIntegrityMismatch" -> pure AuditsSRIMessageSignatureErrorValidationFailedIntegrityMismatch
    "SignatureBaseUnknownDerivedComponent" -> pure AuditsSRIMessageSignatureErrorSignatureBaseUnknownDerivedComponent
    "SignatureBaseMissingHeader" -> pure AuditsSRIMessageSignatureErrorSignatureBaseMissingHeader
    "SignatureBaseInvalidUnencodedDigest" -> pure AuditsSRIMessageSignatureErrorSignatureBaseInvalidUnencodedDigest
    "SignatureBaseUnsupportedComponent" -> pure AuditsSRIMessageSignatureErrorSignatureBaseUnsupportedComponent
    "_" -> fail "failed to parse AuditsSRIMessageSignatureError"
instance ToJSON AuditsSRIMessageSignatureError where
  toJSON v = A.String $ case v of
    AuditsSRIMessageSignatureErrorMissingSignatureHeader -> "MissingSignatureHeader"
    AuditsSRIMessageSignatureErrorMissingSignatureInputHeader -> "MissingSignatureInputHeader"
    AuditsSRIMessageSignatureErrorInvalidSignatureHeader -> "InvalidSignatureHeader"
    AuditsSRIMessageSignatureErrorInvalidSignatureInputHeader -> "InvalidSignatureInputHeader"
    AuditsSRIMessageSignatureErrorSignatureHeaderValueIsNotByteSequence -> "SignatureHeaderValueIsNotByteSequence"
    AuditsSRIMessageSignatureErrorSignatureHeaderValueIsParameterized -> "SignatureHeaderValueIsParameterized"
    AuditsSRIMessageSignatureErrorSignatureHeaderValueIsIncorrectLength -> "SignatureHeaderValueIsIncorrectLength"
    AuditsSRIMessageSignatureErrorSignatureInputHeaderMissingLabel -> "SignatureInputHeaderMissingLabel"
    AuditsSRIMessageSignatureErrorSignatureInputHeaderValueNotInnerList -> "SignatureInputHeaderValueNotInnerList"
    AuditsSRIMessageSignatureErrorSignatureInputHeaderValueMissingComponents -> "SignatureInputHeaderValueMissingComponents"
    AuditsSRIMessageSignatureErrorSignatureInputHeaderInvalidComponentType -> "SignatureInputHeaderInvalidComponentType"
    AuditsSRIMessageSignatureErrorSignatureInputHeaderInvalidComponentName -> "SignatureInputHeaderInvalidComponentName"
    AuditsSRIMessageSignatureErrorSignatureInputHeaderInvalidHeaderComponentParameter -> "SignatureInputHeaderInvalidHeaderComponentParameter"
    AuditsSRIMessageSignatureErrorSignatureInputHeaderInvalidDerivedComponentParameter -> "SignatureInputHeaderInvalidDerivedComponentParameter"
    AuditsSRIMessageSignatureErrorSignatureInputHeaderKeyIdLength -> "SignatureInputHeaderKeyIdLength"
    AuditsSRIMessageSignatureErrorSignatureInputHeaderInvalidParameter -> "SignatureInputHeaderInvalidParameter"
    AuditsSRIMessageSignatureErrorSignatureInputHeaderMissingRequiredParameters -> "SignatureInputHeaderMissingRequiredParameters"
    AuditsSRIMessageSignatureErrorValidationFailedSignatureExpired -> "ValidationFailedSignatureExpired"
    AuditsSRIMessageSignatureErrorValidationFailedInvalidLength -> "ValidationFailedInvalidLength"
    AuditsSRIMessageSignatureErrorValidationFailedSignatureMismatch -> "ValidationFailedSignatureMismatch"
    AuditsSRIMessageSignatureErrorValidationFailedIntegrityMismatch -> "ValidationFailedIntegrityMismatch"
    AuditsSRIMessageSignatureErrorSignatureBaseUnknownDerivedComponent -> "SignatureBaseUnknownDerivedComponent"
    AuditsSRIMessageSignatureErrorSignatureBaseMissingHeader -> "SignatureBaseMissingHeader"
    AuditsSRIMessageSignatureErrorSignatureBaseInvalidUnencodedDigest -> "SignatureBaseInvalidUnencodedDigest"
    AuditsSRIMessageSignatureErrorSignatureBaseUnsupportedComponent -> "SignatureBaseUnsupportedComponent"

-- | Type 'Audits.UnencodedDigestError'.
data AuditsUnencodedDigestError = AuditsUnencodedDigestErrorMalformedDictionary | AuditsUnencodedDigestErrorUnknownAlgorithm | AuditsUnencodedDigestErrorIncorrectDigestType | AuditsUnencodedDigestErrorIncorrectDigestLength
  deriving (Ord, Eq, Show, Read)
instance FromJSON AuditsUnencodedDigestError where
  parseJSON = A.withText "AuditsUnencodedDigestError" $ \v -> case v of
    "MalformedDictionary" -> pure AuditsUnencodedDigestErrorMalformedDictionary
    "UnknownAlgorithm" -> pure AuditsUnencodedDigestErrorUnknownAlgorithm
    "IncorrectDigestType" -> pure AuditsUnencodedDigestErrorIncorrectDigestType
    "IncorrectDigestLength" -> pure AuditsUnencodedDigestErrorIncorrectDigestLength
    "_" -> fail "failed to parse AuditsUnencodedDigestError"
instance ToJSON AuditsUnencodedDigestError where
  toJSON v = A.String $ case v of
    AuditsUnencodedDigestErrorMalformedDictionary -> "MalformedDictionary"
    AuditsUnencodedDigestErrorUnknownAlgorithm -> "UnknownAlgorithm"
    AuditsUnencodedDigestErrorIncorrectDigestType -> "IncorrectDigestType"
    AuditsUnencodedDigestErrorIncorrectDigestLength -> "IncorrectDigestLength"

-- | Type 'Audits.ConnectionAllowlistError'.
data AuditsConnectionAllowlistError = AuditsConnectionAllowlistErrorInvalidHeader | AuditsConnectionAllowlistErrorMoreThanOneList | AuditsConnectionAllowlistErrorItemNotInnerList | AuditsConnectionAllowlistErrorInvalidAllowlistItemType | AuditsConnectionAllowlistErrorReportingEndpointNotToken | AuditsConnectionAllowlistErrorInvalidUrlPattern
  deriving (Ord, Eq, Show, Read)
instance FromJSON AuditsConnectionAllowlistError where
  parseJSON = A.withText "AuditsConnectionAllowlistError" $ \v -> case v of
    "InvalidHeader" -> pure AuditsConnectionAllowlistErrorInvalidHeader
    "MoreThanOneList" -> pure AuditsConnectionAllowlistErrorMoreThanOneList
    "ItemNotInnerList" -> pure AuditsConnectionAllowlistErrorItemNotInnerList
    "InvalidAllowlistItemType" -> pure AuditsConnectionAllowlistErrorInvalidAllowlistItemType
    "ReportingEndpointNotToken" -> pure AuditsConnectionAllowlistErrorReportingEndpointNotToken
    "InvalidUrlPattern" -> pure AuditsConnectionAllowlistErrorInvalidUrlPattern
    "_" -> fail "failed to parse AuditsConnectionAllowlistError"
instance ToJSON AuditsConnectionAllowlistError where
  toJSON v = A.String $ case v of
    AuditsConnectionAllowlistErrorInvalidHeader -> "InvalidHeader"
    AuditsConnectionAllowlistErrorMoreThanOneList -> "MoreThanOneList"
    AuditsConnectionAllowlistErrorItemNotInnerList -> "ItemNotInnerList"
    AuditsConnectionAllowlistErrorInvalidAllowlistItemType -> "InvalidAllowlistItemType"
    AuditsConnectionAllowlistErrorReportingEndpointNotToken -> "ReportingEndpointNotToken"
    AuditsConnectionAllowlistErrorInvalidUrlPattern -> "InvalidUrlPattern"

-- | Type 'Audits.QuirksModeIssueDetails'.
--   Details for issues about documents in Quirks Mode
--   or Limited Quirks Mode that affects page layouting.
data AuditsQuirksModeIssueDetails = AuditsQuirksModeIssueDetails
  {
    -- | If false, it means the document's mode is "quirks"
    --   instead of "limited-quirks".
    auditsQuirksModeIssueDetailsIsLimitedQuirksMode :: Bool,
    auditsQuirksModeIssueDetailsDocumentNodeId :: DOMNetworkEmulationPageSecurity.DOMBackendNodeId,
    auditsQuirksModeIssueDetailsUrl :: T.Text,
    auditsQuirksModeIssueDetailsFrameId :: DOMNetworkEmulationPageSecurity.PageFrameId,
    auditsQuirksModeIssueDetailsLoaderId :: DOMNetworkEmulationPageSecurity.NetworkLoaderId
  }
  deriving (Eq, Show)
instance FromJSON AuditsQuirksModeIssueDetails where
  parseJSON = A.withObject "AuditsQuirksModeIssueDetails" $ \o -> AuditsQuirksModeIssueDetails
    <$> o A..: "isLimitedQuirksMode"
    <*> o A..: "documentNodeId"
    <*> o A..: "url"
    <*> o A..: "frameId"
    <*> o A..: "loaderId"
instance ToJSON AuditsQuirksModeIssueDetails where
  toJSON p = A.object $ catMaybes [
    ("isLimitedQuirksMode" A..=) <$> Just (auditsQuirksModeIssueDetailsIsLimitedQuirksMode p),
    ("documentNodeId" A..=) <$> Just (auditsQuirksModeIssueDetailsDocumentNodeId p),
    ("url" A..=) <$> Just (auditsQuirksModeIssueDetailsUrl p),
    ("frameId" A..=) <$> Just (auditsQuirksModeIssueDetailsFrameId p),
    ("loaderId" A..=) <$> Just (auditsQuirksModeIssueDetailsLoaderId p)
    ]

-- | Type 'Audits.SharedDictionaryIssueDetails'.
data AuditsSharedDictionaryIssueDetails = AuditsSharedDictionaryIssueDetails
  {
    auditsSharedDictionaryIssueDetailsSharedDictionaryError :: AuditsSharedDictionaryError,
    auditsSharedDictionaryIssueDetailsRequest :: AuditsAffectedRequest
  }
  deriving (Eq, Show)
instance FromJSON AuditsSharedDictionaryIssueDetails where
  parseJSON = A.withObject "AuditsSharedDictionaryIssueDetails" $ \o -> AuditsSharedDictionaryIssueDetails
    <$> o A..: "sharedDictionaryError"
    <*> o A..: "request"
instance ToJSON AuditsSharedDictionaryIssueDetails where
  toJSON p = A.object $ catMaybes [
    ("sharedDictionaryError" A..=) <$> Just (auditsSharedDictionaryIssueDetailsSharedDictionaryError p),
    ("request" A..=) <$> Just (auditsSharedDictionaryIssueDetailsRequest p)
    ]

-- | Type 'Audits.SRIMessageSignatureIssueDetails'.
data AuditsSRIMessageSignatureIssueDetails = AuditsSRIMessageSignatureIssueDetails
  {
    auditsSRIMessageSignatureIssueDetailsError :: AuditsSRIMessageSignatureError,
    auditsSRIMessageSignatureIssueDetailsSignatureBase :: T.Text,
    auditsSRIMessageSignatureIssueDetailsIntegrityAssertions :: [T.Text],
    auditsSRIMessageSignatureIssueDetailsRequest :: AuditsAffectedRequest
  }
  deriving (Eq, Show)
instance FromJSON AuditsSRIMessageSignatureIssueDetails where
  parseJSON = A.withObject "AuditsSRIMessageSignatureIssueDetails" $ \o -> AuditsSRIMessageSignatureIssueDetails
    <$> o A..: "error"
    <*> o A..: "signatureBase"
    <*> o A..: "integrityAssertions"
    <*> o A..: "request"
instance ToJSON AuditsSRIMessageSignatureIssueDetails where
  toJSON p = A.object $ catMaybes [
    ("error" A..=) <$> Just (auditsSRIMessageSignatureIssueDetailsError p),
    ("signatureBase" A..=) <$> Just (auditsSRIMessageSignatureIssueDetailsSignatureBase p),
    ("integrityAssertions" A..=) <$> Just (auditsSRIMessageSignatureIssueDetailsIntegrityAssertions p),
    ("request" A..=) <$> Just (auditsSRIMessageSignatureIssueDetailsRequest p)
    ]

-- | Type 'Audits.UnencodedDigestIssueDetails'.
data AuditsUnencodedDigestIssueDetails = AuditsUnencodedDigestIssueDetails
  {
    auditsUnencodedDigestIssueDetailsError :: AuditsUnencodedDigestError,
    auditsUnencodedDigestIssueDetailsRequest :: AuditsAffectedRequest
  }
  deriving (Eq, Show)
instance FromJSON AuditsUnencodedDigestIssueDetails where
  parseJSON = A.withObject "AuditsUnencodedDigestIssueDetails" $ \o -> AuditsUnencodedDigestIssueDetails
    <$> o A..: "error"
    <*> o A..: "request"
instance ToJSON AuditsUnencodedDigestIssueDetails where
  toJSON p = A.object $ catMaybes [
    ("error" A..=) <$> Just (auditsUnencodedDigestIssueDetailsError p),
    ("request" A..=) <$> Just (auditsUnencodedDigestIssueDetailsRequest p)
    ]

-- | Type 'Audits.ConnectionAllowlistIssueDetails'.
data AuditsConnectionAllowlistIssueDetails = AuditsConnectionAllowlistIssueDetails
  {
    auditsConnectionAllowlistIssueDetailsError :: AuditsConnectionAllowlistError,
    auditsConnectionAllowlistIssueDetailsRequest :: AuditsAffectedRequest
  }
  deriving (Eq, Show)
instance FromJSON AuditsConnectionAllowlistIssueDetails where
  parseJSON = A.withObject "AuditsConnectionAllowlistIssueDetails" $ \o -> AuditsConnectionAllowlistIssueDetails
    <$> o A..: "error"
    <*> o A..: "request"
instance ToJSON AuditsConnectionAllowlistIssueDetails where
  toJSON p = A.object $ catMaybes [
    ("error" A..=) <$> Just (auditsConnectionAllowlistIssueDetailsError p),
    ("request" A..=) <$> Just (auditsConnectionAllowlistIssueDetailsRequest p)
    ]

-- | Type 'Audits.GenericIssueErrorType'.
data AuditsGenericIssueErrorType = AuditsGenericIssueErrorTypeFormLabelForNameError | AuditsGenericIssueErrorTypeFormDuplicateIdForInputError | AuditsGenericIssueErrorTypeFormInputWithNoLabelError | AuditsGenericIssueErrorTypeFormAutocompleteAttributeEmptyError | AuditsGenericIssueErrorTypeFormEmptyIdAndNameAttributesForInputError | AuditsGenericIssueErrorTypeFormAriaLabelledByToNonExistingIdError | AuditsGenericIssueErrorTypeFormInputAssignedAutocompleteValueToIdOrNameAttributeError | AuditsGenericIssueErrorTypeFormLabelHasNeitherForNorNestedInputError | AuditsGenericIssueErrorTypeFormLabelForMatchesNonExistingIdError | AuditsGenericIssueErrorTypeFormInputHasWrongButWellIntendedAutocompleteValueError | AuditsGenericIssueErrorTypeResponseWasBlockedByORB | AuditsGenericIssueErrorTypeNavigationEntryMarkedSkippable | AuditsGenericIssueErrorTypeBackUINavigationWouldSkipAd | AuditsGenericIssueErrorTypeAutofillAndManualTextPolicyControlledFeaturesInfo | AuditsGenericIssueErrorTypeAutofillPolicyControlledFeatureInfo | AuditsGenericIssueErrorTypeManualTextPolicyControlledFeatureInfo | AuditsGenericIssueErrorTypeFormModelContextParameterMissingTitleAndDescription | AuditsGenericIssueErrorTypeFormModelContextMissingToolName | AuditsGenericIssueErrorTypeFormModelContextMissingToolDescription | AuditsGenericIssueErrorTypeFormModelContextRequiredParameterMissingName | AuditsGenericIssueErrorTypeFormModelContextParameterMissingName
  deriving (Ord, Eq, Show, Read)
instance FromJSON AuditsGenericIssueErrorType where
  parseJSON = A.withText "AuditsGenericIssueErrorType" $ \v -> case v of
    "FormLabelForNameError" -> pure AuditsGenericIssueErrorTypeFormLabelForNameError
    "FormDuplicateIdForInputError" -> pure AuditsGenericIssueErrorTypeFormDuplicateIdForInputError
    "FormInputWithNoLabelError" -> pure AuditsGenericIssueErrorTypeFormInputWithNoLabelError
    "FormAutocompleteAttributeEmptyError" -> pure AuditsGenericIssueErrorTypeFormAutocompleteAttributeEmptyError
    "FormEmptyIdAndNameAttributesForInputError" -> pure AuditsGenericIssueErrorTypeFormEmptyIdAndNameAttributesForInputError
    "FormAriaLabelledByToNonExistingIdError" -> pure AuditsGenericIssueErrorTypeFormAriaLabelledByToNonExistingIdError
    "FormInputAssignedAutocompleteValueToIdOrNameAttributeError" -> pure AuditsGenericIssueErrorTypeFormInputAssignedAutocompleteValueToIdOrNameAttributeError
    "FormLabelHasNeitherForNorNestedInputError" -> pure AuditsGenericIssueErrorTypeFormLabelHasNeitherForNorNestedInputError
    "FormLabelForMatchesNonExistingIdError" -> pure AuditsGenericIssueErrorTypeFormLabelForMatchesNonExistingIdError
    "FormInputHasWrongButWellIntendedAutocompleteValueError" -> pure AuditsGenericIssueErrorTypeFormInputHasWrongButWellIntendedAutocompleteValueError
    "ResponseWasBlockedByORB" -> pure AuditsGenericIssueErrorTypeResponseWasBlockedByORB
    "NavigationEntryMarkedSkippable" -> pure AuditsGenericIssueErrorTypeNavigationEntryMarkedSkippable
    "BackUINavigationWouldSkipAd" -> pure AuditsGenericIssueErrorTypeBackUINavigationWouldSkipAd
    "AutofillAndManualTextPolicyControlledFeaturesInfo" -> pure AuditsGenericIssueErrorTypeAutofillAndManualTextPolicyControlledFeaturesInfo
    "AutofillPolicyControlledFeatureInfo" -> pure AuditsGenericIssueErrorTypeAutofillPolicyControlledFeatureInfo
    "ManualTextPolicyControlledFeatureInfo" -> pure AuditsGenericIssueErrorTypeManualTextPolicyControlledFeatureInfo
    "FormModelContextParameterMissingTitleAndDescription" -> pure AuditsGenericIssueErrorTypeFormModelContextParameterMissingTitleAndDescription
    "FormModelContextMissingToolName" -> pure AuditsGenericIssueErrorTypeFormModelContextMissingToolName
    "FormModelContextMissingToolDescription" -> pure AuditsGenericIssueErrorTypeFormModelContextMissingToolDescription
    "FormModelContextRequiredParameterMissingName" -> pure AuditsGenericIssueErrorTypeFormModelContextRequiredParameterMissingName
    "FormModelContextParameterMissingName" -> pure AuditsGenericIssueErrorTypeFormModelContextParameterMissingName
    "_" -> fail "failed to parse AuditsGenericIssueErrorType"
instance ToJSON AuditsGenericIssueErrorType where
  toJSON v = A.String $ case v of
    AuditsGenericIssueErrorTypeFormLabelForNameError -> "FormLabelForNameError"
    AuditsGenericIssueErrorTypeFormDuplicateIdForInputError -> "FormDuplicateIdForInputError"
    AuditsGenericIssueErrorTypeFormInputWithNoLabelError -> "FormInputWithNoLabelError"
    AuditsGenericIssueErrorTypeFormAutocompleteAttributeEmptyError -> "FormAutocompleteAttributeEmptyError"
    AuditsGenericIssueErrorTypeFormEmptyIdAndNameAttributesForInputError -> "FormEmptyIdAndNameAttributesForInputError"
    AuditsGenericIssueErrorTypeFormAriaLabelledByToNonExistingIdError -> "FormAriaLabelledByToNonExistingIdError"
    AuditsGenericIssueErrorTypeFormInputAssignedAutocompleteValueToIdOrNameAttributeError -> "FormInputAssignedAutocompleteValueToIdOrNameAttributeError"
    AuditsGenericIssueErrorTypeFormLabelHasNeitherForNorNestedInputError -> "FormLabelHasNeitherForNorNestedInputError"
    AuditsGenericIssueErrorTypeFormLabelForMatchesNonExistingIdError -> "FormLabelForMatchesNonExistingIdError"
    AuditsGenericIssueErrorTypeFormInputHasWrongButWellIntendedAutocompleteValueError -> "FormInputHasWrongButWellIntendedAutocompleteValueError"
    AuditsGenericIssueErrorTypeResponseWasBlockedByORB -> "ResponseWasBlockedByORB"
    AuditsGenericIssueErrorTypeNavigationEntryMarkedSkippable -> "NavigationEntryMarkedSkippable"
    AuditsGenericIssueErrorTypeBackUINavigationWouldSkipAd -> "BackUINavigationWouldSkipAd"
    AuditsGenericIssueErrorTypeAutofillAndManualTextPolicyControlledFeaturesInfo -> "AutofillAndManualTextPolicyControlledFeaturesInfo"
    AuditsGenericIssueErrorTypeAutofillPolicyControlledFeatureInfo -> "AutofillPolicyControlledFeatureInfo"
    AuditsGenericIssueErrorTypeManualTextPolicyControlledFeatureInfo -> "ManualTextPolicyControlledFeatureInfo"
    AuditsGenericIssueErrorTypeFormModelContextParameterMissingTitleAndDescription -> "FormModelContextParameterMissingTitleAndDescription"
    AuditsGenericIssueErrorTypeFormModelContextMissingToolName -> "FormModelContextMissingToolName"
    AuditsGenericIssueErrorTypeFormModelContextMissingToolDescription -> "FormModelContextMissingToolDescription"
    AuditsGenericIssueErrorTypeFormModelContextRequiredParameterMissingName -> "FormModelContextRequiredParameterMissingName"
    AuditsGenericIssueErrorTypeFormModelContextParameterMissingName -> "FormModelContextParameterMissingName"

-- | Type 'Audits.GenericIssueDetails'.
--   Depending on the concrete errorType, different properties are set.
data AuditsGenericIssueDetails = AuditsGenericIssueDetails
  {
    -- | Issues with the same errorType are aggregated in the frontend.
    auditsGenericIssueDetailsErrorType :: AuditsGenericIssueErrorType,
    auditsGenericIssueDetailsFrameId :: Maybe DOMNetworkEmulationPageSecurity.PageFrameId,
    auditsGenericIssueDetailsViolatingNodeId :: Maybe DOMNetworkEmulationPageSecurity.DOMBackendNodeId,
    auditsGenericIssueDetailsViolatingNodeAttribute :: Maybe T.Text,
    auditsGenericIssueDetailsRequest :: Maybe AuditsAffectedRequest
  }
  deriving (Eq, Show)
instance FromJSON AuditsGenericIssueDetails where
  parseJSON = A.withObject "AuditsGenericIssueDetails" $ \o -> AuditsGenericIssueDetails
    <$> o A..: "errorType"
    <*> o A..:? "frameId"
    <*> o A..:? "violatingNodeId"
    <*> o A..:? "violatingNodeAttribute"
    <*> o A..:? "request"
instance ToJSON AuditsGenericIssueDetails where
  toJSON p = A.object $ catMaybes [
    ("errorType" A..=) <$> Just (auditsGenericIssueDetailsErrorType p),
    ("frameId" A..=) <$> (auditsGenericIssueDetailsFrameId p),
    ("violatingNodeId" A..=) <$> (auditsGenericIssueDetailsViolatingNodeId p),
    ("violatingNodeAttribute" A..=) <$> (auditsGenericIssueDetailsViolatingNodeAttribute p),
    ("request" A..=) <$> (auditsGenericIssueDetailsRequest p)
    ]

-- | Type 'Audits.DeprecationIssueDetails'.
--   This issue tracks information needed to print a deprecation message.
--   https://source.chromium.org/chromium/chromium/src/+/main:third_party/blink/renderer/core/frame/third_party/blink/renderer/core/frame/deprecation/README.md
data AuditsDeprecationIssueDetails = AuditsDeprecationIssueDetails
  {
    auditsDeprecationIssueDetailsAffectedFrame :: Maybe AuditsAffectedFrame,
    auditsDeprecationIssueDetailsSourceCodeLocation :: AuditsSourceCodeLocation,
    -- | One of the deprecation names from third_party/blink/renderer/core/frame/deprecation/deprecation.json5
    auditsDeprecationIssueDetailsType :: T.Text
  }
  deriving (Eq, Show)
instance FromJSON AuditsDeprecationIssueDetails where
  parseJSON = A.withObject "AuditsDeprecationIssueDetails" $ \o -> AuditsDeprecationIssueDetails
    <$> o A..:? "affectedFrame"
    <*> o A..: "sourceCodeLocation"
    <*> o A..: "type"
instance ToJSON AuditsDeprecationIssueDetails where
  toJSON p = A.object $ catMaybes [
    ("affectedFrame" A..=) <$> (auditsDeprecationIssueDetailsAffectedFrame p),
    ("sourceCodeLocation" A..=) <$> Just (auditsDeprecationIssueDetailsSourceCodeLocation p),
    ("type" A..=) <$> Just (auditsDeprecationIssueDetailsType p)
    ]

-- | Type 'Audits.BounceTrackingIssueDetails'.
--   This issue warns about sites in the redirect chain of a finished navigation
--   that may be flagged as trackers and have their state cleared if they don't
--   receive a user interaction. Note that in this context 'site' means eTLD+1.
--   For example, if the URL `https://example.test:80/bounce` was in the
--   redirect chain, the site reported would be `example.test`.
data AuditsBounceTrackingIssueDetails = AuditsBounceTrackingIssueDetails
  {
    auditsBounceTrackingIssueDetailsTrackingSites :: [T.Text]
  }
  deriving (Eq, Show)
instance FromJSON AuditsBounceTrackingIssueDetails where
  parseJSON = A.withObject "AuditsBounceTrackingIssueDetails" $ \o -> AuditsBounceTrackingIssueDetails
    <$> o A..: "trackingSites"
instance ToJSON AuditsBounceTrackingIssueDetails where
  toJSON p = A.object $ catMaybes [
    ("trackingSites" A..=) <$> Just (auditsBounceTrackingIssueDetailsTrackingSites p)
    ]

-- | Type 'Audits.CookieDeprecationMetadataIssueDetails'.
--   This issue warns about third-party sites that are accessing cookies on the
--   current page, and have been permitted due to having a global metadata grant.
--   Note that in this context 'site' means eTLD+1. For example, if the URL
--   `https://example.test:80/web_page` was accessing cookies, the site reported
--   would be `example.test`.
data AuditsCookieDeprecationMetadataIssueDetails = AuditsCookieDeprecationMetadataIssueDetails
  {
    auditsCookieDeprecationMetadataIssueDetailsAllowedSites :: [T.Text],
    auditsCookieDeprecationMetadataIssueDetailsOptOutPercentage :: Double,
    auditsCookieDeprecationMetadataIssueDetailsIsOptOutTopLevel :: Bool,
    auditsCookieDeprecationMetadataIssueDetailsOperation :: AuditsCookieOperation
  }
  deriving (Eq, Show)
instance FromJSON AuditsCookieDeprecationMetadataIssueDetails where
  parseJSON = A.withObject "AuditsCookieDeprecationMetadataIssueDetails" $ \o -> AuditsCookieDeprecationMetadataIssueDetails
    <$> o A..: "allowedSites"
    <*> o A..: "optOutPercentage"
    <*> o A..: "isOptOutTopLevel"
    <*> o A..: "operation"
instance ToJSON AuditsCookieDeprecationMetadataIssueDetails where
  toJSON p = A.object $ catMaybes [
    ("allowedSites" A..=) <$> Just (auditsCookieDeprecationMetadataIssueDetailsAllowedSites p),
    ("optOutPercentage" A..=) <$> Just (auditsCookieDeprecationMetadataIssueDetailsOptOutPercentage p),
    ("isOptOutTopLevel" A..=) <$> Just (auditsCookieDeprecationMetadataIssueDetailsIsOptOutTopLevel p),
    ("operation" A..=) <$> Just (auditsCookieDeprecationMetadataIssueDetailsOperation p)
    ]

-- | Type 'Audits.ClientHintIssueReason'.
data AuditsClientHintIssueReason = AuditsClientHintIssueReasonMetaTagAllowListInvalidOrigin | AuditsClientHintIssueReasonMetaTagModifiedHTML
  deriving (Ord, Eq, Show, Read)
instance FromJSON AuditsClientHintIssueReason where
  parseJSON = A.withText "AuditsClientHintIssueReason" $ \v -> case v of
    "MetaTagAllowListInvalidOrigin" -> pure AuditsClientHintIssueReasonMetaTagAllowListInvalidOrigin
    "MetaTagModifiedHTML" -> pure AuditsClientHintIssueReasonMetaTagModifiedHTML
    "_" -> fail "failed to parse AuditsClientHintIssueReason"
instance ToJSON AuditsClientHintIssueReason where
  toJSON v = A.String $ case v of
    AuditsClientHintIssueReasonMetaTagAllowListInvalidOrigin -> "MetaTagAllowListInvalidOrigin"
    AuditsClientHintIssueReasonMetaTagModifiedHTML -> "MetaTagModifiedHTML"

-- | Type 'Audits.FederatedAuthRequestIssueDetails'.
data AuditsFederatedAuthRequestIssueDetails = AuditsFederatedAuthRequestIssueDetails
  {
    auditsFederatedAuthRequestIssueDetailsFederatedAuthRequestIssueReason :: AuditsFederatedAuthRequestIssueReason
  }
  deriving (Eq, Show)
instance FromJSON AuditsFederatedAuthRequestIssueDetails where
  parseJSON = A.withObject "AuditsFederatedAuthRequestIssueDetails" $ \o -> AuditsFederatedAuthRequestIssueDetails
    <$> o A..: "federatedAuthRequestIssueReason"
instance ToJSON AuditsFederatedAuthRequestIssueDetails where
  toJSON p = A.object $ catMaybes [
    ("federatedAuthRequestIssueReason" A..=) <$> Just (auditsFederatedAuthRequestIssueDetailsFederatedAuthRequestIssueReason p)
    ]

-- | Type 'Audits.FederatedAuthRequestIssueReason'.
--   Represents the failure reason when a federated authentication reason fails.
--   Should be updated alongside RequestIdTokenStatus in
--   third_party/blink/public/mojom/devtools/inspector_issue.mojom to include
--   all cases except for success.
data AuditsFederatedAuthRequestIssueReason = AuditsFederatedAuthRequestIssueReasonShouldEmbargo | AuditsFederatedAuthRequestIssueReasonTooManyRequests | AuditsFederatedAuthRequestIssueReasonWellKnownHttpNotFound | AuditsFederatedAuthRequestIssueReasonWellKnownNoResponse | AuditsFederatedAuthRequestIssueReasonWellKnownBlockedByConnectionAllowlist | AuditsFederatedAuthRequestIssueReasonWellKnownInvalidResponse | AuditsFederatedAuthRequestIssueReasonWellKnownListEmpty | AuditsFederatedAuthRequestIssueReasonWellKnownInvalidContentType | AuditsFederatedAuthRequestIssueReasonConfigNotInWellKnown | AuditsFederatedAuthRequestIssueReasonWellKnownTooBig | AuditsFederatedAuthRequestIssueReasonConfigHttpNotFound | AuditsFederatedAuthRequestIssueReasonConfigNoResponse | AuditsFederatedAuthRequestIssueReasonConfigBlockedByConnectionAllowlist | AuditsFederatedAuthRequestIssueReasonConfigInvalidResponse | AuditsFederatedAuthRequestIssueReasonConfigInvalidContentType | AuditsFederatedAuthRequestIssueReasonIdpNotPotentiallyTrustworthy | AuditsFederatedAuthRequestIssueReasonDisabledInSettings | AuditsFederatedAuthRequestIssueReasonDisabledInFlags | AuditsFederatedAuthRequestIssueReasonErrorFetchingSignin | AuditsFederatedAuthRequestIssueReasonInvalidSigninResponse | AuditsFederatedAuthRequestIssueReasonAccountsHttpNotFound | AuditsFederatedAuthRequestIssueReasonAccountsNoResponse | AuditsFederatedAuthRequestIssueReasonAccountsBlockedByConnectionAllowlist | AuditsFederatedAuthRequestIssueReasonAccountsInvalidResponse | AuditsFederatedAuthRequestIssueReasonAccountsListEmpty | AuditsFederatedAuthRequestIssueReasonAccountsInvalidContentType | AuditsFederatedAuthRequestIssueReasonIdTokenHttpNotFound | AuditsFederatedAuthRequestIssueReasonIdTokenNoResponse | AuditsFederatedAuthRequestIssueReasonIdTokenBlockedByConnectionAllowlist | AuditsFederatedAuthRequestIssueReasonIdTokenInvalidResponse | AuditsFederatedAuthRequestIssueReasonIdTokenIdpErrorResponse | AuditsFederatedAuthRequestIssueReasonIdTokenCrossSiteIdpErrorResponse | AuditsFederatedAuthRequestIssueReasonIdTokenInvalidRequest | AuditsFederatedAuthRequestIssueReasonIdTokenInvalidContentType | AuditsFederatedAuthRequestIssueReasonErrorIdToken | AuditsFederatedAuthRequestIssueReasonCanceled | AuditsFederatedAuthRequestIssueReasonRpPageNotVisible | AuditsFederatedAuthRequestIssueReasonSilentMediationFailure | AuditsFederatedAuthRequestIssueReasonNotSignedInWithIdp | AuditsFederatedAuthRequestIssueReasonMissingTransientUserActivation | AuditsFederatedAuthRequestIssueReasonReplacedByActiveMode | AuditsFederatedAuthRequestIssueReasonRelyingPartyOriginIsOpaque | AuditsFederatedAuthRequestIssueReasonTypeNotMatching | AuditsFederatedAuthRequestIssueReasonUiDismissedNoEmbargo | AuditsFederatedAuthRequestIssueReasonCorsError | AuditsFederatedAuthRequestIssueReasonSuppressedBySegmentationPlatform
  deriving (Ord, Eq, Show, Read)
instance FromJSON AuditsFederatedAuthRequestIssueReason where
  parseJSON = A.withText "AuditsFederatedAuthRequestIssueReason" $ \v -> case v of
    "ShouldEmbargo" -> pure AuditsFederatedAuthRequestIssueReasonShouldEmbargo
    "TooManyRequests" -> pure AuditsFederatedAuthRequestIssueReasonTooManyRequests
    "WellKnownHttpNotFound" -> pure AuditsFederatedAuthRequestIssueReasonWellKnownHttpNotFound
    "WellKnownNoResponse" -> pure AuditsFederatedAuthRequestIssueReasonWellKnownNoResponse
    "WellKnownBlockedByConnectionAllowlist" -> pure AuditsFederatedAuthRequestIssueReasonWellKnownBlockedByConnectionAllowlist
    "WellKnownInvalidResponse" -> pure AuditsFederatedAuthRequestIssueReasonWellKnownInvalidResponse
    "WellKnownListEmpty" -> pure AuditsFederatedAuthRequestIssueReasonWellKnownListEmpty
    "WellKnownInvalidContentType" -> pure AuditsFederatedAuthRequestIssueReasonWellKnownInvalidContentType
    "ConfigNotInWellKnown" -> pure AuditsFederatedAuthRequestIssueReasonConfigNotInWellKnown
    "WellKnownTooBig" -> pure AuditsFederatedAuthRequestIssueReasonWellKnownTooBig
    "ConfigHttpNotFound" -> pure AuditsFederatedAuthRequestIssueReasonConfigHttpNotFound
    "ConfigNoResponse" -> pure AuditsFederatedAuthRequestIssueReasonConfigNoResponse
    "ConfigBlockedByConnectionAllowlist" -> pure AuditsFederatedAuthRequestIssueReasonConfigBlockedByConnectionAllowlist
    "ConfigInvalidResponse" -> pure AuditsFederatedAuthRequestIssueReasonConfigInvalidResponse
    "ConfigInvalidContentType" -> pure AuditsFederatedAuthRequestIssueReasonConfigInvalidContentType
    "IdpNotPotentiallyTrustworthy" -> pure AuditsFederatedAuthRequestIssueReasonIdpNotPotentiallyTrustworthy
    "DisabledInSettings" -> pure AuditsFederatedAuthRequestIssueReasonDisabledInSettings
    "DisabledInFlags" -> pure AuditsFederatedAuthRequestIssueReasonDisabledInFlags
    "ErrorFetchingSignin" -> pure AuditsFederatedAuthRequestIssueReasonErrorFetchingSignin
    "InvalidSigninResponse" -> pure AuditsFederatedAuthRequestIssueReasonInvalidSigninResponse
    "AccountsHttpNotFound" -> pure AuditsFederatedAuthRequestIssueReasonAccountsHttpNotFound
    "AccountsNoResponse" -> pure AuditsFederatedAuthRequestIssueReasonAccountsNoResponse
    "AccountsBlockedByConnectionAllowlist" -> pure AuditsFederatedAuthRequestIssueReasonAccountsBlockedByConnectionAllowlist
    "AccountsInvalidResponse" -> pure AuditsFederatedAuthRequestIssueReasonAccountsInvalidResponse
    "AccountsListEmpty" -> pure AuditsFederatedAuthRequestIssueReasonAccountsListEmpty
    "AccountsInvalidContentType" -> pure AuditsFederatedAuthRequestIssueReasonAccountsInvalidContentType
    "IdTokenHttpNotFound" -> pure AuditsFederatedAuthRequestIssueReasonIdTokenHttpNotFound
    "IdTokenNoResponse" -> pure AuditsFederatedAuthRequestIssueReasonIdTokenNoResponse
    "IdTokenBlockedByConnectionAllowlist" -> pure AuditsFederatedAuthRequestIssueReasonIdTokenBlockedByConnectionAllowlist
    "IdTokenInvalidResponse" -> pure AuditsFederatedAuthRequestIssueReasonIdTokenInvalidResponse
    "IdTokenIdpErrorResponse" -> pure AuditsFederatedAuthRequestIssueReasonIdTokenIdpErrorResponse
    "IdTokenCrossSiteIdpErrorResponse" -> pure AuditsFederatedAuthRequestIssueReasonIdTokenCrossSiteIdpErrorResponse
    "IdTokenInvalidRequest" -> pure AuditsFederatedAuthRequestIssueReasonIdTokenInvalidRequest
    "IdTokenInvalidContentType" -> pure AuditsFederatedAuthRequestIssueReasonIdTokenInvalidContentType
    "ErrorIdToken" -> pure AuditsFederatedAuthRequestIssueReasonErrorIdToken
    "Canceled" -> pure AuditsFederatedAuthRequestIssueReasonCanceled
    "RpPageNotVisible" -> pure AuditsFederatedAuthRequestIssueReasonRpPageNotVisible
    "SilentMediationFailure" -> pure AuditsFederatedAuthRequestIssueReasonSilentMediationFailure
    "NotSignedInWithIdp" -> pure AuditsFederatedAuthRequestIssueReasonNotSignedInWithIdp
    "MissingTransientUserActivation" -> pure AuditsFederatedAuthRequestIssueReasonMissingTransientUserActivation
    "ReplacedByActiveMode" -> pure AuditsFederatedAuthRequestIssueReasonReplacedByActiveMode
    "RelyingPartyOriginIsOpaque" -> pure AuditsFederatedAuthRequestIssueReasonRelyingPartyOriginIsOpaque
    "TypeNotMatching" -> pure AuditsFederatedAuthRequestIssueReasonTypeNotMatching
    "UiDismissedNoEmbargo" -> pure AuditsFederatedAuthRequestIssueReasonUiDismissedNoEmbargo
    "CorsError" -> pure AuditsFederatedAuthRequestIssueReasonCorsError
    "SuppressedBySegmentationPlatform" -> pure AuditsFederatedAuthRequestIssueReasonSuppressedBySegmentationPlatform
    "_" -> fail "failed to parse AuditsFederatedAuthRequestIssueReason"
instance ToJSON AuditsFederatedAuthRequestIssueReason where
  toJSON v = A.String $ case v of
    AuditsFederatedAuthRequestIssueReasonShouldEmbargo -> "ShouldEmbargo"
    AuditsFederatedAuthRequestIssueReasonTooManyRequests -> "TooManyRequests"
    AuditsFederatedAuthRequestIssueReasonWellKnownHttpNotFound -> "WellKnownHttpNotFound"
    AuditsFederatedAuthRequestIssueReasonWellKnownNoResponse -> "WellKnownNoResponse"
    AuditsFederatedAuthRequestIssueReasonWellKnownBlockedByConnectionAllowlist -> "WellKnownBlockedByConnectionAllowlist"
    AuditsFederatedAuthRequestIssueReasonWellKnownInvalidResponse -> "WellKnownInvalidResponse"
    AuditsFederatedAuthRequestIssueReasonWellKnownListEmpty -> "WellKnownListEmpty"
    AuditsFederatedAuthRequestIssueReasonWellKnownInvalidContentType -> "WellKnownInvalidContentType"
    AuditsFederatedAuthRequestIssueReasonConfigNotInWellKnown -> "ConfigNotInWellKnown"
    AuditsFederatedAuthRequestIssueReasonWellKnownTooBig -> "WellKnownTooBig"
    AuditsFederatedAuthRequestIssueReasonConfigHttpNotFound -> "ConfigHttpNotFound"
    AuditsFederatedAuthRequestIssueReasonConfigNoResponse -> "ConfigNoResponse"
    AuditsFederatedAuthRequestIssueReasonConfigBlockedByConnectionAllowlist -> "ConfigBlockedByConnectionAllowlist"
    AuditsFederatedAuthRequestIssueReasonConfigInvalidResponse -> "ConfigInvalidResponse"
    AuditsFederatedAuthRequestIssueReasonConfigInvalidContentType -> "ConfigInvalidContentType"
    AuditsFederatedAuthRequestIssueReasonIdpNotPotentiallyTrustworthy -> "IdpNotPotentiallyTrustworthy"
    AuditsFederatedAuthRequestIssueReasonDisabledInSettings -> "DisabledInSettings"
    AuditsFederatedAuthRequestIssueReasonDisabledInFlags -> "DisabledInFlags"
    AuditsFederatedAuthRequestIssueReasonErrorFetchingSignin -> "ErrorFetchingSignin"
    AuditsFederatedAuthRequestIssueReasonInvalidSigninResponse -> "InvalidSigninResponse"
    AuditsFederatedAuthRequestIssueReasonAccountsHttpNotFound -> "AccountsHttpNotFound"
    AuditsFederatedAuthRequestIssueReasonAccountsNoResponse -> "AccountsNoResponse"
    AuditsFederatedAuthRequestIssueReasonAccountsBlockedByConnectionAllowlist -> "AccountsBlockedByConnectionAllowlist"
    AuditsFederatedAuthRequestIssueReasonAccountsInvalidResponse -> "AccountsInvalidResponse"
    AuditsFederatedAuthRequestIssueReasonAccountsListEmpty -> "AccountsListEmpty"
    AuditsFederatedAuthRequestIssueReasonAccountsInvalidContentType -> "AccountsInvalidContentType"
    AuditsFederatedAuthRequestIssueReasonIdTokenHttpNotFound -> "IdTokenHttpNotFound"
    AuditsFederatedAuthRequestIssueReasonIdTokenNoResponse -> "IdTokenNoResponse"
    AuditsFederatedAuthRequestIssueReasonIdTokenBlockedByConnectionAllowlist -> "IdTokenBlockedByConnectionAllowlist"
    AuditsFederatedAuthRequestIssueReasonIdTokenInvalidResponse -> "IdTokenInvalidResponse"
    AuditsFederatedAuthRequestIssueReasonIdTokenIdpErrorResponse -> "IdTokenIdpErrorResponse"
    AuditsFederatedAuthRequestIssueReasonIdTokenCrossSiteIdpErrorResponse -> "IdTokenCrossSiteIdpErrorResponse"
    AuditsFederatedAuthRequestIssueReasonIdTokenInvalidRequest -> "IdTokenInvalidRequest"
    AuditsFederatedAuthRequestIssueReasonIdTokenInvalidContentType -> "IdTokenInvalidContentType"
    AuditsFederatedAuthRequestIssueReasonErrorIdToken -> "ErrorIdToken"
    AuditsFederatedAuthRequestIssueReasonCanceled -> "Canceled"
    AuditsFederatedAuthRequestIssueReasonRpPageNotVisible -> "RpPageNotVisible"
    AuditsFederatedAuthRequestIssueReasonSilentMediationFailure -> "SilentMediationFailure"
    AuditsFederatedAuthRequestIssueReasonNotSignedInWithIdp -> "NotSignedInWithIdp"
    AuditsFederatedAuthRequestIssueReasonMissingTransientUserActivation -> "MissingTransientUserActivation"
    AuditsFederatedAuthRequestIssueReasonReplacedByActiveMode -> "ReplacedByActiveMode"
    AuditsFederatedAuthRequestIssueReasonRelyingPartyOriginIsOpaque -> "RelyingPartyOriginIsOpaque"
    AuditsFederatedAuthRequestIssueReasonTypeNotMatching -> "TypeNotMatching"
    AuditsFederatedAuthRequestIssueReasonUiDismissedNoEmbargo -> "UiDismissedNoEmbargo"
    AuditsFederatedAuthRequestIssueReasonCorsError -> "CorsError"
    AuditsFederatedAuthRequestIssueReasonSuppressedBySegmentationPlatform -> "SuppressedBySegmentationPlatform"

-- | Type 'Audits.FederatedAuthUserInfoRequestIssueDetails'.
data AuditsFederatedAuthUserInfoRequestIssueDetails = AuditsFederatedAuthUserInfoRequestIssueDetails
  {
    auditsFederatedAuthUserInfoRequestIssueDetailsFederatedAuthUserInfoRequestIssueReason :: AuditsFederatedAuthUserInfoRequestIssueReason
  }
  deriving (Eq, Show)
instance FromJSON AuditsFederatedAuthUserInfoRequestIssueDetails where
  parseJSON = A.withObject "AuditsFederatedAuthUserInfoRequestIssueDetails" $ \o -> AuditsFederatedAuthUserInfoRequestIssueDetails
    <$> o A..: "federatedAuthUserInfoRequestIssueReason"
instance ToJSON AuditsFederatedAuthUserInfoRequestIssueDetails where
  toJSON p = A.object $ catMaybes [
    ("federatedAuthUserInfoRequestIssueReason" A..=) <$> Just (auditsFederatedAuthUserInfoRequestIssueDetailsFederatedAuthUserInfoRequestIssueReason p)
    ]

-- | Type 'Audits.FederatedAuthUserInfoRequestIssueReason'.
--   Represents the failure reason when a getUserInfo() call fails.
--   Should be updated alongside FederatedAuthUserInfoRequestResult in
--   third_party/blink/public/mojom/devtools/inspector_issue.mojom.
data AuditsFederatedAuthUserInfoRequestIssueReason = AuditsFederatedAuthUserInfoRequestIssueReasonNotSameOrigin | AuditsFederatedAuthUserInfoRequestIssueReasonNotIframe | AuditsFederatedAuthUserInfoRequestIssueReasonNotPotentiallyTrustworthy | AuditsFederatedAuthUserInfoRequestIssueReasonNoApiPermission | AuditsFederatedAuthUserInfoRequestIssueReasonNotSignedInWithIdp | AuditsFederatedAuthUserInfoRequestIssueReasonNoAccountSharingPermission | AuditsFederatedAuthUserInfoRequestIssueReasonInvalidConfigOrWellKnown | AuditsFederatedAuthUserInfoRequestIssueReasonInvalidAccountsResponse | AuditsFederatedAuthUserInfoRequestIssueReasonNoReturningUserFromFetchedAccounts
  deriving (Ord, Eq, Show, Read)
instance FromJSON AuditsFederatedAuthUserInfoRequestIssueReason where
  parseJSON = A.withText "AuditsFederatedAuthUserInfoRequestIssueReason" $ \v -> case v of
    "NotSameOrigin" -> pure AuditsFederatedAuthUserInfoRequestIssueReasonNotSameOrigin
    "NotIframe" -> pure AuditsFederatedAuthUserInfoRequestIssueReasonNotIframe
    "NotPotentiallyTrustworthy" -> pure AuditsFederatedAuthUserInfoRequestIssueReasonNotPotentiallyTrustworthy
    "NoApiPermission" -> pure AuditsFederatedAuthUserInfoRequestIssueReasonNoApiPermission
    "NotSignedInWithIdp" -> pure AuditsFederatedAuthUserInfoRequestIssueReasonNotSignedInWithIdp
    "NoAccountSharingPermission" -> pure AuditsFederatedAuthUserInfoRequestIssueReasonNoAccountSharingPermission
    "InvalidConfigOrWellKnown" -> pure AuditsFederatedAuthUserInfoRequestIssueReasonInvalidConfigOrWellKnown
    "InvalidAccountsResponse" -> pure AuditsFederatedAuthUserInfoRequestIssueReasonInvalidAccountsResponse
    "NoReturningUserFromFetchedAccounts" -> pure AuditsFederatedAuthUserInfoRequestIssueReasonNoReturningUserFromFetchedAccounts
    "_" -> fail "failed to parse AuditsFederatedAuthUserInfoRequestIssueReason"
instance ToJSON AuditsFederatedAuthUserInfoRequestIssueReason where
  toJSON v = A.String $ case v of
    AuditsFederatedAuthUserInfoRequestIssueReasonNotSameOrigin -> "NotSameOrigin"
    AuditsFederatedAuthUserInfoRequestIssueReasonNotIframe -> "NotIframe"
    AuditsFederatedAuthUserInfoRequestIssueReasonNotPotentiallyTrustworthy -> "NotPotentiallyTrustworthy"
    AuditsFederatedAuthUserInfoRequestIssueReasonNoApiPermission -> "NoApiPermission"
    AuditsFederatedAuthUserInfoRequestIssueReasonNotSignedInWithIdp -> "NotSignedInWithIdp"
    AuditsFederatedAuthUserInfoRequestIssueReasonNoAccountSharingPermission -> "NoAccountSharingPermission"
    AuditsFederatedAuthUserInfoRequestIssueReasonInvalidConfigOrWellKnown -> "InvalidConfigOrWellKnown"
    AuditsFederatedAuthUserInfoRequestIssueReasonInvalidAccountsResponse -> "InvalidAccountsResponse"
    AuditsFederatedAuthUserInfoRequestIssueReasonNoReturningUserFromFetchedAccounts -> "NoReturningUserFromFetchedAccounts"

-- | Type 'Audits.EmailVerificationRequestIssueDetails'.
data AuditsEmailVerificationRequestIssueDetails = AuditsEmailVerificationRequestIssueDetails
  {
    auditsEmailVerificationRequestIssueDetailsEmailVerificationRequestIssueReason :: AuditsEmailVerificationRequestIssueReason
  }
  deriving (Eq, Show)
instance FromJSON AuditsEmailVerificationRequestIssueDetails where
  parseJSON = A.withObject "AuditsEmailVerificationRequestIssueDetails" $ \o -> AuditsEmailVerificationRequestIssueDetails
    <$> o A..: "emailVerificationRequestIssueReason"
instance ToJSON AuditsEmailVerificationRequestIssueDetails where
  toJSON p = A.object $ catMaybes [
    ("emailVerificationRequestIssueReason" A..=) <$> Just (auditsEmailVerificationRequestIssueDetailsEmailVerificationRequestIssueReason p)
    ]

-- | Type 'Audits.EmailVerificationRequestIssueReason'.
--   Represents the failure reason when an email verification request fails.
--   Should be updated alongside EmailVerificationRequestResult in
--   third_party/blink/public/mojom/devtools/inspector_issue.mojom.
data AuditsEmailVerificationRequestIssueReason = AuditsEmailVerificationRequestIssueReasonInvalidEmail | AuditsEmailVerificationRequestIssueReasonDnsFetchFailed | AuditsEmailVerificationRequestIssueReasonDnsInvalidRecord | AuditsEmailVerificationRequestIssueReasonWellKnownHttpNotFound | AuditsEmailVerificationRequestIssueReasonWellKnownNoResponse | AuditsEmailVerificationRequestIssueReasonWellKnownInvalidResponse | AuditsEmailVerificationRequestIssueReasonWellKnownListEmpty | AuditsEmailVerificationRequestIssueReasonWellKnownInvalidContentType | AuditsEmailVerificationRequestIssueReasonWellKnownMissingIssuanceEndpoint | AuditsEmailVerificationRequestIssueReasonWellKnownIssuanceEndpointCrossOrigin | AuditsEmailVerificationRequestIssueReasonWellKnownUnsupportedSigningAlgorithm | AuditsEmailVerificationRequestIssueReasonTokenHttpNotFound | AuditsEmailVerificationRequestIssueReasonTokenNoResponse | AuditsEmailVerificationRequestIssueReasonTokenInvalidResponse | AuditsEmailVerificationRequestIssueReasonTokenInvalidContentType | AuditsEmailVerificationRequestIssueReasonTokenMalformedSdJwt | AuditsEmailVerificationRequestIssueReasonTokenInvalidSdJwt | AuditsEmailVerificationRequestIssueReasonKeyBindingSigningFailed | AuditsEmailVerificationRequestIssueReasonRpOriginIsOpaque | AuditsEmailVerificationRequestIssueReasonWellKnownMissingAccountsEndpoint | AuditsEmailVerificationRequestIssueReasonUserLoggedOut | AuditsEmailVerificationRequestIssueReasonWellKnownAccountsEndpointCrossOrigin | AuditsEmailVerificationRequestIssueReasonAccountsHttpNotFound | AuditsEmailVerificationRequestIssueReasonAccountsNoResponse | AuditsEmailVerificationRequestIssueReasonAccountsInvalidResponse | AuditsEmailVerificationRequestIssueReasonAccountsInvalidContentType | AuditsEmailVerificationRequestIssueReasonAccountsEmptyList | AuditsEmailVerificationRequestIssueReasonEmailVerificationWellKnownHttpNotFound | AuditsEmailVerificationRequestIssueReasonEmailVerificationWellKnownNoResponse | AuditsEmailVerificationRequestIssueReasonEmailVerificationWellKnownInvalidResponse | AuditsEmailVerificationRequestIssueReasonEmailVerificationWellKnownInvalidContentType | AuditsEmailVerificationRequestIssueReasonJwksHttpNotFound | AuditsEmailVerificationRequestIssueReasonJwksInvalidResponse | AuditsEmailVerificationRequestIssueReasonTokenVerificationSdJwtUnsupportedHeaderAlg | AuditsEmailVerificationRequestIssueReasonTokenVerificationSdJwtInvalidTyp | AuditsEmailVerificationRequestIssueReasonTokenVerificationSdJwtMissingIss | AuditsEmailVerificationRequestIssueReasonTokenVerificationSdJwtMissingIat | AuditsEmailVerificationRequestIssueReasonTokenVerificationSdJwtMissingCnf | AuditsEmailVerificationRequestIssueReasonTokenVerificationSdJwtMissingEmail | AuditsEmailVerificationRequestIssueReasonTokenVerificationSdJwtInvalidIssuedAt | AuditsEmailVerificationRequestIssueReasonTokenVerificationSdJwtInvalidIssuer | AuditsEmailVerificationRequestIssueReasonTokenVerificationSdJwtJwksMissingKeys | AuditsEmailVerificationRequestIssueReasonTokenVerificationSdJwtSignatureFailed | AuditsEmailVerificationRequestIssueReasonTokenVerificationSdJwtInvalidEmailVerified | AuditsEmailVerificationRequestIssueReasonTokenVerificationSdJwtInvalidEmail | AuditsEmailVerificationRequestIssueReasonTokenVerificationSdJwtInvalidHolderKey | AuditsEmailVerificationRequestIssueReasonTokenVerificationKbInvalidTyp | AuditsEmailVerificationRequestIssueReasonTokenVerificationKbMissingAud | AuditsEmailVerificationRequestIssueReasonTokenVerificationKbMissingNonce | AuditsEmailVerificationRequestIssueReasonTokenVerificationKbMissingIat | AuditsEmailVerificationRequestIssueReasonTokenVerificationKbMissingSdHash | AuditsEmailVerificationRequestIssueReasonTokenVerificationKbInvalidIssuedAt | AuditsEmailVerificationRequestIssueReasonTokenVerificationKbInvalidAudience | AuditsEmailVerificationRequestIssueReasonTokenVerificationKbInvalidNonce | AuditsEmailVerificationRequestIssueReasonTokenVerificationKbInvalidSdHash | AuditsEmailVerificationRequestIssueReasonTokenVerificationKbMissingCnf | AuditsEmailVerificationRequestIssueReasonTokenVerificationKbSignatureFailed
  deriving (Ord, Eq, Show, Read)
instance FromJSON AuditsEmailVerificationRequestIssueReason where
  parseJSON = A.withText "AuditsEmailVerificationRequestIssueReason" $ \v -> case v of
    "InvalidEmail" -> pure AuditsEmailVerificationRequestIssueReasonInvalidEmail
    "DnsFetchFailed" -> pure AuditsEmailVerificationRequestIssueReasonDnsFetchFailed
    "DnsInvalidRecord" -> pure AuditsEmailVerificationRequestIssueReasonDnsInvalidRecord
    "WellKnownHttpNotFound" -> pure AuditsEmailVerificationRequestIssueReasonWellKnownHttpNotFound
    "WellKnownNoResponse" -> pure AuditsEmailVerificationRequestIssueReasonWellKnownNoResponse
    "WellKnownInvalidResponse" -> pure AuditsEmailVerificationRequestIssueReasonWellKnownInvalidResponse
    "WellKnownListEmpty" -> pure AuditsEmailVerificationRequestIssueReasonWellKnownListEmpty
    "WellKnownInvalidContentType" -> pure AuditsEmailVerificationRequestIssueReasonWellKnownInvalidContentType
    "WellKnownMissingIssuanceEndpoint" -> pure AuditsEmailVerificationRequestIssueReasonWellKnownMissingIssuanceEndpoint
    "WellKnownIssuanceEndpointCrossOrigin" -> pure AuditsEmailVerificationRequestIssueReasonWellKnownIssuanceEndpointCrossOrigin
    "WellKnownUnsupportedSigningAlgorithm" -> pure AuditsEmailVerificationRequestIssueReasonWellKnownUnsupportedSigningAlgorithm
    "TokenHttpNotFound" -> pure AuditsEmailVerificationRequestIssueReasonTokenHttpNotFound
    "TokenNoResponse" -> pure AuditsEmailVerificationRequestIssueReasonTokenNoResponse
    "TokenInvalidResponse" -> pure AuditsEmailVerificationRequestIssueReasonTokenInvalidResponse
    "TokenInvalidContentType" -> pure AuditsEmailVerificationRequestIssueReasonTokenInvalidContentType
    "TokenMalformedSdJwt" -> pure AuditsEmailVerificationRequestIssueReasonTokenMalformedSdJwt
    "TokenInvalidSdJwt" -> pure AuditsEmailVerificationRequestIssueReasonTokenInvalidSdJwt
    "KeyBindingSigningFailed" -> pure AuditsEmailVerificationRequestIssueReasonKeyBindingSigningFailed
    "RpOriginIsOpaque" -> pure AuditsEmailVerificationRequestIssueReasonRpOriginIsOpaque
    "WellKnownMissingAccountsEndpoint" -> pure AuditsEmailVerificationRequestIssueReasonWellKnownMissingAccountsEndpoint
    "UserLoggedOut" -> pure AuditsEmailVerificationRequestIssueReasonUserLoggedOut
    "WellKnownAccountsEndpointCrossOrigin" -> pure AuditsEmailVerificationRequestIssueReasonWellKnownAccountsEndpointCrossOrigin
    "AccountsHttpNotFound" -> pure AuditsEmailVerificationRequestIssueReasonAccountsHttpNotFound
    "AccountsNoResponse" -> pure AuditsEmailVerificationRequestIssueReasonAccountsNoResponse
    "AccountsInvalidResponse" -> pure AuditsEmailVerificationRequestIssueReasonAccountsInvalidResponse
    "AccountsInvalidContentType" -> pure AuditsEmailVerificationRequestIssueReasonAccountsInvalidContentType
    "AccountsEmptyList" -> pure AuditsEmailVerificationRequestIssueReasonAccountsEmptyList
    "EmailVerificationWellKnownHttpNotFound" -> pure AuditsEmailVerificationRequestIssueReasonEmailVerificationWellKnownHttpNotFound
    "EmailVerificationWellKnownNoResponse" -> pure AuditsEmailVerificationRequestIssueReasonEmailVerificationWellKnownNoResponse
    "EmailVerificationWellKnownInvalidResponse" -> pure AuditsEmailVerificationRequestIssueReasonEmailVerificationWellKnownInvalidResponse
    "EmailVerificationWellKnownInvalidContentType" -> pure AuditsEmailVerificationRequestIssueReasonEmailVerificationWellKnownInvalidContentType
    "JwksHttpNotFound" -> pure AuditsEmailVerificationRequestIssueReasonJwksHttpNotFound
    "JwksInvalidResponse" -> pure AuditsEmailVerificationRequestIssueReasonJwksInvalidResponse
    "TokenVerificationSdJwtUnsupportedHeaderAlg" -> pure AuditsEmailVerificationRequestIssueReasonTokenVerificationSdJwtUnsupportedHeaderAlg
    "TokenVerificationSdJwtInvalidTyp" -> pure AuditsEmailVerificationRequestIssueReasonTokenVerificationSdJwtInvalidTyp
    "TokenVerificationSdJwtMissingIss" -> pure AuditsEmailVerificationRequestIssueReasonTokenVerificationSdJwtMissingIss
    "TokenVerificationSdJwtMissingIat" -> pure AuditsEmailVerificationRequestIssueReasonTokenVerificationSdJwtMissingIat
    "TokenVerificationSdJwtMissingCnf" -> pure AuditsEmailVerificationRequestIssueReasonTokenVerificationSdJwtMissingCnf
    "TokenVerificationSdJwtMissingEmail" -> pure AuditsEmailVerificationRequestIssueReasonTokenVerificationSdJwtMissingEmail
    "TokenVerificationSdJwtInvalidIssuedAt" -> pure AuditsEmailVerificationRequestIssueReasonTokenVerificationSdJwtInvalidIssuedAt
    "TokenVerificationSdJwtInvalidIssuer" -> pure AuditsEmailVerificationRequestIssueReasonTokenVerificationSdJwtInvalidIssuer
    "TokenVerificationSdJwtJwksMissingKeys" -> pure AuditsEmailVerificationRequestIssueReasonTokenVerificationSdJwtJwksMissingKeys
    "TokenVerificationSdJwtSignatureFailed" -> pure AuditsEmailVerificationRequestIssueReasonTokenVerificationSdJwtSignatureFailed
    "TokenVerificationSdJwtInvalidEmailVerified" -> pure AuditsEmailVerificationRequestIssueReasonTokenVerificationSdJwtInvalidEmailVerified
    "TokenVerificationSdJwtInvalidEmail" -> pure AuditsEmailVerificationRequestIssueReasonTokenVerificationSdJwtInvalidEmail
    "TokenVerificationSdJwtInvalidHolderKey" -> pure AuditsEmailVerificationRequestIssueReasonTokenVerificationSdJwtInvalidHolderKey
    "TokenVerificationKbInvalidTyp" -> pure AuditsEmailVerificationRequestIssueReasonTokenVerificationKbInvalidTyp
    "TokenVerificationKbMissingAud" -> pure AuditsEmailVerificationRequestIssueReasonTokenVerificationKbMissingAud
    "TokenVerificationKbMissingNonce" -> pure AuditsEmailVerificationRequestIssueReasonTokenVerificationKbMissingNonce
    "TokenVerificationKbMissingIat" -> pure AuditsEmailVerificationRequestIssueReasonTokenVerificationKbMissingIat
    "TokenVerificationKbMissingSdHash" -> pure AuditsEmailVerificationRequestIssueReasonTokenVerificationKbMissingSdHash
    "TokenVerificationKbInvalidIssuedAt" -> pure AuditsEmailVerificationRequestIssueReasonTokenVerificationKbInvalidIssuedAt
    "TokenVerificationKbInvalidAudience" -> pure AuditsEmailVerificationRequestIssueReasonTokenVerificationKbInvalidAudience
    "TokenVerificationKbInvalidNonce" -> pure AuditsEmailVerificationRequestIssueReasonTokenVerificationKbInvalidNonce
    "TokenVerificationKbInvalidSdHash" -> pure AuditsEmailVerificationRequestIssueReasonTokenVerificationKbInvalidSdHash
    "TokenVerificationKbMissingCnf" -> pure AuditsEmailVerificationRequestIssueReasonTokenVerificationKbMissingCnf
    "TokenVerificationKbSignatureFailed" -> pure AuditsEmailVerificationRequestIssueReasonTokenVerificationKbSignatureFailed
    "_" -> fail "failed to parse AuditsEmailVerificationRequestIssueReason"
instance ToJSON AuditsEmailVerificationRequestIssueReason where
  toJSON v = A.String $ case v of
    AuditsEmailVerificationRequestIssueReasonInvalidEmail -> "InvalidEmail"
    AuditsEmailVerificationRequestIssueReasonDnsFetchFailed -> "DnsFetchFailed"
    AuditsEmailVerificationRequestIssueReasonDnsInvalidRecord -> "DnsInvalidRecord"
    AuditsEmailVerificationRequestIssueReasonWellKnownHttpNotFound -> "WellKnownHttpNotFound"
    AuditsEmailVerificationRequestIssueReasonWellKnownNoResponse -> "WellKnownNoResponse"
    AuditsEmailVerificationRequestIssueReasonWellKnownInvalidResponse -> "WellKnownInvalidResponse"
    AuditsEmailVerificationRequestIssueReasonWellKnownListEmpty -> "WellKnownListEmpty"
    AuditsEmailVerificationRequestIssueReasonWellKnownInvalidContentType -> "WellKnownInvalidContentType"
    AuditsEmailVerificationRequestIssueReasonWellKnownMissingIssuanceEndpoint -> "WellKnownMissingIssuanceEndpoint"
    AuditsEmailVerificationRequestIssueReasonWellKnownIssuanceEndpointCrossOrigin -> "WellKnownIssuanceEndpointCrossOrigin"
    AuditsEmailVerificationRequestIssueReasonWellKnownUnsupportedSigningAlgorithm -> "WellKnownUnsupportedSigningAlgorithm"
    AuditsEmailVerificationRequestIssueReasonTokenHttpNotFound -> "TokenHttpNotFound"
    AuditsEmailVerificationRequestIssueReasonTokenNoResponse -> "TokenNoResponse"
    AuditsEmailVerificationRequestIssueReasonTokenInvalidResponse -> "TokenInvalidResponse"
    AuditsEmailVerificationRequestIssueReasonTokenInvalidContentType -> "TokenInvalidContentType"
    AuditsEmailVerificationRequestIssueReasonTokenMalformedSdJwt -> "TokenMalformedSdJwt"
    AuditsEmailVerificationRequestIssueReasonTokenInvalidSdJwt -> "TokenInvalidSdJwt"
    AuditsEmailVerificationRequestIssueReasonKeyBindingSigningFailed -> "KeyBindingSigningFailed"
    AuditsEmailVerificationRequestIssueReasonRpOriginIsOpaque -> "RpOriginIsOpaque"
    AuditsEmailVerificationRequestIssueReasonWellKnownMissingAccountsEndpoint -> "WellKnownMissingAccountsEndpoint"
    AuditsEmailVerificationRequestIssueReasonUserLoggedOut -> "UserLoggedOut"
    AuditsEmailVerificationRequestIssueReasonWellKnownAccountsEndpointCrossOrigin -> "WellKnownAccountsEndpointCrossOrigin"
    AuditsEmailVerificationRequestIssueReasonAccountsHttpNotFound -> "AccountsHttpNotFound"
    AuditsEmailVerificationRequestIssueReasonAccountsNoResponse -> "AccountsNoResponse"
    AuditsEmailVerificationRequestIssueReasonAccountsInvalidResponse -> "AccountsInvalidResponse"
    AuditsEmailVerificationRequestIssueReasonAccountsInvalidContentType -> "AccountsInvalidContentType"
    AuditsEmailVerificationRequestIssueReasonAccountsEmptyList -> "AccountsEmptyList"
    AuditsEmailVerificationRequestIssueReasonEmailVerificationWellKnownHttpNotFound -> "EmailVerificationWellKnownHttpNotFound"
    AuditsEmailVerificationRequestIssueReasonEmailVerificationWellKnownNoResponse -> "EmailVerificationWellKnownNoResponse"
    AuditsEmailVerificationRequestIssueReasonEmailVerificationWellKnownInvalidResponse -> "EmailVerificationWellKnownInvalidResponse"
    AuditsEmailVerificationRequestIssueReasonEmailVerificationWellKnownInvalidContentType -> "EmailVerificationWellKnownInvalidContentType"
    AuditsEmailVerificationRequestIssueReasonJwksHttpNotFound -> "JwksHttpNotFound"
    AuditsEmailVerificationRequestIssueReasonJwksInvalidResponse -> "JwksInvalidResponse"
    AuditsEmailVerificationRequestIssueReasonTokenVerificationSdJwtUnsupportedHeaderAlg -> "TokenVerificationSdJwtUnsupportedHeaderAlg"
    AuditsEmailVerificationRequestIssueReasonTokenVerificationSdJwtInvalidTyp -> "TokenVerificationSdJwtInvalidTyp"
    AuditsEmailVerificationRequestIssueReasonTokenVerificationSdJwtMissingIss -> "TokenVerificationSdJwtMissingIss"
    AuditsEmailVerificationRequestIssueReasonTokenVerificationSdJwtMissingIat -> "TokenVerificationSdJwtMissingIat"
    AuditsEmailVerificationRequestIssueReasonTokenVerificationSdJwtMissingCnf -> "TokenVerificationSdJwtMissingCnf"
    AuditsEmailVerificationRequestIssueReasonTokenVerificationSdJwtMissingEmail -> "TokenVerificationSdJwtMissingEmail"
    AuditsEmailVerificationRequestIssueReasonTokenVerificationSdJwtInvalidIssuedAt -> "TokenVerificationSdJwtInvalidIssuedAt"
    AuditsEmailVerificationRequestIssueReasonTokenVerificationSdJwtInvalidIssuer -> "TokenVerificationSdJwtInvalidIssuer"
    AuditsEmailVerificationRequestIssueReasonTokenVerificationSdJwtJwksMissingKeys -> "TokenVerificationSdJwtJwksMissingKeys"
    AuditsEmailVerificationRequestIssueReasonTokenVerificationSdJwtSignatureFailed -> "TokenVerificationSdJwtSignatureFailed"
    AuditsEmailVerificationRequestIssueReasonTokenVerificationSdJwtInvalidEmailVerified -> "TokenVerificationSdJwtInvalidEmailVerified"
    AuditsEmailVerificationRequestIssueReasonTokenVerificationSdJwtInvalidEmail -> "TokenVerificationSdJwtInvalidEmail"
    AuditsEmailVerificationRequestIssueReasonTokenVerificationSdJwtInvalidHolderKey -> "TokenVerificationSdJwtInvalidHolderKey"
    AuditsEmailVerificationRequestIssueReasonTokenVerificationKbInvalidTyp -> "TokenVerificationKbInvalidTyp"
    AuditsEmailVerificationRequestIssueReasonTokenVerificationKbMissingAud -> "TokenVerificationKbMissingAud"
    AuditsEmailVerificationRequestIssueReasonTokenVerificationKbMissingNonce -> "TokenVerificationKbMissingNonce"
    AuditsEmailVerificationRequestIssueReasonTokenVerificationKbMissingIat -> "TokenVerificationKbMissingIat"
    AuditsEmailVerificationRequestIssueReasonTokenVerificationKbMissingSdHash -> "TokenVerificationKbMissingSdHash"
    AuditsEmailVerificationRequestIssueReasonTokenVerificationKbInvalidIssuedAt -> "TokenVerificationKbInvalidIssuedAt"
    AuditsEmailVerificationRequestIssueReasonTokenVerificationKbInvalidAudience -> "TokenVerificationKbInvalidAudience"
    AuditsEmailVerificationRequestIssueReasonTokenVerificationKbInvalidNonce -> "TokenVerificationKbInvalidNonce"
    AuditsEmailVerificationRequestIssueReasonTokenVerificationKbInvalidSdHash -> "TokenVerificationKbInvalidSdHash"
    AuditsEmailVerificationRequestIssueReasonTokenVerificationKbMissingCnf -> "TokenVerificationKbMissingCnf"
    AuditsEmailVerificationRequestIssueReasonTokenVerificationKbSignatureFailed -> "TokenVerificationKbSignatureFailed"

-- | Type 'Audits.ClientHintIssueDetails'.
--   This issue tracks client hints related issues. It's used to deprecate old
--   features, encourage the use of new ones, and provide general guidance.
data AuditsClientHintIssueDetails = AuditsClientHintIssueDetails
  {
    auditsClientHintIssueDetailsSourceCodeLocation :: AuditsSourceCodeLocation,
    auditsClientHintIssueDetailsClientHintIssueReason :: AuditsClientHintIssueReason
  }
  deriving (Eq, Show)
instance FromJSON AuditsClientHintIssueDetails where
  parseJSON = A.withObject "AuditsClientHintIssueDetails" $ \o -> AuditsClientHintIssueDetails
    <$> o A..: "sourceCodeLocation"
    <*> o A..: "clientHintIssueReason"
instance ToJSON AuditsClientHintIssueDetails where
  toJSON p = A.object $ catMaybes [
    ("sourceCodeLocation" A..=) <$> Just (auditsClientHintIssueDetailsSourceCodeLocation p),
    ("clientHintIssueReason" A..=) <$> Just (auditsClientHintIssueDetailsClientHintIssueReason p)
    ]

-- | Type 'Audits.FailedRequestInfo'.
data AuditsFailedRequestInfo = AuditsFailedRequestInfo
  {
    -- | The URL that failed to load.
    auditsFailedRequestInfoUrl :: T.Text,
    -- | The failure message for the failed request.
    auditsFailedRequestInfoFailureMessage :: T.Text,
    auditsFailedRequestInfoRequestId :: Maybe DOMNetworkEmulationPageSecurity.NetworkRequestId
  }
  deriving (Eq, Show)
instance FromJSON AuditsFailedRequestInfo where
  parseJSON = A.withObject "AuditsFailedRequestInfo" $ \o -> AuditsFailedRequestInfo
    <$> o A..: "url"
    <*> o A..: "failureMessage"
    <*> o A..:? "requestId"
instance ToJSON AuditsFailedRequestInfo where
  toJSON p = A.object $ catMaybes [
    ("url" A..=) <$> Just (auditsFailedRequestInfoUrl p),
    ("failureMessage" A..=) <$> Just (auditsFailedRequestInfoFailureMessage p),
    ("requestId" A..=) <$> (auditsFailedRequestInfoRequestId p)
    ]

-- | Type 'Audits.PartitioningBlobURLInfo'.
data AuditsPartitioningBlobURLInfo = AuditsPartitioningBlobURLInfoBlockedCrossPartitionFetching | AuditsPartitioningBlobURLInfoEnforceNoopenerForNavigation
  deriving (Ord, Eq, Show, Read)
instance FromJSON AuditsPartitioningBlobURLInfo where
  parseJSON = A.withText "AuditsPartitioningBlobURLInfo" $ \v -> case v of
    "BlockedCrossPartitionFetching" -> pure AuditsPartitioningBlobURLInfoBlockedCrossPartitionFetching
    "EnforceNoopenerForNavigation" -> pure AuditsPartitioningBlobURLInfoEnforceNoopenerForNavigation
    "_" -> fail "failed to parse AuditsPartitioningBlobURLInfo"
instance ToJSON AuditsPartitioningBlobURLInfo where
  toJSON v = A.String $ case v of
    AuditsPartitioningBlobURLInfoBlockedCrossPartitionFetching -> "BlockedCrossPartitionFetching"
    AuditsPartitioningBlobURLInfoEnforceNoopenerForNavigation -> "EnforceNoopenerForNavigation"

-- | Type 'Audits.PartitioningBlobURLIssueDetails'.
data AuditsPartitioningBlobURLIssueDetails = AuditsPartitioningBlobURLIssueDetails
  {
    -- | The BlobURL that failed to load.
    auditsPartitioningBlobURLIssueDetailsUrl :: T.Text,
    -- | Additional information about the Partitioning Blob URL issue.
    auditsPartitioningBlobURLIssueDetailsPartitioningBlobURLInfo :: AuditsPartitioningBlobURLInfo
  }
  deriving (Eq, Show)
instance FromJSON AuditsPartitioningBlobURLIssueDetails where
  parseJSON = A.withObject "AuditsPartitioningBlobURLIssueDetails" $ \o -> AuditsPartitioningBlobURLIssueDetails
    <$> o A..: "url"
    <*> o A..: "partitioningBlobURLInfo"
instance ToJSON AuditsPartitioningBlobURLIssueDetails where
  toJSON p = A.object $ catMaybes [
    ("url" A..=) <$> Just (auditsPartitioningBlobURLIssueDetailsUrl p),
    ("partitioningBlobURLInfo" A..=) <$> Just (auditsPartitioningBlobURLIssueDetailsPartitioningBlobURLInfo p)
    ]

-- | Type 'Audits.ElementAccessibilityIssueReason'.
data AuditsElementAccessibilityIssueReason = AuditsElementAccessibilityIssueReasonDisallowedSelectChild | AuditsElementAccessibilityIssueReasonDisallowedOptGroupChild | AuditsElementAccessibilityIssueReasonNonPhrasingContentOptionChild | AuditsElementAccessibilityIssueReasonInteractiveContentOptionChild | AuditsElementAccessibilityIssueReasonInteractiveContentLegendChild | AuditsElementAccessibilityIssueReasonInteractiveContentSummaryDescendant
  deriving (Ord, Eq, Show, Read)
instance FromJSON AuditsElementAccessibilityIssueReason where
  parseJSON = A.withText "AuditsElementAccessibilityIssueReason" $ \v -> case v of
    "DisallowedSelectChild" -> pure AuditsElementAccessibilityIssueReasonDisallowedSelectChild
    "DisallowedOptGroupChild" -> pure AuditsElementAccessibilityIssueReasonDisallowedOptGroupChild
    "NonPhrasingContentOptionChild" -> pure AuditsElementAccessibilityIssueReasonNonPhrasingContentOptionChild
    "InteractiveContentOptionChild" -> pure AuditsElementAccessibilityIssueReasonInteractiveContentOptionChild
    "InteractiveContentLegendChild" -> pure AuditsElementAccessibilityIssueReasonInteractiveContentLegendChild
    "InteractiveContentSummaryDescendant" -> pure AuditsElementAccessibilityIssueReasonInteractiveContentSummaryDescendant
    "_" -> fail "failed to parse AuditsElementAccessibilityIssueReason"
instance ToJSON AuditsElementAccessibilityIssueReason where
  toJSON v = A.String $ case v of
    AuditsElementAccessibilityIssueReasonDisallowedSelectChild -> "DisallowedSelectChild"
    AuditsElementAccessibilityIssueReasonDisallowedOptGroupChild -> "DisallowedOptGroupChild"
    AuditsElementAccessibilityIssueReasonNonPhrasingContentOptionChild -> "NonPhrasingContentOptionChild"
    AuditsElementAccessibilityIssueReasonInteractiveContentOptionChild -> "InteractiveContentOptionChild"
    AuditsElementAccessibilityIssueReasonInteractiveContentLegendChild -> "InteractiveContentLegendChild"
    AuditsElementAccessibilityIssueReasonInteractiveContentSummaryDescendant -> "InteractiveContentSummaryDescendant"

-- | Type 'Audits.ElementAccessibilityIssueDetails'.
--   This issue warns about errors in the select or summary element content model.
data AuditsElementAccessibilityIssueDetails = AuditsElementAccessibilityIssueDetails
  {
    auditsElementAccessibilityIssueDetailsNodeId :: DOMNetworkEmulationPageSecurity.DOMBackendNodeId,
    auditsElementAccessibilityIssueDetailsElementAccessibilityIssueReason :: AuditsElementAccessibilityIssueReason,
    auditsElementAccessibilityIssueDetailsHasDisallowedAttributes :: Bool
  }
  deriving (Eq, Show)
instance FromJSON AuditsElementAccessibilityIssueDetails where
  parseJSON = A.withObject "AuditsElementAccessibilityIssueDetails" $ \o -> AuditsElementAccessibilityIssueDetails
    <$> o A..: "nodeId"
    <*> o A..: "elementAccessibilityIssueReason"
    <*> o A..: "hasDisallowedAttributes"
instance ToJSON AuditsElementAccessibilityIssueDetails where
  toJSON p = A.object $ catMaybes [
    ("nodeId" A..=) <$> Just (auditsElementAccessibilityIssueDetailsNodeId p),
    ("elementAccessibilityIssueReason" A..=) <$> Just (auditsElementAccessibilityIssueDetailsElementAccessibilityIssueReason p),
    ("hasDisallowedAttributes" A..=) <$> Just (auditsElementAccessibilityIssueDetailsHasDisallowedAttributes p)
    ]

-- | Type 'Audits.StyleSheetLoadingIssueReason'.
data AuditsStyleSheetLoadingIssueReason = AuditsStyleSheetLoadingIssueReasonLateImportRule | AuditsStyleSheetLoadingIssueReasonRequestFailed
  deriving (Ord, Eq, Show, Read)
instance FromJSON AuditsStyleSheetLoadingIssueReason where
  parseJSON = A.withText "AuditsStyleSheetLoadingIssueReason" $ \v -> case v of
    "LateImportRule" -> pure AuditsStyleSheetLoadingIssueReasonLateImportRule
    "RequestFailed" -> pure AuditsStyleSheetLoadingIssueReasonRequestFailed
    "_" -> fail "failed to parse AuditsStyleSheetLoadingIssueReason"
instance ToJSON AuditsStyleSheetLoadingIssueReason where
  toJSON v = A.String $ case v of
    AuditsStyleSheetLoadingIssueReasonLateImportRule -> "LateImportRule"
    AuditsStyleSheetLoadingIssueReasonRequestFailed -> "RequestFailed"

-- | Type 'Audits.StylesheetLoadingIssueDetails'.
--   This issue warns when a referenced stylesheet couldn't be loaded.
data AuditsStylesheetLoadingIssueDetails = AuditsStylesheetLoadingIssueDetails
  {
    -- | Source code position that referenced the failing stylesheet.
    auditsStylesheetLoadingIssueDetailsSourceCodeLocation :: AuditsSourceCodeLocation,
    -- | Reason why the stylesheet couldn't be loaded.
    auditsStylesheetLoadingIssueDetailsStyleSheetLoadingIssueReason :: AuditsStyleSheetLoadingIssueReason,
    -- | Contains additional info when the failure was due to a request.
    auditsStylesheetLoadingIssueDetailsFailedRequestInfo :: Maybe AuditsFailedRequestInfo
  }
  deriving (Eq, Show)
instance FromJSON AuditsStylesheetLoadingIssueDetails where
  parseJSON = A.withObject "AuditsStylesheetLoadingIssueDetails" $ \o -> AuditsStylesheetLoadingIssueDetails
    <$> o A..: "sourceCodeLocation"
    <*> o A..: "styleSheetLoadingIssueReason"
    <*> o A..:? "failedRequestInfo"
instance ToJSON AuditsStylesheetLoadingIssueDetails where
  toJSON p = A.object $ catMaybes [
    ("sourceCodeLocation" A..=) <$> Just (auditsStylesheetLoadingIssueDetailsSourceCodeLocation p),
    ("styleSheetLoadingIssueReason" A..=) <$> Just (auditsStylesheetLoadingIssueDetailsStyleSheetLoadingIssueReason p),
    ("failedRequestInfo" A..=) <$> (auditsStylesheetLoadingIssueDetailsFailedRequestInfo p)
    ]

-- | Type 'Audits.PropertyRuleIssueReason'.
data AuditsPropertyRuleIssueReason = AuditsPropertyRuleIssueReasonInvalidSyntax | AuditsPropertyRuleIssueReasonInvalidInitialValue | AuditsPropertyRuleIssueReasonInvalidInherits | AuditsPropertyRuleIssueReasonInvalidName
  deriving (Ord, Eq, Show, Read)
instance FromJSON AuditsPropertyRuleIssueReason where
  parseJSON = A.withText "AuditsPropertyRuleIssueReason" $ \v -> case v of
    "InvalidSyntax" -> pure AuditsPropertyRuleIssueReasonInvalidSyntax
    "InvalidInitialValue" -> pure AuditsPropertyRuleIssueReasonInvalidInitialValue
    "InvalidInherits" -> pure AuditsPropertyRuleIssueReasonInvalidInherits
    "InvalidName" -> pure AuditsPropertyRuleIssueReasonInvalidName
    "_" -> fail "failed to parse AuditsPropertyRuleIssueReason"
instance ToJSON AuditsPropertyRuleIssueReason where
  toJSON v = A.String $ case v of
    AuditsPropertyRuleIssueReasonInvalidSyntax -> "InvalidSyntax"
    AuditsPropertyRuleIssueReasonInvalidInitialValue -> "InvalidInitialValue"
    AuditsPropertyRuleIssueReasonInvalidInherits -> "InvalidInherits"
    AuditsPropertyRuleIssueReasonInvalidName -> "InvalidName"

-- | Type 'Audits.PropertyRuleIssueDetails'.
--   This issue warns about errors in property rules that lead to property
--   registrations being ignored.
data AuditsPropertyRuleIssueDetails = AuditsPropertyRuleIssueDetails
  {
    -- | Source code position of the property rule.
    auditsPropertyRuleIssueDetailsSourceCodeLocation :: AuditsSourceCodeLocation,
    -- | Reason why the property rule was discarded.
    auditsPropertyRuleIssueDetailsPropertyRuleIssueReason :: AuditsPropertyRuleIssueReason,
    -- | The value of the property rule property that failed to parse
    auditsPropertyRuleIssueDetailsPropertyValue :: Maybe T.Text
  }
  deriving (Eq, Show)
instance FromJSON AuditsPropertyRuleIssueDetails where
  parseJSON = A.withObject "AuditsPropertyRuleIssueDetails" $ \o -> AuditsPropertyRuleIssueDetails
    <$> o A..: "sourceCodeLocation"
    <*> o A..: "propertyRuleIssueReason"
    <*> o A..:? "propertyValue"
instance ToJSON AuditsPropertyRuleIssueDetails where
  toJSON p = A.object $ catMaybes [
    ("sourceCodeLocation" A..=) <$> Just (auditsPropertyRuleIssueDetailsSourceCodeLocation p),
    ("propertyRuleIssueReason" A..=) <$> Just (auditsPropertyRuleIssueDetailsPropertyRuleIssueReason p),
    ("propertyValue" A..=) <$> (auditsPropertyRuleIssueDetailsPropertyValue p)
    ]

-- | Type 'Audits.UserReidentificationIssueType'.
data AuditsUserReidentificationIssueType = AuditsUserReidentificationIssueTypeBlockedFrameNavigation | AuditsUserReidentificationIssueTypeBlockedSubresource | AuditsUserReidentificationIssueTypeNoisedCanvasReadback
  deriving (Ord, Eq, Show, Read)
instance FromJSON AuditsUserReidentificationIssueType where
  parseJSON = A.withText "AuditsUserReidentificationIssueType" $ \v -> case v of
    "BlockedFrameNavigation" -> pure AuditsUserReidentificationIssueTypeBlockedFrameNavigation
    "BlockedSubresource" -> pure AuditsUserReidentificationIssueTypeBlockedSubresource
    "NoisedCanvasReadback" -> pure AuditsUserReidentificationIssueTypeNoisedCanvasReadback
    "_" -> fail "failed to parse AuditsUserReidentificationIssueType"
instance ToJSON AuditsUserReidentificationIssueType where
  toJSON v = A.String $ case v of
    AuditsUserReidentificationIssueTypeBlockedFrameNavigation -> "BlockedFrameNavigation"
    AuditsUserReidentificationIssueTypeBlockedSubresource -> "BlockedSubresource"
    AuditsUserReidentificationIssueTypeNoisedCanvasReadback -> "NoisedCanvasReadback"

-- | Type 'Audits.UserReidentificationIssueDetails'.
--   This issue warns about uses of APIs that may be considered misuse to
--   re-identify users.
data AuditsUserReidentificationIssueDetails = AuditsUserReidentificationIssueDetails
  {
    auditsUserReidentificationIssueDetailsType :: AuditsUserReidentificationIssueType,
    -- | Applies to BlockedFrameNavigation and BlockedSubresource issue types.
    auditsUserReidentificationIssueDetailsRequest :: Maybe AuditsAffectedRequest,
    -- | Applies to NoisedCanvasReadback issue type.
    auditsUserReidentificationIssueDetailsSourceCodeLocation :: Maybe AuditsSourceCodeLocation
  }
  deriving (Eq, Show)
instance FromJSON AuditsUserReidentificationIssueDetails where
  parseJSON = A.withObject "AuditsUserReidentificationIssueDetails" $ \o -> AuditsUserReidentificationIssueDetails
    <$> o A..: "type"
    <*> o A..:? "request"
    <*> o A..:? "sourceCodeLocation"
instance ToJSON AuditsUserReidentificationIssueDetails where
  toJSON p = A.object $ catMaybes [
    ("type" A..=) <$> Just (auditsUserReidentificationIssueDetailsType p),
    ("request" A..=) <$> (auditsUserReidentificationIssueDetailsRequest p),
    ("sourceCodeLocation" A..=) <$> (auditsUserReidentificationIssueDetailsSourceCodeLocation p)
    ]

-- | Type 'Audits.PermissionElementIssueType'.
data AuditsPermissionElementIssueType = AuditsPermissionElementIssueTypeInvalidType | AuditsPermissionElementIssueTypeFencedFrameDisallowed | AuditsPermissionElementIssueTypeCspFrameAncestorsMissing | AuditsPermissionElementIssueTypePermissionsPolicyBlocked | AuditsPermissionElementIssueTypePaddingRightUnsupported | AuditsPermissionElementIssueTypePaddingBottomUnsupported | AuditsPermissionElementIssueTypeInsetBoxShadowUnsupported | AuditsPermissionElementIssueTypeRequestInProgress | AuditsPermissionElementIssueTypeUntrustedEvent | AuditsPermissionElementIssueTypeRegistrationFailed | AuditsPermissionElementIssueTypeTypeNotSupported | AuditsPermissionElementIssueTypeInvalidTypeActivation | AuditsPermissionElementIssueTypeSecurityChecksFailed | AuditsPermissionElementIssueTypeActivationDisabled | AuditsPermissionElementIssueTypeGeolocationDeprecated | AuditsPermissionElementIssueTypeInvalidDisplayStyle | AuditsPermissionElementIssueTypeNonOpaqueColor | AuditsPermissionElementIssueTypeLowContrast | AuditsPermissionElementIssueTypeFontSizeTooSmall | AuditsPermissionElementIssueTypeFontSizeTooLarge | AuditsPermissionElementIssueTypeInvalidSizeValue | AuditsPermissionElementIssueTypeNonSecureContext | AuditsPermissionElementIssueTypeMissingTransientUserActivation
  deriving (Ord, Eq, Show, Read)
instance FromJSON AuditsPermissionElementIssueType where
  parseJSON = A.withText "AuditsPermissionElementIssueType" $ \v -> case v of
    "InvalidType" -> pure AuditsPermissionElementIssueTypeInvalidType
    "FencedFrameDisallowed" -> pure AuditsPermissionElementIssueTypeFencedFrameDisallowed
    "CspFrameAncestorsMissing" -> pure AuditsPermissionElementIssueTypeCspFrameAncestorsMissing
    "PermissionsPolicyBlocked" -> pure AuditsPermissionElementIssueTypePermissionsPolicyBlocked
    "PaddingRightUnsupported" -> pure AuditsPermissionElementIssueTypePaddingRightUnsupported
    "PaddingBottomUnsupported" -> pure AuditsPermissionElementIssueTypePaddingBottomUnsupported
    "InsetBoxShadowUnsupported" -> pure AuditsPermissionElementIssueTypeInsetBoxShadowUnsupported
    "RequestInProgress" -> pure AuditsPermissionElementIssueTypeRequestInProgress
    "UntrustedEvent" -> pure AuditsPermissionElementIssueTypeUntrustedEvent
    "RegistrationFailed" -> pure AuditsPermissionElementIssueTypeRegistrationFailed
    "TypeNotSupported" -> pure AuditsPermissionElementIssueTypeTypeNotSupported
    "InvalidTypeActivation" -> pure AuditsPermissionElementIssueTypeInvalidTypeActivation
    "SecurityChecksFailed" -> pure AuditsPermissionElementIssueTypeSecurityChecksFailed
    "ActivationDisabled" -> pure AuditsPermissionElementIssueTypeActivationDisabled
    "GeolocationDeprecated" -> pure AuditsPermissionElementIssueTypeGeolocationDeprecated
    "InvalidDisplayStyle" -> pure AuditsPermissionElementIssueTypeInvalidDisplayStyle
    "NonOpaqueColor" -> pure AuditsPermissionElementIssueTypeNonOpaqueColor
    "LowContrast" -> pure AuditsPermissionElementIssueTypeLowContrast
    "FontSizeTooSmall" -> pure AuditsPermissionElementIssueTypeFontSizeTooSmall
    "FontSizeTooLarge" -> pure AuditsPermissionElementIssueTypeFontSizeTooLarge
    "InvalidSizeValue" -> pure AuditsPermissionElementIssueTypeInvalidSizeValue
    "NonSecureContext" -> pure AuditsPermissionElementIssueTypeNonSecureContext
    "MissingTransientUserActivation" -> pure AuditsPermissionElementIssueTypeMissingTransientUserActivation
    "_" -> fail "failed to parse AuditsPermissionElementIssueType"
instance ToJSON AuditsPermissionElementIssueType where
  toJSON v = A.String $ case v of
    AuditsPermissionElementIssueTypeInvalidType -> "InvalidType"
    AuditsPermissionElementIssueTypeFencedFrameDisallowed -> "FencedFrameDisallowed"
    AuditsPermissionElementIssueTypeCspFrameAncestorsMissing -> "CspFrameAncestorsMissing"
    AuditsPermissionElementIssueTypePermissionsPolicyBlocked -> "PermissionsPolicyBlocked"
    AuditsPermissionElementIssueTypePaddingRightUnsupported -> "PaddingRightUnsupported"
    AuditsPermissionElementIssueTypePaddingBottomUnsupported -> "PaddingBottomUnsupported"
    AuditsPermissionElementIssueTypeInsetBoxShadowUnsupported -> "InsetBoxShadowUnsupported"
    AuditsPermissionElementIssueTypeRequestInProgress -> "RequestInProgress"
    AuditsPermissionElementIssueTypeUntrustedEvent -> "UntrustedEvent"
    AuditsPermissionElementIssueTypeRegistrationFailed -> "RegistrationFailed"
    AuditsPermissionElementIssueTypeTypeNotSupported -> "TypeNotSupported"
    AuditsPermissionElementIssueTypeInvalidTypeActivation -> "InvalidTypeActivation"
    AuditsPermissionElementIssueTypeSecurityChecksFailed -> "SecurityChecksFailed"
    AuditsPermissionElementIssueTypeActivationDisabled -> "ActivationDisabled"
    AuditsPermissionElementIssueTypeGeolocationDeprecated -> "GeolocationDeprecated"
    AuditsPermissionElementIssueTypeInvalidDisplayStyle -> "InvalidDisplayStyle"
    AuditsPermissionElementIssueTypeNonOpaqueColor -> "NonOpaqueColor"
    AuditsPermissionElementIssueTypeLowContrast -> "LowContrast"
    AuditsPermissionElementIssueTypeFontSizeTooSmall -> "FontSizeTooSmall"
    AuditsPermissionElementIssueTypeFontSizeTooLarge -> "FontSizeTooLarge"
    AuditsPermissionElementIssueTypeInvalidSizeValue -> "InvalidSizeValue"
    AuditsPermissionElementIssueTypeNonSecureContext -> "NonSecureContext"
    AuditsPermissionElementIssueTypeMissingTransientUserActivation -> "MissingTransientUserActivation"

-- | Type 'Audits.PermissionElementIssueDetails'.
--   This issue warns about improper usage of the <permission> element.
data AuditsPermissionElementIssueDetails = AuditsPermissionElementIssueDetails
  {
    auditsPermissionElementIssueDetailsIssueType :: AuditsPermissionElementIssueType,
    -- | The value of the type attribute.
    auditsPermissionElementIssueDetailsType :: Maybe T.Text,
    -- | The node ID of the <permission> element.
    auditsPermissionElementIssueDetailsNodeId :: Maybe DOMNetworkEmulationPageSecurity.DOMBackendNodeId,
    -- | True if the issue is a warning, false if it is an error.
    auditsPermissionElementIssueDetailsIsWarning :: Maybe Bool,
    -- | Fields for message construction:
    --   Used for messages that reference a specific permission name
    auditsPermissionElementIssueDetailsPermissionName :: Maybe T.Text,
    -- | Used for messages about occlusion
    auditsPermissionElementIssueDetailsOccluderNodeInfo :: Maybe T.Text,
    -- | Used for messages about occluder's parent
    auditsPermissionElementIssueDetailsOccluderParentNodeInfo :: Maybe T.Text,
    -- | Used for messages about activation disabled reason
    auditsPermissionElementIssueDetailsDisableReason :: Maybe T.Text
  }
  deriving (Eq, Show)
instance FromJSON AuditsPermissionElementIssueDetails where
  parseJSON = A.withObject "AuditsPermissionElementIssueDetails" $ \o -> AuditsPermissionElementIssueDetails
    <$> o A..: "issueType"
    <*> o A..:? "type"
    <*> o A..:? "nodeId"
    <*> o A..:? "isWarning"
    <*> o A..:? "permissionName"
    <*> o A..:? "occluderNodeInfo"
    <*> o A..:? "occluderParentNodeInfo"
    <*> o A..:? "disableReason"
instance ToJSON AuditsPermissionElementIssueDetails where
  toJSON p = A.object $ catMaybes [
    ("issueType" A..=) <$> Just (auditsPermissionElementIssueDetailsIssueType p),
    ("type" A..=) <$> (auditsPermissionElementIssueDetailsType p),
    ("nodeId" A..=) <$> (auditsPermissionElementIssueDetailsNodeId p),
    ("isWarning" A..=) <$> (auditsPermissionElementIssueDetailsIsWarning p),
    ("permissionName" A..=) <$> (auditsPermissionElementIssueDetailsPermissionName p),
    ("occluderNodeInfo" A..=) <$> (auditsPermissionElementIssueDetailsOccluderNodeInfo p),
    ("occluderParentNodeInfo" A..=) <$> (auditsPermissionElementIssueDetailsOccluderParentNodeInfo p),
    ("disableReason" A..=) <$> (auditsPermissionElementIssueDetailsDisableReason p)
    ]

-- | Type 'Audits.SelectivePermissionsInterventionIssueDetails'.
--   The issue warns about blocked calls to privacy sensitive APIs via the
--   Selective Permissions Intervention.
data AuditsSelectivePermissionsInterventionIssueDetails = AuditsSelectivePermissionsInterventionIssueDetails
  {
    -- | Which API was intervened on.
    auditsSelectivePermissionsInterventionIssueDetailsApiName :: T.Text,
    -- | Why the ad script using the API is considered an ad.
    auditsSelectivePermissionsInterventionIssueDetailsAdAncestry :: DOMNetworkEmulationPageSecurity.NetworkAdAncestry,
    -- | The stack trace at the time of the intervention.
    auditsSelectivePermissionsInterventionIssueDetailsStackTrace :: Maybe Runtime.RuntimeStackTrace
  }
  deriving (Eq, Show)
instance FromJSON AuditsSelectivePermissionsInterventionIssueDetails where
  parseJSON = A.withObject "AuditsSelectivePermissionsInterventionIssueDetails" $ \o -> AuditsSelectivePermissionsInterventionIssueDetails
    <$> o A..: "apiName"
    <*> o A..: "adAncestry"
    <*> o A..:? "stackTrace"
instance ToJSON AuditsSelectivePermissionsInterventionIssueDetails where
  toJSON p = A.object $ catMaybes [
    ("apiName" A..=) <$> Just (auditsSelectivePermissionsInterventionIssueDetailsApiName p),
    ("adAncestry" A..=) <$> Just (auditsSelectivePermissionsInterventionIssueDetailsAdAncestry p),
    ("stackTrace" A..=) <$> (auditsSelectivePermissionsInterventionIssueDetailsStackTrace p)
    ]

-- | Type 'Audits.LazyLoadImageIssueDetails'.
--   Details for issues about lazy-loaded images without explicit dimensions.
data AuditsLazyLoadImageIssueDetails = AuditsLazyLoadImageIssueDetails
  {
    -- | DOM node of the problematic HTMLImageElement.
    auditsLazyLoadImageIssueDetailsNodeId :: DOMNetworkEmulationPageSecurity.DOMBackendNodeId,
    -- | URL or src attribute of the image.
    auditsLazyLoadImageIssueDetailsUrl :: T.Text,
    -- | Frame containing the image.
    auditsLazyLoadImageIssueDetailsFrameId :: DOMNetworkEmulationPageSecurity.PageFrameId
  }
  deriving (Eq, Show)
instance FromJSON AuditsLazyLoadImageIssueDetails where
  parseJSON = A.withObject "AuditsLazyLoadImageIssueDetails" $ \o -> AuditsLazyLoadImageIssueDetails
    <$> o A..: "nodeId"
    <*> o A..: "url"
    <*> o A..: "frameId"
instance ToJSON AuditsLazyLoadImageIssueDetails where
  toJSON p = A.object $ catMaybes [
    ("nodeId" A..=) <$> Just (auditsLazyLoadImageIssueDetailsNodeId p),
    ("url" A..=) <$> Just (auditsLazyLoadImageIssueDetailsUrl p),
    ("frameId" A..=) <$> Just (auditsLazyLoadImageIssueDetailsFrameId p)
    ]

-- | Type 'Audits.InspectorIssueCode'.
--   A unique identifier for the type of issue. Each type may use one of the
--   optional fields in InspectorIssueDetails to convey more specific
--   information about the kind of issue.
data AuditsInspectorIssueCode = AuditsInspectorIssueCodeCookieIssue | AuditsInspectorIssueCodeMixedContentIssue | AuditsInspectorIssueCodeBlockedByResponseIssue | AuditsInspectorIssueCodeHeavyAdIssue | AuditsInspectorIssueCodeContentSecurityPolicyIssue | AuditsInspectorIssueCodeSharedArrayBufferIssue | AuditsInspectorIssueCodeCorsIssue | AuditsInspectorIssueCodeQuirksModeIssue | AuditsInspectorIssueCodePartitioningBlobURLIssue | AuditsInspectorIssueCodeNavigatorUserAgentIssue | AuditsInspectorIssueCodeGenericIssue | AuditsInspectorIssueCodeDeprecationIssue | AuditsInspectorIssueCodeClientHintIssue | AuditsInspectorIssueCodeFederatedAuthRequestIssue | AuditsInspectorIssueCodeBounceTrackingIssue | AuditsInspectorIssueCodeCookieDeprecationMetadataIssue | AuditsInspectorIssueCodeStylesheetLoadingIssue | AuditsInspectorIssueCodeFederatedAuthUserInfoRequestIssue | AuditsInspectorIssueCodePropertyRuleIssue | AuditsInspectorIssueCodeSharedDictionaryIssue | AuditsInspectorIssueCodeElementAccessibilityIssue | AuditsInspectorIssueCodeSRIMessageSignatureIssue | AuditsInspectorIssueCodeUnencodedDigestIssue | AuditsInspectorIssueCodeConnectionAllowlistIssue | AuditsInspectorIssueCodeUserReidentificationIssue | AuditsInspectorIssueCodePermissionElementIssue | AuditsInspectorIssueCodePerformanceIssue | AuditsInspectorIssueCodeSelectivePermissionsInterventionIssue | AuditsInspectorIssueCodeEmailVerificationRequestIssue | AuditsInspectorIssueCodeLazyLoadImageIssue
  deriving (Ord, Eq, Show, Read)
instance FromJSON AuditsInspectorIssueCode where
  parseJSON = A.withText "AuditsInspectorIssueCode" $ \v -> case v of
    "CookieIssue" -> pure AuditsInspectorIssueCodeCookieIssue
    "MixedContentIssue" -> pure AuditsInspectorIssueCodeMixedContentIssue
    "BlockedByResponseIssue" -> pure AuditsInspectorIssueCodeBlockedByResponseIssue
    "HeavyAdIssue" -> pure AuditsInspectorIssueCodeHeavyAdIssue
    "ContentSecurityPolicyIssue" -> pure AuditsInspectorIssueCodeContentSecurityPolicyIssue
    "SharedArrayBufferIssue" -> pure AuditsInspectorIssueCodeSharedArrayBufferIssue
    "CorsIssue" -> pure AuditsInspectorIssueCodeCorsIssue
    "QuirksModeIssue" -> pure AuditsInspectorIssueCodeQuirksModeIssue
    "PartitioningBlobURLIssue" -> pure AuditsInspectorIssueCodePartitioningBlobURLIssue
    "NavigatorUserAgentIssue" -> pure AuditsInspectorIssueCodeNavigatorUserAgentIssue
    "GenericIssue" -> pure AuditsInspectorIssueCodeGenericIssue
    "DeprecationIssue" -> pure AuditsInspectorIssueCodeDeprecationIssue
    "ClientHintIssue" -> pure AuditsInspectorIssueCodeClientHintIssue
    "FederatedAuthRequestIssue" -> pure AuditsInspectorIssueCodeFederatedAuthRequestIssue
    "BounceTrackingIssue" -> pure AuditsInspectorIssueCodeBounceTrackingIssue
    "CookieDeprecationMetadataIssue" -> pure AuditsInspectorIssueCodeCookieDeprecationMetadataIssue
    "StylesheetLoadingIssue" -> pure AuditsInspectorIssueCodeStylesheetLoadingIssue
    "FederatedAuthUserInfoRequestIssue" -> pure AuditsInspectorIssueCodeFederatedAuthUserInfoRequestIssue
    "PropertyRuleIssue" -> pure AuditsInspectorIssueCodePropertyRuleIssue
    "SharedDictionaryIssue" -> pure AuditsInspectorIssueCodeSharedDictionaryIssue
    "ElementAccessibilityIssue" -> pure AuditsInspectorIssueCodeElementAccessibilityIssue
    "SRIMessageSignatureIssue" -> pure AuditsInspectorIssueCodeSRIMessageSignatureIssue
    "UnencodedDigestIssue" -> pure AuditsInspectorIssueCodeUnencodedDigestIssue
    "ConnectionAllowlistIssue" -> pure AuditsInspectorIssueCodeConnectionAllowlistIssue
    "UserReidentificationIssue" -> pure AuditsInspectorIssueCodeUserReidentificationIssue
    "PermissionElementIssue" -> pure AuditsInspectorIssueCodePermissionElementIssue
    "PerformanceIssue" -> pure AuditsInspectorIssueCodePerformanceIssue
    "SelectivePermissionsInterventionIssue" -> pure AuditsInspectorIssueCodeSelectivePermissionsInterventionIssue
    "EmailVerificationRequestIssue" -> pure AuditsInspectorIssueCodeEmailVerificationRequestIssue
    "LazyLoadImageIssue" -> pure AuditsInspectorIssueCodeLazyLoadImageIssue
    "_" -> fail "failed to parse AuditsInspectorIssueCode"
instance ToJSON AuditsInspectorIssueCode where
  toJSON v = A.String $ case v of
    AuditsInspectorIssueCodeCookieIssue -> "CookieIssue"
    AuditsInspectorIssueCodeMixedContentIssue -> "MixedContentIssue"
    AuditsInspectorIssueCodeBlockedByResponseIssue -> "BlockedByResponseIssue"
    AuditsInspectorIssueCodeHeavyAdIssue -> "HeavyAdIssue"
    AuditsInspectorIssueCodeContentSecurityPolicyIssue -> "ContentSecurityPolicyIssue"
    AuditsInspectorIssueCodeSharedArrayBufferIssue -> "SharedArrayBufferIssue"
    AuditsInspectorIssueCodeCorsIssue -> "CorsIssue"
    AuditsInspectorIssueCodeQuirksModeIssue -> "QuirksModeIssue"
    AuditsInspectorIssueCodePartitioningBlobURLIssue -> "PartitioningBlobURLIssue"
    AuditsInspectorIssueCodeNavigatorUserAgentIssue -> "NavigatorUserAgentIssue"
    AuditsInspectorIssueCodeGenericIssue -> "GenericIssue"
    AuditsInspectorIssueCodeDeprecationIssue -> "DeprecationIssue"
    AuditsInspectorIssueCodeClientHintIssue -> "ClientHintIssue"
    AuditsInspectorIssueCodeFederatedAuthRequestIssue -> "FederatedAuthRequestIssue"
    AuditsInspectorIssueCodeBounceTrackingIssue -> "BounceTrackingIssue"
    AuditsInspectorIssueCodeCookieDeprecationMetadataIssue -> "CookieDeprecationMetadataIssue"
    AuditsInspectorIssueCodeStylesheetLoadingIssue -> "StylesheetLoadingIssue"
    AuditsInspectorIssueCodeFederatedAuthUserInfoRequestIssue -> "FederatedAuthUserInfoRequestIssue"
    AuditsInspectorIssueCodePropertyRuleIssue -> "PropertyRuleIssue"
    AuditsInspectorIssueCodeSharedDictionaryIssue -> "SharedDictionaryIssue"
    AuditsInspectorIssueCodeElementAccessibilityIssue -> "ElementAccessibilityIssue"
    AuditsInspectorIssueCodeSRIMessageSignatureIssue -> "SRIMessageSignatureIssue"
    AuditsInspectorIssueCodeUnencodedDigestIssue -> "UnencodedDigestIssue"
    AuditsInspectorIssueCodeConnectionAllowlistIssue -> "ConnectionAllowlistIssue"
    AuditsInspectorIssueCodeUserReidentificationIssue -> "UserReidentificationIssue"
    AuditsInspectorIssueCodePermissionElementIssue -> "PermissionElementIssue"
    AuditsInspectorIssueCodePerformanceIssue -> "PerformanceIssue"
    AuditsInspectorIssueCodeSelectivePermissionsInterventionIssue -> "SelectivePermissionsInterventionIssue"
    AuditsInspectorIssueCodeEmailVerificationRequestIssue -> "EmailVerificationRequestIssue"
    AuditsInspectorIssueCodeLazyLoadImageIssue -> "LazyLoadImageIssue"

-- | Type 'Audits.InspectorIssueDetails'.
--   This struct holds a list of optional fields with additional information
--   specific to the kind of issue. When adding a new issue code, please also
--   add a new optional field to this type.
data AuditsInspectorIssueDetails = AuditsInspectorIssueDetails
  {
    auditsInspectorIssueDetailsCookieIssueDetails :: Maybe AuditsCookieIssueDetails,
    auditsInspectorIssueDetailsMixedContentIssueDetails :: Maybe AuditsMixedContentIssueDetails,
    auditsInspectorIssueDetailsBlockedByResponseIssueDetails :: Maybe AuditsBlockedByResponseIssueDetails,
    auditsInspectorIssueDetailsHeavyAdIssueDetails :: Maybe AuditsHeavyAdIssueDetails,
    auditsInspectorIssueDetailsContentSecurityPolicyIssueDetails :: Maybe AuditsContentSecurityPolicyIssueDetails,
    auditsInspectorIssueDetailsSharedArrayBufferIssueDetails :: Maybe AuditsSharedArrayBufferIssueDetails,
    auditsInspectorIssueDetailsCorsIssueDetails :: Maybe AuditsCorsIssueDetails,
    auditsInspectorIssueDetailsQuirksModeIssueDetails :: Maybe AuditsQuirksModeIssueDetails,
    auditsInspectorIssueDetailsPartitioningBlobURLIssueDetails :: Maybe AuditsPartitioningBlobURLIssueDetails,
    auditsInspectorIssueDetailsGenericIssueDetails :: Maybe AuditsGenericIssueDetails,
    auditsInspectorIssueDetailsDeprecationIssueDetails :: Maybe AuditsDeprecationIssueDetails,
    auditsInspectorIssueDetailsClientHintIssueDetails :: Maybe AuditsClientHintIssueDetails,
    auditsInspectorIssueDetailsFederatedAuthRequestIssueDetails :: Maybe AuditsFederatedAuthRequestIssueDetails,
    auditsInspectorIssueDetailsBounceTrackingIssueDetails :: Maybe AuditsBounceTrackingIssueDetails,
    auditsInspectorIssueDetailsCookieDeprecationMetadataIssueDetails :: Maybe AuditsCookieDeprecationMetadataIssueDetails,
    auditsInspectorIssueDetailsStylesheetLoadingIssueDetails :: Maybe AuditsStylesheetLoadingIssueDetails,
    auditsInspectorIssueDetailsPropertyRuleIssueDetails :: Maybe AuditsPropertyRuleIssueDetails,
    auditsInspectorIssueDetailsFederatedAuthUserInfoRequestIssueDetails :: Maybe AuditsFederatedAuthUserInfoRequestIssueDetails,
    auditsInspectorIssueDetailsSharedDictionaryIssueDetails :: Maybe AuditsSharedDictionaryIssueDetails,
    auditsInspectorIssueDetailsElementAccessibilityIssueDetails :: Maybe AuditsElementAccessibilityIssueDetails,
    auditsInspectorIssueDetailsSriMessageSignatureIssueDetails :: Maybe AuditsSRIMessageSignatureIssueDetails,
    auditsInspectorIssueDetailsUnencodedDigestIssueDetails :: Maybe AuditsUnencodedDigestIssueDetails,
    auditsInspectorIssueDetailsConnectionAllowlistIssueDetails :: Maybe AuditsConnectionAllowlistIssueDetails,
    auditsInspectorIssueDetailsUserReidentificationIssueDetails :: Maybe AuditsUserReidentificationIssueDetails,
    auditsInspectorIssueDetailsPermissionElementIssueDetails :: Maybe AuditsPermissionElementIssueDetails,
    auditsInspectorIssueDetailsPerformanceIssueDetails :: Maybe AuditsPerformanceIssueDetails,
    auditsInspectorIssueDetailsSelectivePermissionsInterventionIssueDetails :: Maybe AuditsSelectivePermissionsInterventionIssueDetails,
    auditsInspectorIssueDetailsEmailVerificationRequestIssueDetails :: Maybe AuditsEmailVerificationRequestIssueDetails,
    auditsInspectorIssueDetailsLazyLoadImageIssueDetails :: Maybe AuditsLazyLoadImageIssueDetails
  }
  deriving (Eq, Show)
instance FromJSON AuditsInspectorIssueDetails where
  parseJSON = A.withObject "AuditsInspectorIssueDetails" $ \o -> AuditsInspectorIssueDetails
    <$> o A..:? "cookieIssueDetails"
    <*> o A..:? "mixedContentIssueDetails"
    <*> o A..:? "blockedByResponseIssueDetails"
    <*> o A..:? "heavyAdIssueDetails"
    <*> o A..:? "contentSecurityPolicyIssueDetails"
    <*> o A..:? "sharedArrayBufferIssueDetails"
    <*> o A..:? "corsIssueDetails"
    <*> o A..:? "quirksModeIssueDetails"
    <*> o A..:? "partitioningBlobURLIssueDetails"
    <*> o A..:? "genericIssueDetails"
    <*> o A..:? "deprecationIssueDetails"
    <*> o A..:? "clientHintIssueDetails"
    <*> o A..:? "federatedAuthRequestIssueDetails"
    <*> o A..:? "bounceTrackingIssueDetails"
    <*> o A..:? "cookieDeprecationMetadataIssueDetails"
    <*> o A..:? "stylesheetLoadingIssueDetails"
    <*> o A..:? "propertyRuleIssueDetails"
    <*> o A..:? "federatedAuthUserInfoRequestIssueDetails"
    <*> o A..:? "sharedDictionaryIssueDetails"
    <*> o A..:? "elementAccessibilityIssueDetails"
    <*> o A..:? "sriMessageSignatureIssueDetails"
    <*> o A..:? "unencodedDigestIssueDetails"
    <*> o A..:? "connectionAllowlistIssueDetails"
    <*> o A..:? "userReidentificationIssueDetails"
    <*> o A..:? "permissionElementIssueDetails"
    <*> o A..:? "performanceIssueDetails"
    <*> o A..:? "selectivePermissionsInterventionIssueDetails"
    <*> o A..:? "emailVerificationRequestIssueDetails"
    <*> o A..:? "lazyLoadImageIssueDetails"
instance ToJSON AuditsInspectorIssueDetails where
  toJSON p = A.object $ catMaybes [
    ("cookieIssueDetails" A..=) <$> (auditsInspectorIssueDetailsCookieIssueDetails p),
    ("mixedContentIssueDetails" A..=) <$> (auditsInspectorIssueDetailsMixedContentIssueDetails p),
    ("blockedByResponseIssueDetails" A..=) <$> (auditsInspectorIssueDetailsBlockedByResponseIssueDetails p),
    ("heavyAdIssueDetails" A..=) <$> (auditsInspectorIssueDetailsHeavyAdIssueDetails p),
    ("contentSecurityPolicyIssueDetails" A..=) <$> (auditsInspectorIssueDetailsContentSecurityPolicyIssueDetails p),
    ("sharedArrayBufferIssueDetails" A..=) <$> (auditsInspectorIssueDetailsSharedArrayBufferIssueDetails p),
    ("corsIssueDetails" A..=) <$> (auditsInspectorIssueDetailsCorsIssueDetails p),
    ("quirksModeIssueDetails" A..=) <$> (auditsInspectorIssueDetailsQuirksModeIssueDetails p),
    ("partitioningBlobURLIssueDetails" A..=) <$> (auditsInspectorIssueDetailsPartitioningBlobURLIssueDetails p),
    ("genericIssueDetails" A..=) <$> (auditsInspectorIssueDetailsGenericIssueDetails p),
    ("deprecationIssueDetails" A..=) <$> (auditsInspectorIssueDetailsDeprecationIssueDetails p),
    ("clientHintIssueDetails" A..=) <$> (auditsInspectorIssueDetailsClientHintIssueDetails p),
    ("federatedAuthRequestIssueDetails" A..=) <$> (auditsInspectorIssueDetailsFederatedAuthRequestIssueDetails p),
    ("bounceTrackingIssueDetails" A..=) <$> (auditsInspectorIssueDetailsBounceTrackingIssueDetails p),
    ("cookieDeprecationMetadataIssueDetails" A..=) <$> (auditsInspectorIssueDetailsCookieDeprecationMetadataIssueDetails p),
    ("stylesheetLoadingIssueDetails" A..=) <$> (auditsInspectorIssueDetailsStylesheetLoadingIssueDetails p),
    ("propertyRuleIssueDetails" A..=) <$> (auditsInspectorIssueDetailsPropertyRuleIssueDetails p),
    ("federatedAuthUserInfoRequestIssueDetails" A..=) <$> (auditsInspectorIssueDetailsFederatedAuthUserInfoRequestIssueDetails p),
    ("sharedDictionaryIssueDetails" A..=) <$> (auditsInspectorIssueDetailsSharedDictionaryIssueDetails p),
    ("elementAccessibilityIssueDetails" A..=) <$> (auditsInspectorIssueDetailsElementAccessibilityIssueDetails p),
    ("sriMessageSignatureIssueDetails" A..=) <$> (auditsInspectorIssueDetailsSriMessageSignatureIssueDetails p),
    ("unencodedDigestIssueDetails" A..=) <$> (auditsInspectorIssueDetailsUnencodedDigestIssueDetails p),
    ("connectionAllowlistIssueDetails" A..=) <$> (auditsInspectorIssueDetailsConnectionAllowlistIssueDetails p),
    ("userReidentificationIssueDetails" A..=) <$> (auditsInspectorIssueDetailsUserReidentificationIssueDetails p),
    ("permissionElementIssueDetails" A..=) <$> (auditsInspectorIssueDetailsPermissionElementIssueDetails p),
    ("performanceIssueDetails" A..=) <$> (auditsInspectorIssueDetailsPerformanceIssueDetails p),
    ("selectivePermissionsInterventionIssueDetails" A..=) <$> (auditsInspectorIssueDetailsSelectivePermissionsInterventionIssueDetails p),
    ("emailVerificationRequestIssueDetails" A..=) <$> (auditsInspectorIssueDetailsEmailVerificationRequestIssueDetails p),
    ("lazyLoadImageIssueDetails" A..=) <$> (auditsInspectorIssueDetailsLazyLoadImageIssueDetails p)
    ]

-- | Type 'Audits.IssueId'.
--   A unique id for a DevTools inspector issue. Allows other entities (e.g.
--   exceptions, CDP message, console messages, etc.) to reference an issue.
type AuditsIssueId = T.Text

-- | Type 'Audits.InspectorIssue'.
--   An inspector issue reported from the back-end.
data AuditsInspectorIssue = AuditsInspectorIssue
  {
    auditsInspectorIssueCode :: AuditsInspectorIssueCode,
    auditsInspectorIssueDetails :: AuditsInspectorIssueDetails,
    -- | A unique id for this issue. May be omitted if no other entity (e.g.
    --   exception, CDP message, etc.) is referencing this issue.
    auditsInspectorIssueIssueId :: Maybe AuditsIssueId
  }
  deriving (Eq, Show)
instance FromJSON AuditsInspectorIssue where
  parseJSON = A.withObject "AuditsInspectorIssue" $ \o -> AuditsInspectorIssue
    <$> o A..: "code"
    <*> o A..: "details"
    <*> o A..:? "issueId"
instance ToJSON AuditsInspectorIssue where
  toJSON p = A.object $ catMaybes [
    ("code" A..=) <$> Just (auditsInspectorIssueCode p),
    ("details" A..=) <$> Just (auditsInspectorIssueDetails p),
    ("issueId" A..=) <$> (auditsInspectorIssueIssueId p)
    ]

-- | Type of the 'Audits.issueAdded' event.
data AuditsIssueAdded = AuditsIssueAdded
  {
    auditsIssueAddedIssue :: AuditsInspectorIssue
  }
  deriving (Eq, Show)
instance FromJSON AuditsIssueAdded where
  parseJSON = A.withObject "AuditsIssueAdded" $ \o -> AuditsIssueAdded
    <$> o A..: "issue"
instance Event AuditsIssueAdded where
  eventName _ = "Audits.issueAdded"

-- | Returns the response body and size if it were re-encoded with the specified settings. Only
--   applies to images.

-- | Parameters of the 'Audits.getEncodedResponse' command.
data PAuditsGetEncodedResponseEncoding = PAuditsGetEncodedResponseEncodingWebp | PAuditsGetEncodedResponseEncodingJpeg | PAuditsGetEncodedResponseEncodingPng
  deriving (Ord, Eq, Show, Read)
instance FromJSON PAuditsGetEncodedResponseEncoding where
  parseJSON = A.withText "PAuditsGetEncodedResponseEncoding" $ \v -> case v of
    "webp" -> pure PAuditsGetEncodedResponseEncodingWebp
    "jpeg" -> pure PAuditsGetEncodedResponseEncodingJpeg
    "png" -> pure PAuditsGetEncodedResponseEncodingPng
    "_" -> fail "failed to parse PAuditsGetEncodedResponseEncoding"
instance ToJSON PAuditsGetEncodedResponseEncoding where
  toJSON v = A.String $ case v of
    PAuditsGetEncodedResponseEncodingWebp -> "webp"
    PAuditsGetEncodedResponseEncodingJpeg -> "jpeg"
    PAuditsGetEncodedResponseEncodingPng -> "png"
data PAuditsGetEncodedResponse = PAuditsGetEncodedResponse
  {
    -- | Identifier of the network request to get content for.
    pAuditsGetEncodedResponseRequestId :: DOMNetworkEmulationPageSecurity.NetworkRequestId,
    -- | The encoding to use.
    pAuditsGetEncodedResponseEncoding :: PAuditsGetEncodedResponseEncoding,
    -- | The quality of the encoding (0-1). (defaults to 1)
    pAuditsGetEncodedResponseQuality :: Maybe Double,
    -- | Whether to only return the size information (defaults to false).
    pAuditsGetEncodedResponseSizeOnly :: Maybe Bool
  }
  deriving (Eq, Show)
pAuditsGetEncodedResponse
  {-
  -- | Identifier of the network request to get content for.
  -}
  :: DOMNetworkEmulationPageSecurity.NetworkRequestId
  {-
  -- | The encoding to use.
  -}
  -> PAuditsGetEncodedResponseEncoding
  -> PAuditsGetEncodedResponse
pAuditsGetEncodedResponse
  arg_pAuditsGetEncodedResponseRequestId
  arg_pAuditsGetEncodedResponseEncoding
  = PAuditsGetEncodedResponse
    arg_pAuditsGetEncodedResponseRequestId
    arg_pAuditsGetEncodedResponseEncoding
    Nothing
    Nothing
instance ToJSON PAuditsGetEncodedResponse where
  toJSON p = A.object $ catMaybes [
    ("requestId" A..=) <$> Just (pAuditsGetEncodedResponseRequestId p),
    ("encoding" A..=) <$> Just (pAuditsGetEncodedResponseEncoding p),
    ("quality" A..=) <$> (pAuditsGetEncodedResponseQuality p),
    ("sizeOnly" A..=) <$> (pAuditsGetEncodedResponseSizeOnly p)
    ]
data AuditsGetEncodedResponse = AuditsGetEncodedResponse
  {
    -- | The encoded body as a base64 string. Omitted if sizeOnly is true. (Encoded as a base64 string when passed over JSON)
    auditsGetEncodedResponseBody :: Maybe T.Text,
    -- | Size before re-encoding.
    auditsGetEncodedResponseOriginalSize :: Int,
    -- | Size after re-encoding.
    auditsGetEncodedResponseEncodedSize :: Int
  }
  deriving (Eq, Show)
instance FromJSON AuditsGetEncodedResponse where
  parseJSON = A.withObject "AuditsGetEncodedResponse" $ \o -> AuditsGetEncodedResponse
    <$> o A..:? "body"
    <*> o A..: "originalSize"
    <*> o A..: "encodedSize"
instance Command PAuditsGetEncodedResponse where
  type CommandResponse PAuditsGetEncodedResponse = AuditsGetEncodedResponse
  commandName _ = "Audits.getEncodedResponse"

-- | Disables issues domain, prevents further issues from being reported to the client.

-- | Parameters of the 'Audits.disable' command.
data PAuditsDisable = PAuditsDisable
  deriving (Eq, Show)
pAuditsDisable
  :: PAuditsDisable
pAuditsDisable
  = PAuditsDisable
instance ToJSON PAuditsDisable where
  toJSON _ = A.Null
instance Command PAuditsDisable where
  type CommandResponse PAuditsDisable = ()
  commandName _ = "Audits.disable"
  fromJSON = const . A.Success . const ()

-- | Enables issues domain, sends the issues collected so far to the client by means of the
--   `issueAdded` event.

-- | Parameters of the 'Audits.enable' command.
data PAuditsEnable = PAuditsEnable
  deriving (Eq, Show)
pAuditsEnable
  :: PAuditsEnable
pAuditsEnable
  = PAuditsEnable
instance ToJSON PAuditsEnable where
  toJSON _ = A.Null
instance Command PAuditsEnable where
  type CommandResponse PAuditsEnable = ()
  commandName _ = "Audits.enable"
  fromJSON = const . A.Success . const ()

-- | Runs the form issues check for the target page. Found issues are reported
--   using Audits.issueAdded event.

-- | Parameters of the 'Audits.checkFormsIssues' command.
data PAuditsCheckFormsIssues = PAuditsCheckFormsIssues
  deriving (Eq, Show)
pAuditsCheckFormsIssues
  :: PAuditsCheckFormsIssues
pAuditsCheckFormsIssues
  = PAuditsCheckFormsIssues
instance ToJSON PAuditsCheckFormsIssues where
  toJSON _ = A.Null
data AuditsCheckFormsIssues = AuditsCheckFormsIssues
  {
    auditsCheckFormsIssuesFormIssues :: [AuditsGenericIssueDetails]
  }
  deriving (Eq, Show)
instance FromJSON AuditsCheckFormsIssues where
  parseJSON = A.withObject "AuditsCheckFormsIssues" $ \o -> AuditsCheckFormsIssues
    <$> o A..: "formIssues"
instance Command PAuditsCheckFormsIssues where
  type CommandResponse PAuditsCheckFormsIssues = AuditsCheckFormsIssues
  commandName _ = "Audits.checkFormsIssues"

