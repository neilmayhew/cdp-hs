{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}


{- |
= Preload

-}


module CDP.Domains.Preload (module CDP.Domains.Preload) where

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


-- | Type 'Preload.RuleSetId'.
--   Unique id
type PreloadRuleSetId = T.Text

-- | Type 'Preload.RuleSet'.
--   Corresponds to SpeculationRuleSet
data PreloadRuleSet = PreloadRuleSet
  {
    preloadRuleSetId :: PreloadRuleSetId,
    -- | Identifies a document which the rule set is associated with.
    preloadRuleSetLoaderId :: DOMNetworkEmulationPageSecurity.NetworkLoaderId,
    -- | Source text of JSON representing the rule set. If it comes from
    --   `<script>` tag, it is the textContent of the node. Note that it is
    --   a JSON for valid case.
    --   
    --   See also:
    --   - https://wicg.github.io/nav-speculation/speculation-rules.html
    --   - https://github.com/WICG/nav-speculation/blob/main/triggers.md
    preloadRuleSetSourceText :: T.Text,
    -- | A speculation rule set is either added through an inline
    --   `<script>` tag or through an external resource via the
    --   'Speculation-Rules' HTTP header. For the first case, we include
    --   the BackendNodeId of the relevant `<script>` tag. For the second
    --   case, we include the external URL where the rule set was loaded
    --   from, and also RequestId if Network domain is enabled.
    --   
    --   See also:
    --   - https://wicg.github.io/nav-speculation/speculation-rules.html#speculation-rules-script
    --   - https://wicg.github.io/nav-speculation/speculation-rules.html#speculation-rules-header
    preloadRuleSetBackendNodeId :: Maybe DOMNetworkEmulationPageSecurity.DOMBackendNodeId,
    preloadRuleSetUrl :: Maybe T.Text,
    preloadRuleSetRequestId :: Maybe DOMNetworkEmulationPageSecurity.NetworkRequestId,
    -- | Error information
    --   `errorMessage` is null iff `errorType` is null.
    preloadRuleSetErrorType :: Maybe PreloadRuleSetErrorType,
    -- | For more details, see:
    --   https://github.com/WICG/nav-speculation/blob/main/speculation-rules-tags.md
    preloadRuleSetTag :: Maybe T.Text
  }
  deriving (Eq, Show)
instance FromJSON PreloadRuleSet where
  parseJSON = A.withObject "PreloadRuleSet" $ \o -> PreloadRuleSet
    <$> o A..: "id"
    <*> o A..: "loaderId"
    <*> o A..: "sourceText"
    <*> o A..:? "backendNodeId"
    <*> o A..:? "url"
    <*> o A..:? "requestId"
    <*> o A..:? "errorType"
    <*> o A..:? "tag"
instance ToJSON PreloadRuleSet where
  toJSON p = A.object $ catMaybes [
    ("id" A..=) <$> Just (preloadRuleSetId p),
    ("loaderId" A..=) <$> Just (preloadRuleSetLoaderId p),
    ("sourceText" A..=) <$> Just (preloadRuleSetSourceText p),
    ("backendNodeId" A..=) <$> (preloadRuleSetBackendNodeId p),
    ("url" A..=) <$> (preloadRuleSetUrl p),
    ("requestId" A..=) <$> (preloadRuleSetRequestId p),
    ("errorType" A..=) <$> (preloadRuleSetErrorType p),
    ("tag" A..=) <$> (preloadRuleSetTag p)
    ]

-- | Type 'Preload.RuleSetErrorType'.
data PreloadRuleSetErrorType = PreloadRuleSetErrorTypeSourceIsNotJsonObject | PreloadRuleSetErrorTypeInvalidRulesSkipped | PreloadRuleSetErrorTypeInvalidRulesetLevelTag
  deriving (Ord, Eq, Show, Read)
instance FromJSON PreloadRuleSetErrorType where
  parseJSON = A.withText "PreloadRuleSetErrorType" $ \v -> case v of
    "SourceIsNotJsonObject" -> pure PreloadRuleSetErrorTypeSourceIsNotJsonObject
    "InvalidRulesSkipped" -> pure PreloadRuleSetErrorTypeInvalidRulesSkipped
    "InvalidRulesetLevelTag" -> pure PreloadRuleSetErrorTypeInvalidRulesetLevelTag
    "_" -> fail "failed to parse PreloadRuleSetErrorType"
instance ToJSON PreloadRuleSetErrorType where
  toJSON v = A.String $ case v of
    PreloadRuleSetErrorTypeSourceIsNotJsonObject -> "SourceIsNotJsonObject"
    PreloadRuleSetErrorTypeInvalidRulesSkipped -> "InvalidRulesSkipped"
    PreloadRuleSetErrorTypeInvalidRulesetLevelTag -> "InvalidRulesetLevelTag"

-- | Type 'Preload.SpeculationAction'.
--   The type of preloading attempted. It corresponds to
--   mojom::SpeculationAction (although PrefetchWithSubresources is omitted as it
--   isn't being used by clients).
data PreloadSpeculationAction = PreloadSpeculationActionPrefetch | PreloadSpeculationActionPrerender | PreloadSpeculationActionPrerenderUntilScript
  deriving (Ord, Eq, Show, Read)
instance FromJSON PreloadSpeculationAction where
  parseJSON = A.withText "PreloadSpeculationAction" $ \v -> case v of
    "Prefetch" -> pure PreloadSpeculationActionPrefetch
    "Prerender" -> pure PreloadSpeculationActionPrerender
    "PrerenderUntilScript" -> pure PreloadSpeculationActionPrerenderUntilScript
    "_" -> fail "failed to parse PreloadSpeculationAction"
instance ToJSON PreloadSpeculationAction where
  toJSON v = A.String $ case v of
    PreloadSpeculationActionPrefetch -> "Prefetch"
    PreloadSpeculationActionPrerender -> "Prerender"
    PreloadSpeculationActionPrerenderUntilScript -> "PrerenderUntilScript"

-- | Type 'Preload.SpeculationTargetHint'.
--   Corresponds to mojom::SpeculationTargetHint.
--   See https://github.com/WICG/nav-speculation/blob/main/triggers.md#window-name-targeting-hints
data PreloadSpeculationTargetHint = PreloadSpeculationTargetHintBlank | PreloadSpeculationTargetHintSelf
  deriving (Ord, Eq, Show, Read)
instance FromJSON PreloadSpeculationTargetHint where
  parseJSON = A.withText "PreloadSpeculationTargetHint" $ \v -> case v of
    "Blank" -> pure PreloadSpeculationTargetHintBlank
    "Self" -> pure PreloadSpeculationTargetHintSelf
    "_" -> fail "failed to parse PreloadSpeculationTargetHint"
instance ToJSON PreloadSpeculationTargetHint where
  toJSON v = A.String $ case v of
    PreloadSpeculationTargetHintBlank -> "Blank"
    PreloadSpeculationTargetHintSelf -> "Self"

-- | Type 'Preload.PreloadingAttemptKey'.
--   A key that identifies a preloading attempt.
--   
--   The url used is the url specified by the trigger (i.e. the initial URL), and
--   not the final url that is navigated to. For example, prerendering allows
--   same-origin main frame navigations during the attempt, but the attempt is
--   still keyed with the initial URL.
data PreloadPreloadingAttemptKey = PreloadPreloadingAttemptKey
  {
    preloadPreloadingAttemptKeyLoaderId :: DOMNetworkEmulationPageSecurity.NetworkLoaderId,
    preloadPreloadingAttemptKeyAction :: PreloadSpeculationAction,
    preloadPreloadingAttemptKeyUrl :: T.Text,
    preloadPreloadingAttemptKeyFormSubmission :: Maybe Bool,
    preloadPreloadingAttemptKeyTargetHint :: Maybe PreloadSpeculationTargetHint
  }
  deriving (Eq, Show)
instance FromJSON PreloadPreloadingAttemptKey where
  parseJSON = A.withObject "PreloadPreloadingAttemptKey" $ \o -> PreloadPreloadingAttemptKey
    <$> o A..: "loaderId"
    <*> o A..: "action"
    <*> o A..: "url"
    <*> o A..:? "formSubmission"
    <*> o A..:? "targetHint"
instance ToJSON PreloadPreloadingAttemptKey where
  toJSON p = A.object $ catMaybes [
    ("loaderId" A..=) <$> Just (preloadPreloadingAttemptKeyLoaderId p),
    ("action" A..=) <$> Just (preloadPreloadingAttemptKeyAction p),
    ("url" A..=) <$> Just (preloadPreloadingAttemptKeyUrl p),
    ("formSubmission" A..=) <$> (preloadPreloadingAttemptKeyFormSubmission p),
    ("targetHint" A..=) <$> (preloadPreloadingAttemptKeyTargetHint p)
    ]

-- | Type 'Preload.PreloadingAttemptSource'.
--   Lists sources for a preloading attempt, specifically the ids of rule sets
--   that had a speculation rule that triggered the attempt, and the
--   BackendNodeIds of <a href> or <area href> elements that triggered the
--   attempt (in the case of attempts triggered by a document rule). It is
--   possible for multiple rule sets and links to trigger a single attempt.
data PreloadPreloadingAttemptSource = PreloadPreloadingAttemptSource
  {
    preloadPreloadingAttemptSourceKey :: PreloadPreloadingAttemptKey,
    preloadPreloadingAttemptSourceRuleSetIds :: [PreloadRuleSetId],
    preloadPreloadingAttemptSourceNodeIds :: [DOMNetworkEmulationPageSecurity.DOMBackendNodeId]
  }
  deriving (Eq, Show)
instance FromJSON PreloadPreloadingAttemptSource where
  parseJSON = A.withObject "PreloadPreloadingAttemptSource" $ \o -> PreloadPreloadingAttemptSource
    <$> o A..: "key"
    <*> o A..: "ruleSetIds"
    <*> o A..: "nodeIds"
instance ToJSON PreloadPreloadingAttemptSource where
  toJSON p = A.object $ catMaybes [
    ("key" A..=) <$> Just (preloadPreloadingAttemptSourceKey p),
    ("ruleSetIds" A..=) <$> Just (preloadPreloadingAttemptSourceRuleSetIds p),
    ("nodeIds" A..=) <$> Just (preloadPreloadingAttemptSourceNodeIds p)
    ]

-- | Type 'Preload.PreloadPipelineId'.
--   Chrome manages different types of preloads together using a
--   concept of preloading pipeline. For example, if a site uses a
--   SpeculationRules for prerender, Chrome first starts a prefetch and
--   then upgrades it to prerender.
--   
--   CDP events for them are emitted separately but they share
--   `PreloadPipelineId`.
type PreloadPreloadPipelineId = T.Text

-- | Type 'Preload.PrerenderFinalStatus'.
--   List of FinalStatus reasons for Prerender2.
data PreloadPrerenderFinalStatus = PreloadPrerenderFinalStatusActivated | PreloadPrerenderFinalStatusDestroyed | PreloadPrerenderFinalStatusLowEndDevice | PreloadPrerenderFinalStatusInvalidSchemeRedirect | PreloadPrerenderFinalStatusInvalidSchemeNavigation | PreloadPrerenderFinalStatusNavigationRequestBlockedByCsp | PreloadPrerenderFinalStatusMojoBinderPolicy | PreloadPrerenderFinalStatusRendererProcessCrashed | PreloadPrerenderFinalStatusRendererProcessKilled | PreloadPrerenderFinalStatusDownload | PreloadPrerenderFinalStatusTriggerDestroyed | PreloadPrerenderFinalStatusNavigationNotCommitted | PreloadPrerenderFinalStatusNavigationBadHttpStatus | PreloadPrerenderFinalStatusClientCertRequested | PreloadPrerenderFinalStatusNavigationRequestNetworkError | PreloadPrerenderFinalStatusCancelAllHostsForTesting | PreloadPrerenderFinalStatusDidFailLoad | PreloadPrerenderFinalStatusStop | PreloadPrerenderFinalStatusSslCertificateError | PreloadPrerenderFinalStatusLoginAuthRequested | PreloadPrerenderFinalStatusUaChangeRequiresReload | PreloadPrerenderFinalStatusBlockedByClient | PreloadPrerenderFinalStatusAudioOutputDeviceRequested | PreloadPrerenderFinalStatusMixedContent | PreloadPrerenderFinalStatusTriggerBackgrounded | PreloadPrerenderFinalStatusMemoryLimitExceeded | PreloadPrerenderFinalStatusDataSaverEnabled | PreloadPrerenderFinalStatusTriggerUrlHasEffectiveUrl | PreloadPrerenderFinalStatusActivatedBeforeStarted | PreloadPrerenderFinalStatusInactivePageRestriction | PreloadPrerenderFinalStatusStartFailed | PreloadPrerenderFinalStatusTimeoutBackgrounded | PreloadPrerenderFinalStatusCrossSiteRedirectInInitialNavigation | PreloadPrerenderFinalStatusCrossSiteNavigationInInitialNavigation | PreloadPrerenderFinalStatusSameSiteCrossOriginRedirectNotOptInInInitialNavigation | PreloadPrerenderFinalStatusSameSiteCrossOriginNavigationNotOptInInInitialNavigation | PreloadPrerenderFinalStatusActivationNavigationParameterMismatch | PreloadPrerenderFinalStatusActivatedInBackground | PreloadPrerenderFinalStatusEmbedderHostDisallowed | PreloadPrerenderFinalStatusActivationNavigationDestroyedBeforeSuccess | PreloadPrerenderFinalStatusTabClosedByUserGesture | PreloadPrerenderFinalStatusTabClosedWithoutUserGesture | PreloadPrerenderFinalStatusPrimaryMainFrameRendererProcessCrashed | PreloadPrerenderFinalStatusPrimaryMainFrameRendererProcessKilled | PreloadPrerenderFinalStatusActivationFramePolicyNotCompatible | PreloadPrerenderFinalStatusPreloadingDisabled | PreloadPrerenderFinalStatusBatterySaverEnabled | PreloadPrerenderFinalStatusActivatedDuringMainFrameNavigation | PreloadPrerenderFinalStatusPreloadingUnsupportedByWebContents | PreloadPrerenderFinalStatusCrossSiteRedirectInMainFrameNavigation | PreloadPrerenderFinalStatusCrossSiteNavigationInMainFrameNavigation | PreloadPrerenderFinalStatusSameSiteCrossOriginRedirectNotOptInInMainFrameNavigation | PreloadPrerenderFinalStatusSameSiteCrossOriginNavigationNotOptInInMainFrameNavigation | PreloadPrerenderFinalStatusMemoryPressureOnTrigger | PreloadPrerenderFinalStatusMemoryPressureAfterTriggered | PreloadPrerenderFinalStatusPrerenderingDisabledByDevTools | PreloadPrerenderFinalStatusSpeculationRuleRemoved | PreloadPrerenderFinalStatusActivatedWithAuxiliaryBrowsingContexts | PreloadPrerenderFinalStatusMaxNumOfRunningEagerPrerendersExceeded | PreloadPrerenderFinalStatusMaxNumOfRunningNonEagerPrerendersExceeded | PreloadPrerenderFinalStatusMaxNumOfRunningEmbedderPrerendersExceeded | PreloadPrerenderFinalStatusPrerenderingUrlHasEffectiveUrl | PreloadPrerenderFinalStatusRedirectedPrerenderingUrlHasEffectiveUrl | PreloadPrerenderFinalStatusActivationUrlHasEffectiveUrl | PreloadPrerenderFinalStatusJavaScriptInterfaceAdded | PreloadPrerenderFinalStatusJavaScriptInterfaceRemoved | PreloadPrerenderFinalStatusAllPrerenderingCanceled | PreloadPrerenderFinalStatusWindowClosed | PreloadPrerenderFinalStatusSlowNetwork | PreloadPrerenderFinalStatusOtherPrerenderedPageActivated | PreloadPrerenderFinalStatusV8OptimizerDisabled | PreloadPrerenderFinalStatusPrerenderFailedDuringPrefetch | PreloadPrerenderFinalStatusBrowsingDataRemoved | PreloadPrerenderFinalStatusPrerenderHostReused | PreloadPrerenderFinalStatusFormSubmitWhenPrerendering | PreloadPrerenderFinalStatusCrossDocumentRestart
  deriving (Ord, Eq, Show, Read)
instance FromJSON PreloadPrerenderFinalStatus where
  parseJSON = A.withText "PreloadPrerenderFinalStatus" $ \v -> case v of
    "Activated" -> pure PreloadPrerenderFinalStatusActivated
    "Destroyed" -> pure PreloadPrerenderFinalStatusDestroyed
    "LowEndDevice" -> pure PreloadPrerenderFinalStatusLowEndDevice
    "InvalidSchemeRedirect" -> pure PreloadPrerenderFinalStatusInvalidSchemeRedirect
    "InvalidSchemeNavigation" -> pure PreloadPrerenderFinalStatusInvalidSchemeNavigation
    "NavigationRequestBlockedByCsp" -> pure PreloadPrerenderFinalStatusNavigationRequestBlockedByCsp
    "MojoBinderPolicy" -> pure PreloadPrerenderFinalStatusMojoBinderPolicy
    "RendererProcessCrashed" -> pure PreloadPrerenderFinalStatusRendererProcessCrashed
    "RendererProcessKilled" -> pure PreloadPrerenderFinalStatusRendererProcessKilled
    "Download" -> pure PreloadPrerenderFinalStatusDownload
    "TriggerDestroyed" -> pure PreloadPrerenderFinalStatusTriggerDestroyed
    "NavigationNotCommitted" -> pure PreloadPrerenderFinalStatusNavigationNotCommitted
    "NavigationBadHttpStatus" -> pure PreloadPrerenderFinalStatusNavigationBadHttpStatus
    "ClientCertRequested" -> pure PreloadPrerenderFinalStatusClientCertRequested
    "NavigationRequestNetworkError" -> pure PreloadPrerenderFinalStatusNavigationRequestNetworkError
    "CancelAllHostsForTesting" -> pure PreloadPrerenderFinalStatusCancelAllHostsForTesting
    "DidFailLoad" -> pure PreloadPrerenderFinalStatusDidFailLoad
    "Stop" -> pure PreloadPrerenderFinalStatusStop
    "SslCertificateError" -> pure PreloadPrerenderFinalStatusSslCertificateError
    "LoginAuthRequested" -> pure PreloadPrerenderFinalStatusLoginAuthRequested
    "UaChangeRequiresReload" -> pure PreloadPrerenderFinalStatusUaChangeRequiresReload
    "BlockedByClient" -> pure PreloadPrerenderFinalStatusBlockedByClient
    "AudioOutputDeviceRequested" -> pure PreloadPrerenderFinalStatusAudioOutputDeviceRequested
    "MixedContent" -> pure PreloadPrerenderFinalStatusMixedContent
    "TriggerBackgrounded" -> pure PreloadPrerenderFinalStatusTriggerBackgrounded
    "MemoryLimitExceeded" -> pure PreloadPrerenderFinalStatusMemoryLimitExceeded
    "DataSaverEnabled" -> pure PreloadPrerenderFinalStatusDataSaverEnabled
    "TriggerUrlHasEffectiveUrl" -> pure PreloadPrerenderFinalStatusTriggerUrlHasEffectiveUrl
    "ActivatedBeforeStarted" -> pure PreloadPrerenderFinalStatusActivatedBeforeStarted
    "InactivePageRestriction" -> pure PreloadPrerenderFinalStatusInactivePageRestriction
    "StartFailed" -> pure PreloadPrerenderFinalStatusStartFailed
    "TimeoutBackgrounded" -> pure PreloadPrerenderFinalStatusTimeoutBackgrounded
    "CrossSiteRedirectInInitialNavigation" -> pure PreloadPrerenderFinalStatusCrossSiteRedirectInInitialNavigation
    "CrossSiteNavigationInInitialNavigation" -> pure PreloadPrerenderFinalStatusCrossSiteNavigationInInitialNavigation
    "SameSiteCrossOriginRedirectNotOptInInInitialNavigation" -> pure PreloadPrerenderFinalStatusSameSiteCrossOriginRedirectNotOptInInInitialNavigation
    "SameSiteCrossOriginNavigationNotOptInInInitialNavigation" -> pure PreloadPrerenderFinalStatusSameSiteCrossOriginNavigationNotOptInInInitialNavigation
    "ActivationNavigationParameterMismatch" -> pure PreloadPrerenderFinalStatusActivationNavigationParameterMismatch
    "ActivatedInBackground" -> pure PreloadPrerenderFinalStatusActivatedInBackground
    "EmbedderHostDisallowed" -> pure PreloadPrerenderFinalStatusEmbedderHostDisallowed
    "ActivationNavigationDestroyedBeforeSuccess" -> pure PreloadPrerenderFinalStatusActivationNavigationDestroyedBeforeSuccess
    "TabClosedByUserGesture" -> pure PreloadPrerenderFinalStatusTabClosedByUserGesture
    "TabClosedWithoutUserGesture" -> pure PreloadPrerenderFinalStatusTabClosedWithoutUserGesture
    "PrimaryMainFrameRendererProcessCrashed" -> pure PreloadPrerenderFinalStatusPrimaryMainFrameRendererProcessCrashed
    "PrimaryMainFrameRendererProcessKilled" -> pure PreloadPrerenderFinalStatusPrimaryMainFrameRendererProcessKilled
    "ActivationFramePolicyNotCompatible" -> pure PreloadPrerenderFinalStatusActivationFramePolicyNotCompatible
    "PreloadingDisabled" -> pure PreloadPrerenderFinalStatusPreloadingDisabled
    "BatterySaverEnabled" -> pure PreloadPrerenderFinalStatusBatterySaverEnabled
    "ActivatedDuringMainFrameNavigation" -> pure PreloadPrerenderFinalStatusActivatedDuringMainFrameNavigation
    "PreloadingUnsupportedByWebContents" -> pure PreloadPrerenderFinalStatusPreloadingUnsupportedByWebContents
    "CrossSiteRedirectInMainFrameNavigation" -> pure PreloadPrerenderFinalStatusCrossSiteRedirectInMainFrameNavigation
    "CrossSiteNavigationInMainFrameNavigation" -> pure PreloadPrerenderFinalStatusCrossSiteNavigationInMainFrameNavigation
    "SameSiteCrossOriginRedirectNotOptInInMainFrameNavigation" -> pure PreloadPrerenderFinalStatusSameSiteCrossOriginRedirectNotOptInInMainFrameNavigation
    "SameSiteCrossOriginNavigationNotOptInInMainFrameNavigation" -> pure PreloadPrerenderFinalStatusSameSiteCrossOriginNavigationNotOptInInMainFrameNavigation
    "MemoryPressureOnTrigger" -> pure PreloadPrerenderFinalStatusMemoryPressureOnTrigger
    "MemoryPressureAfterTriggered" -> pure PreloadPrerenderFinalStatusMemoryPressureAfterTriggered
    "PrerenderingDisabledByDevTools" -> pure PreloadPrerenderFinalStatusPrerenderingDisabledByDevTools
    "SpeculationRuleRemoved" -> pure PreloadPrerenderFinalStatusSpeculationRuleRemoved
    "ActivatedWithAuxiliaryBrowsingContexts" -> pure PreloadPrerenderFinalStatusActivatedWithAuxiliaryBrowsingContexts
    "MaxNumOfRunningEagerPrerendersExceeded" -> pure PreloadPrerenderFinalStatusMaxNumOfRunningEagerPrerendersExceeded
    "MaxNumOfRunningNonEagerPrerendersExceeded" -> pure PreloadPrerenderFinalStatusMaxNumOfRunningNonEagerPrerendersExceeded
    "MaxNumOfRunningEmbedderPrerendersExceeded" -> pure PreloadPrerenderFinalStatusMaxNumOfRunningEmbedderPrerendersExceeded
    "PrerenderingUrlHasEffectiveUrl" -> pure PreloadPrerenderFinalStatusPrerenderingUrlHasEffectiveUrl
    "RedirectedPrerenderingUrlHasEffectiveUrl" -> pure PreloadPrerenderFinalStatusRedirectedPrerenderingUrlHasEffectiveUrl
    "ActivationUrlHasEffectiveUrl" -> pure PreloadPrerenderFinalStatusActivationUrlHasEffectiveUrl
    "JavaScriptInterfaceAdded" -> pure PreloadPrerenderFinalStatusJavaScriptInterfaceAdded
    "JavaScriptInterfaceRemoved" -> pure PreloadPrerenderFinalStatusJavaScriptInterfaceRemoved
    "AllPrerenderingCanceled" -> pure PreloadPrerenderFinalStatusAllPrerenderingCanceled
    "WindowClosed" -> pure PreloadPrerenderFinalStatusWindowClosed
    "SlowNetwork" -> pure PreloadPrerenderFinalStatusSlowNetwork
    "OtherPrerenderedPageActivated" -> pure PreloadPrerenderFinalStatusOtherPrerenderedPageActivated
    "V8OptimizerDisabled" -> pure PreloadPrerenderFinalStatusV8OptimizerDisabled
    "PrerenderFailedDuringPrefetch" -> pure PreloadPrerenderFinalStatusPrerenderFailedDuringPrefetch
    "BrowsingDataRemoved" -> pure PreloadPrerenderFinalStatusBrowsingDataRemoved
    "PrerenderHostReused" -> pure PreloadPrerenderFinalStatusPrerenderHostReused
    "FormSubmitWhenPrerendering" -> pure PreloadPrerenderFinalStatusFormSubmitWhenPrerendering
    "CrossDocumentRestart" -> pure PreloadPrerenderFinalStatusCrossDocumentRestart
    "_" -> fail "failed to parse PreloadPrerenderFinalStatus"
instance ToJSON PreloadPrerenderFinalStatus where
  toJSON v = A.String $ case v of
    PreloadPrerenderFinalStatusActivated -> "Activated"
    PreloadPrerenderFinalStatusDestroyed -> "Destroyed"
    PreloadPrerenderFinalStatusLowEndDevice -> "LowEndDevice"
    PreloadPrerenderFinalStatusInvalidSchemeRedirect -> "InvalidSchemeRedirect"
    PreloadPrerenderFinalStatusInvalidSchemeNavigation -> "InvalidSchemeNavigation"
    PreloadPrerenderFinalStatusNavigationRequestBlockedByCsp -> "NavigationRequestBlockedByCsp"
    PreloadPrerenderFinalStatusMojoBinderPolicy -> "MojoBinderPolicy"
    PreloadPrerenderFinalStatusRendererProcessCrashed -> "RendererProcessCrashed"
    PreloadPrerenderFinalStatusRendererProcessKilled -> "RendererProcessKilled"
    PreloadPrerenderFinalStatusDownload -> "Download"
    PreloadPrerenderFinalStatusTriggerDestroyed -> "TriggerDestroyed"
    PreloadPrerenderFinalStatusNavigationNotCommitted -> "NavigationNotCommitted"
    PreloadPrerenderFinalStatusNavigationBadHttpStatus -> "NavigationBadHttpStatus"
    PreloadPrerenderFinalStatusClientCertRequested -> "ClientCertRequested"
    PreloadPrerenderFinalStatusNavigationRequestNetworkError -> "NavigationRequestNetworkError"
    PreloadPrerenderFinalStatusCancelAllHostsForTesting -> "CancelAllHostsForTesting"
    PreloadPrerenderFinalStatusDidFailLoad -> "DidFailLoad"
    PreloadPrerenderFinalStatusStop -> "Stop"
    PreloadPrerenderFinalStatusSslCertificateError -> "SslCertificateError"
    PreloadPrerenderFinalStatusLoginAuthRequested -> "LoginAuthRequested"
    PreloadPrerenderFinalStatusUaChangeRequiresReload -> "UaChangeRequiresReload"
    PreloadPrerenderFinalStatusBlockedByClient -> "BlockedByClient"
    PreloadPrerenderFinalStatusAudioOutputDeviceRequested -> "AudioOutputDeviceRequested"
    PreloadPrerenderFinalStatusMixedContent -> "MixedContent"
    PreloadPrerenderFinalStatusTriggerBackgrounded -> "TriggerBackgrounded"
    PreloadPrerenderFinalStatusMemoryLimitExceeded -> "MemoryLimitExceeded"
    PreloadPrerenderFinalStatusDataSaverEnabled -> "DataSaverEnabled"
    PreloadPrerenderFinalStatusTriggerUrlHasEffectiveUrl -> "TriggerUrlHasEffectiveUrl"
    PreloadPrerenderFinalStatusActivatedBeforeStarted -> "ActivatedBeforeStarted"
    PreloadPrerenderFinalStatusInactivePageRestriction -> "InactivePageRestriction"
    PreloadPrerenderFinalStatusStartFailed -> "StartFailed"
    PreloadPrerenderFinalStatusTimeoutBackgrounded -> "TimeoutBackgrounded"
    PreloadPrerenderFinalStatusCrossSiteRedirectInInitialNavigation -> "CrossSiteRedirectInInitialNavigation"
    PreloadPrerenderFinalStatusCrossSiteNavigationInInitialNavigation -> "CrossSiteNavigationInInitialNavigation"
    PreloadPrerenderFinalStatusSameSiteCrossOriginRedirectNotOptInInInitialNavigation -> "SameSiteCrossOriginRedirectNotOptInInInitialNavigation"
    PreloadPrerenderFinalStatusSameSiteCrossOriginNavigationNotOptInInInitialNavigation -> "SameSiteCrossOriginNavigationNotOptInInInitialNavigation"
    PreloadPrerenderFinalStatusActivationNavigationParameterMismatch -> "ActivationNavigationParameterMismatch"
    PreloadPrerenderFinalStatusActivatedInBackground -> "ActivatedInBackground"
    PreloadPrerenderFinalStatusEmbedderHostDisallowed -> "EmbedderHostDisallowed"
    PreloadPrerenderFinalStatusActivationNavigationDestroyedBeforeSuccess -> "ActivationNavigationDestroyedBeforeSuccess"
    PreloadPrerenderFinalStatusTabClosedByUserGesture -> "TabClosedByUserGesture"
    PreloadPrerenderFinalStatusTabClosedWithoutUserGesture -> "TabClosedWithoutUserGesture"
    PreloadPrerenderFinalStatusPrimaryMainFrameRendererProcessCrashed -> "PrimaryMainFrameRendererProcessCrashed"
    PreloadPrerenderFinalStatusPrimaryMainFrameRendererProcessKilled -> "PrimaryMainFrameRendererProcessKilled"
    PreloadPrerenderFinalStatusActivationFramePolicyNotCompatible -> "ActivationFramePolicyNotCompatible"
    PreloadPrerenderFinalStatusPreloadingDisabled -> "PreloadingDisabled"
    PreloadPrerenderFinalStatusBatterySaverEnabled -> "BatterySaverEnabled"
    PreloadPrerenderFinalStatusActivatedDuringMainFrameNavigation -> "ActivatedDuringMainFrameNavigation"
    PreloadPrerenderFinalStatusPreloadingUnsupportedByWebContents -> "PreloadingUnsupportedByWebContents"
    PreloadPrerenderFinalStatusCrossSiteRedirectInMainFrameNavigation -> "CrossSiteRedirectInMainFrameNavigation"
    PreloadPrerenderFinalStatusCrossSiteNavigationInMainFrameNavigation -> "CrossSiteNavigationInMainFrameNavigation"
    PreloadPrerenderFinalStatusSameSiteCrossOriginRedirectNotOptInInMainFrameNavigation -> "SameSiteCrossOriginRedirectNotOptInInMainFrameNavigation"
    PreloadPrerenderFinalStatusSameSiteCrossOriginNavigationNotOptInInMainFrameNavigation -> "SameSiteCrossOriginNavigationNotOptInInMainFrameNavigation"
    PreloadPrerenderFinalStatusMemoryPressureOnTrigger -> "MemoryPressureOnTrigger"
    PreloadPrerenderFinalStatusMemoryPressureAfterTriggered -> "MemoryPressureAfterTriggered"
    PreloadPrerenderFinalStatusPrerenderingDisabledByDevTools -> "PrerenderingDisabledByDevTools"
    PreloadPrerenderFinalStatusSpeculationRuleRemoved -> "SpeculationRuleRemoved"
    PreloadPrerenderFinalStatusActivatedWithAuxiliaryBrowsingContexts -> "ActivatedWithAuxiliaryBrowsingContexts"
    PreloadPrerenderFinalStatusMaxNumOfRunningEagerPrerendersExceeded -> "MaxNumOfRunningEagerPrerendersExceeded"
    PreloadPrerenderFinalStatusMaxNumOfRunningNonEagerPrerendersExceeded -> "MaxNumOfRunningNonEagerPrerendersExceeded"
    PreloadPrerenderFinalStatusMaxNumOfRunningEmbedderPrerendersExceeded -> "MaxNumOfRunningEmbedderPrerendersExceeded"
    PreloadPrerenderFinalStatusPrerenderingUrlHasEffectiveUrl -> "PrerenderingUrlHasEffectiveUrl"
    PreloadPrerenderFinalStatusRedirectedPrerenderingUrlHasEffectiveUrl -> "RedirectedPrerenderingUrlHasEffectiveUrl"
    PreloadPrerenderFinalStatusActivationUrlHasEffectiveUrl -> "ActivationUrlHasEffectiveUrl"
    PreloadPrerenderFinalStatusJavaScriptInterfaceAdded -> "JavaScriptInterfaceAdded"
    PreloadPrerenderFinalStatusJavaScriptInterfaceRemoved -> "JavaScriptInterfaceRemoved"
    PreloadPrerenderFinalStatusAllPrerenderingCanceled -> "AllPrerenderingCanceled"
    PreloadPrerenderFinalStatusWindowClosed -> "WindowClosed"
    PreloadPrerenderFinalStatusSlowNetwork -> "SlowNetwork"
    PreloadPrerenderFinalStatusOtherPrerenderedPageActivated -> "OtherPrerenderedPageActivated"
    PreloadPrerenderFinalStatusV8OptimizerDisabled -> "V8OptimizerDisabled"
    PreloadPrerenderFinalStatusPrerenderFailedDuringPrefetch -> "PrerenderFailedDuringPrefetch"
    PreloadPrerenderFinalStatusBrowsingDataRemoved -> "BrowsingDataRemoved"
    PreloadPrerenderFinalStatusPrerenderHostReused -> "PrerenderHostReused"
    PreloadPrerenderFinalStatusFormSubmitWhenPrerendering -> "FormSubmitWhenPrerendering"
    PreloadPrerenderFinalStatusCrossDocumentRestart -> "CrossDocumentRestart"

-- | Type 'Preload.PreloadingStatus'.
--   Preloading status values, see also PreloadingTriggeringOutcome. This
--   status is shared by prefetchStatusUpdated and prerenderStatusUpdated.
data PreloadPreloadingStatus = PreloadPreloadingStatusPending | PreloadPreloadingStatusRunning | PreloadPreloadingStatusReady | PreloadPreloadingStatusSuccess | PreloadPreloadingStatusFailure | PreloadPreloadingStatusNotSupported
  deriving (Ord, Eq, Show, Read)
instance FromJSON PreloadPreloadingStatus where
  parseJSON = A.withText "PreloadPreloadingStatus" $ \v -> case v of
    "Pending" -> pure PreloadPreloadingStatusPending
    "Running" -> pure PreloadPreloadingStatusRunning
    "Ready" -> pure PreloadPreloadingStatusReady
    "Success" -> pure PreloadPreloadingStatusSuccess
    "Failure" -> pure PreloadPreloadingStatusFailure
    "NotSupported" -> pure PreloadPreloadingStatusNotSupported
    "_" -> fail "failed to parse PreloadPreloadingStatus"
instance ToJSON PreloadPreloadingStatus where
  toJSON v = A.String $ case v of
    PreloadPreloadingStatusPending -> "Pending"
    PreloadPreloadingStatusRunning -> "Running"
    PreloadPreloadingStatusReady -> "Ready"
    PreloadPreloadingStatusSuccess -> "Success"
    PreloadPreloadingStatusFailure -> "Failure"
    PreloadPreloadingStatusNotSupported -> "NotSupported"

-- | Type 'Preload.PrefetchStatus'.
--   TODO(https://crbug.com/1384419): revisit the list of PrefetchStatus and
--   filter out the ones that aren't necessary to the developers.
data PreloadPrefetchStatus = PreloadPrefetchStatusPrefetchAllowed | PreloadPrefetchStatusPrefetchFailedIneligibleRedirect | PreloadPrefetchStatusPrefetchFailedInvalidRedirect | PreloadPrefetchStatusPrefetchFailedMIMENotSupported | PreloadPrefetchStatusPrefetchFailedNetError | PreloadPrefetchStatusPrefetchFailedNon2XX | PreloadPrefetchStatusPrefetchEvictedAfterBrowsingDataRemoved | PreloadPrefetchStatusPrefetchEvictedAfterCandidateRemoved | PreloadPrefetchStatusPrefetchEvictedForNewerPrefetch | PreloadPrefetchStatusPrefetchHeldback | PreloadPrefetchStatusPrefetchIneligibleRetryAfter | PreloadPrefetchStatusPrefetchIsPrivacyDecoy | PreloadPrefetchStatusPrefetchIsStale | PreloadPrefetchStatusPrefetchNotEligibleBlockedByConnectionAllowlist | PreloadPrefetchStatusPrefetchNotEligibleBrowserContextOffTheRecord | PreloadPrefetchStatusPrefetchNotEligibleDataSaverEnabled | PreloadPrefetchStatusPrefetchNotEligibleExistingProxy | PreloadPrefetchStatusPrefetchNotEligibleHostIsNonUnique | PreloadPrefetchStatusPrefetchNotEligibleNonDefaultStoragePartition | PreloadPrefetchStatusPrefetchNotEligibleSameSiteCrossOriginPrefetchRequiredProxy | PreloadPrefetchStatusPrefetchNotEligibleSchemeIsNotHttps | PreloadPrefetchStatusPrefetchNotEligibleUserHasCookies | PreloadPrefetchStatusPrefetchNotEligibleUserHasServiceWorker | PreloadPrefetchStatusPrefetchNotEligibleUserHasServiceWorkerNoFetchHandler | PreloadPrefetchStatusPrefetchNotEligibleRedirectFromServiceWorker | PreloadPrefetchStatusPrefetchNotEligibleRedirectToServiceWorker | PreloadPrefetchStatusPrefetchNotEligibleBatterySaverEnabled | PreloadPrefetchStatusPrefetchNotEligiblePreloadingDisabled | PreloadPrefetchStatusPrefetchNotFinishedInTime | PreloadPrefetchStatusPrefetchNotStarted | PreloadPrefetchStatusPrefetchNotUsedCookiesChanged | PreloadPrefetchStatusPrefetchProxyNotAvailable | PreloadPrefetchStatusPrefetchResponseUsed | PreloadPrefetchStatusPrefetchSuccessfulButNotUsed | PreloadPrefetchStatusPrefetchNotUsedProbeFailed | PreloadPrefetchStatusPrefetchCancelledOnUserNavigation
  deriving (Ord, Eq, Show, Read)
instance FromJSON PreloadPrefetchStatus where
  parseJSON = A.withText "PreloadPrefetchStatus" $ \v -> case v of
    "PrefetchAllowed" -> pure PreloadPrefetchStatusPrefetchAllowed
    "PrefetchFailedIneligibleRedirect" -> pure PreloadPrefetchStatusPrefetchFailedIneligibleRedirect
    "PrefetchFailedInvalidRedirect" -> pure PreloadPrefetchStatusPrefetchFailedInvalidRedirect
    "PrefetchFailedMIMENotSupported" -> pure PreloadPrefetchStatusPrefetchFailedMIMENotSupported
    "PrefetchFailedNetError" -> pure PreloadPrefetchStatusPrefetchFailedNetError
    "PrefetchFailedNon2XX" -> pure PreloadPrefetchStatusPrefetchFailedNon2XX
    "PrefetchEvictedAfterBrowsingDataRemoved" -> pure PreloadPrefetchStatusPrefetchEvictedAfterBrowsingDataRemoved
    "PrefetchEvictedAfterCandidateRemoved" -> pure PreloadPrefetchStatusPrefetchEvictedAfterCandidateRemoved
    "PrefetchEvictedForNewerPrefetch" -> pure PreloadPrefetchStatusPrefetchEvictedForNewerPrefetch
    "PrefetchHeldback" -> pure PreloadPrefetchStatusPrefetchHeldback
    "PrefetchIneligibleRetryAfter" -> pure PreloadPrefetchStatusPrefetchIneligibleRetryAfter
    "PrefetchIsPrivacyDecoy" -> pure PreloadPrefetchStatusPrefetchIsPrivacyDecoy
    "PrefetchIsStale" -> pure PreloadPrefetchStatusPrefetchIsStale
    "PrefetchNotEligibleBlockedByConnectionAllowlist" -> pure PreloadPrefetchStatusPrefetchNotEligibleBlockedByConnectionAllowlist
    "PrefetchNotEligibleBrowserContextOffTheRecord" -> pure PreloadPrefetchStatusPrefetchNotEligibleBrowserContextOffTheRecord
    "PrefetchNotEligibleDataSaverEnabled" -> pure PreloadPrefetchStatusPrefetchNotEligibleDataSaverEnabled
    "PrefetchNotEligibleExistingProxy" -> pure PreloadPrefetchStatusPrefetchNotEligibleExistingProxy
    "PrefetchNotEligibleHostIsNonUnique" -> pure PreloadPrefetchStatusPrefetchNotEligibleHostIsNonUnique
    "PrefetchNotEligibleNonDefaultStoragePartition" -> pure PreloadPrefetchStatusPrefetchNotEligibleNonDefaultStoragePartition
    "PrefetchNotEligibleSameSiteCrossOriginPrefetchRequiredProxy" -> pure PreloadPrefetchStatusPrefetchNotEligibleSameSiteCrossOriginPrefetchRequiredProxy
    "PrefetchNotEligibleSchemeIsNotHttps" -> pure PreloadPrefetchStatusPrefetchNotEligibleSchemeIsNotHttps
    "PrefetchNotEligibleUserHasCookies" -> pure PreloadPrefetchStatusPrefetchNotEligibleUserHasCookies
    "PrefetchNotEligibleUserHasServiceWorker" -> pure PreloadPrefetchStatusPrefetchNotEligibleUserHasServiceWorker
    "PrefetchNotEligibleUserHasServiceWorkerNoFetchHandler" -> pure PreloadPrefetchStatusPrefetchNotEligibleUserHasServiceWorkerNoFetchHandler
    "PrefetchNotEligibleRedirectFromServiceWorker" -> pure PreloadPrefetchStatusPrefetchNotEligibleRedirectFromServiceWorker
    "PrefetchNotEligibleRedirectToServiceWorker" -> pure PreloadPrefetchStatusPrefetchNotEligibleRedirectToServiceWorker
    "PrefetchNotEligibleBatterySaverEnabled" -> pure PreloadPrefetchStatusPrefetchNotEligibleBatterySaverEnabled
    "PrefetchNotEligiblePreloadingDisabled" -> pure PreloadPrefetchStatusPrefetchNotEligiblePreloadingDisabled
    "PrefetchNotFinishedInTime" -> pure PreloadPrefetchStatusPrefetchNotFinishedInTime
    "PrefetchNotStarted" -> pure PreloadPrefetchStatusPrefetchNotStarted
    "PrefetchNotUsedCookiesChanged" -> pure PreloadPrefetchStatusPrefetchNotUsedCookiesChanged
    "PrefetchProxyNotAvailable" -> pure PreloadPrefetchStatusPrefetchProxyNotAvailable
    "PrefetchResponseUsed" -> pure PreloadPrefetchStatusPrefetchResponseUsed
    "PrefetchSuccessfulButNotUsed" -> pure PreloadPrefetchStatusPrefetchSuccessfulButNotUsed
    "PrefetchNotUsedProbeFailed" -> pure PreloadPrefetchStatusPrefetchNotUsedProbeFailed
    "PrefetchCancelledOnUserNavigation" -> pure PreloadPrefetchStatusPrefetchCancelledOnUserNavigation
    "_" -> fail "failed to parse PreloadPrefetchStatus"
instance ToJSON PreloadPrefetchStatus where
  toJSON v = A.String $ case v of
    PreloadPrefetchStatusPrefetchAllowed -> "PrefetchAllowed"
    PreloadPrefetchStatusPrefetchFailedIneligibleRedirect -> "PrefetchFailedIneligibleRedirect"
    PreloadPrefetchStatusPrefetchFailedInvalidRedirect -> "PrefetchFailedInvalidRedirect"
    PreloadPrefetchStatusPrefetchFailedMIMENotSupported -> "PrefetchFailedMIMENotSupported"
    PreloadPrefetchStatusPrefetchFailedNetError -> "PrefetchFailedNetError"
    PreloadPrefetchStatusPrefetchFailedNon2XX -> "PrefetchFailedNon2XX"
    PreloadPrefetchStatusPrefetchEvictedAfterBrowsingDataRemoved -> "PrefetchEvictedAfterBrowsingDataRemoved"
    PreloadPrefetchStatusPrefetchEvictedAfterCandidateRemoved -> "PrefetchEvictedAfterCandidateRemoved"
    PreloadPrefetchStatusPrefetchEvictedForNewerPrefetch -> "PrefetchEvictedForNewerPrefetch"
    PreloadPrefetchStatusPrefetchHeldback -> "PrefetchHeldback"
    PreloadPrefetchStatusPrefetchIneligibleRetryAfter -> "PrefetchIneligibleRetryAfter"
    PreloadPrefetchStatusPrefetchIsPrivacyDecoy -> "PrefetchIsPrivacyDecoy"
    PreloadPrefetchStatusPrefetchIsStale -> "PrefetchIsStale"
    PreloadPrefetchStatusPrefetchNotEligibleBlockedByConnectionAllowlist -> "PrefetchNotEligibleBlockedByConnectionAllowlist"
    PreloadPrefetchStatusPrefetchNotEligibleBrowserContextOffTheRecord -> "PrefetchNotEligibleBrowserContextOffTheRecord"
    PreloadPrefetchStatusPrefetchNotEligibleDataSaverEnabled -> "PrefetchNotEligibleDataSaverEnabled"
    PreloadPrefetchStatusPrefetchNotEligibleExistingProxy -> "PrefetchNotEligibleExistingProxy"
    PreloadPrefetchStatusPrefetchNotEligibleHostIsNonUnique -> "PrefetchNotEligibleHostIsNonUnique"
    PreloadPrefetchStatusPrefetchNotEligibleNonDefaultStoragePartition -> "PrefetchNotEligibleNonDefaultStoragePartition"
    PreloadPrefetchStatusPrefetchNotEligibleSameSiteCrossOriginPrefetchRequiredProxy -> "PrefetchNotEligibleSameSiteCrossOriginPrefetchRequiredProxy"
    PreloadPrefetchStatusPrefetchNotEligibleSchemeIsNotHttps -> "PrefetchNotEligibleSchemeIsNotHttps"
    PreloadPrefetchStatusPrefetchNotEligibleUserHasCookies -> "PrefetchNotEligibleUserHasCookies"
    PreloadPrefetchStatusPrefetchNotEligibleUserHasServiceWorker -> "PrefetchNotEligibleUserHasServiceWorker"
    PreloadPrefetchStatusPrefetchNotEligibleUserHasServiceWorkerNoFetchHandler -> "PrefetchNotEligibleUserHasServiceWorkerNoFetchHandler"
    PreloadPrefetchStatusPrefetchNotEligibleRedirectFromServiceWorker -> "PrefetchNotEligibleRedirectFromServiceWorker"
    PreloadPrefetchStatusPrefetchNotEligibleRedirectToServiceWorker -> "PrefetchNotEligibleRedirectToServiceWorker"
    PreloadPrefetchStatusPrefetchNotEligibleBatterySaverEnabled -> "PrefetchNotEligibleBatterySaverEnabled"
    PreloadPrefetchStatusPrefetchNotEligiblePreloadingDisabled -> "PrefetchNotEligiblePreloadingDisabled"
    PreloadPrefetchStatusPrefetchNotFinishedInTime -> "PrefetchNotFinishedInTime"
    PreloadPrefetchStatusPrefetchNotStarted -> "PrefetchNotStarted"
    PreloadPrefetchStatusPrefetchNotUsedCookiesChanged -> "PrefetchNotUsedCookiesChanged"
    PreloadPrefetchStatusPrefetchProxyNotAvailable -> "PrefetchProxyNotAvailable"
    PreloadPrefetchStatusPrefetchResponseUsed -> "PrefetchResponseUsed"
    PreloadPrefetchStatusPrefetchSuccessfulButNotUsed -> "PrefetchSuccessfulButNotUsed"
    PreloadPrefetchStatusPrefetchNotUsedProbeFailed -> "PrefetchNotUsedProbeFailed"
    PreloadPrefetchStatusPrefetchCancelledOnUserNavigation -> "PrefetchCancelledOnUserNavigation"

-- | Type 'Preload.PrerenderMismatchedHeaders'.
--   Information of headers to be displayed when the header mismatch occurred.
data PreloadPrerenderMismatchedHeaders = PreloadPrerenderMismatchedHeaders
  {
    preloadPrerenderMismatchedHeadersHeaderName :: T.Text,
    preloadPrerenderMismatchedHeadersInitialValue :: Maybe T.Text,
    preloadPrerenderMismatchedHeadersActivationValue :: Maybe T.Text
  }
  deriving (Eq, Show)
instance FromJSON PreloadPrerenderMismatchedHeaders where
  parseJSON = A.withObject "PreloadPrerenderMismatchedHeaders" $ \o -> PreloadPrerenderMismatchedHeaders
    <$> o A..: "headerName"
    <*> o A..:? "initialValue"
    <*> o A..:? "activationValue"
instance ToJSON PreloadPrerenderMismatchedHeaders where
  toJSON p = A.object $ catMaybes [
    ("headerName" A..=) <$> Just (preloadPrerenderMismatchedHeadersHeaderName p),
    ("initialValue" A..=) <$> (preloadPrerenderMismatchedHeadersInitialValue p),
    ("activationValue" A..=) <$> (preloadPrerenderMismatchedHeadersActivationValue p)
    ]

-- | Type of the 'Preload.ruleSetUpdated' event.
data PreloadRuleSetUpdated = PreloadRuleSetUpdated
  {
    preloadRuleSetUpdatedRuleSet :: PreloadRuleSet
  }
  deriving (Eq, Show)
instance FromJSON PreloadRuleSetUpdated where
  parseJSON = A.withObject "PreloadRuleSetUpdated" $ \o -> PreloadRuleSetUpdated
    <$> o A..: "ruleSet"
instance Event PreloadRuleSetUpdated where
  eventName _ = "Preload.ruleSetUpdated"

-- | Type of the 'Preload.ruleSetRemoved' event.
data PreloadRuleSetRemoved = PreloadRuleSetRemoved
  {
    preloadRuleSetRemovedId :: PreloadRuleSetId
  }
  deriving (Eq, Show)
instance FromJSON PreloadRuleSetRemoved where
  parseJSON = A.withObject "PreloadRuleSetRemoved" $ \o -> PreloadRuleSetRemoved
    <$> o A..: "id"
instance Event PreloadRuleSetRemoved where
  eventName _ = "Preload.ruleSetRemoved"

-- | Type of the 'Preload.preloadEnabledStateUpdated' event.
data PreloadPreloadEnabledStateUpdated = PreloadPreloadEnabledStateUpdated
  {
    preloadPreloadEnabledStateUpdatedDisabledByPreference :: Bool,
    preloadPreloadEnabledStateUpdatedDisabledByDataSaver :: Bool,
    preloadPreloadEnabledStateUpdatedDisabledByBatterySaver :: Bool,
    preloadPreloadEnabledStateUpdatedDisabledByHoldbackPrefetchSpeculationRules :: Bool,
    preloadPreloadEnabledStateUpdatedDisabledByHoldbackPrerenderSpeculationRules :: Bool
  }
  deriving (Eq, Show)
instance FromJSON PreloadPreloadEnabledStateUpdated where
  parseJSON = A.withObject "PreloadPreloadEnabledStateUpdated" $ \o -> PreloadPreloadEnabledStateUpdated
    <$> o A..: "disabledByPreference"
    <*> o A..: "disabledByDataSaver"
    <*> o A..: "disabledByBatterySaver"
    <*> o A..: "disabledByHoldbackPrefetchSpeculationRules"
    <*> o A..: "disabledByHoldbackPrerenderSpeculationRules"
instance Event PreloadPreloadEnabledStateUpdated where
  eventName _ = "Preload.preloadEnabledStateUpdated"

-- | Type of the 'Preload.prefetchStatusUpdated' event.
data PreloadPrefetchStatusUpdated = PreloadPrefetchStatusUpdated
  {
    preloadPrefetchStatusUpdatedKey :: PreloadPreloadingAttemptKey,
    preloadPrefetchStatusUpdatedPipelineId :: PreloadPreloadPipelineId,
    -- | The frame id of the frame initiating prefetch.
    preloadPrefetchStatusUpdatedInitiatingFrameId :: DOMNetworkEmulationPageSecurity.PageFrameId,
    preloadPrefetchStatusUpdatedPrefetchUrl :: T.Text,
    preloadPrefetchStatusUpdatedStatus :: PreloadPreloadingStatus,
    preloadPrefetchStatusUpdatedPrefetchStatus :: PreloadPrefetchStatus,
    preloadPrefetchStatusUpdatedRequestId :: DOMNetworkEmulationPageSecurity.NetworkRequestId
  }
  deriving (Eq, Show)
instance FromJSON PreloadPrefetchStatusUpdated where
  parseJSON = A.withObject "PreloadPrefetchStatusUpdated" $ \o -> PreloadPrefetchStatusUpdated
    <$> o A..: "key"
    <*> o A..: "pipelineId"
    <*> o A..: "initiatingFrameId"
    <*> o A..: "prefetchUrl"
    <*> o A..: "status"
    <*> o A..: "prefetchStatus"
    <*> o A..: "requestId"
instance Event PreloadPrefetchStatusUpdated where
  eventName _ = "Preload.prefetchStatusUpdated"

-- | Type of the 'Preload.prerenderStatusUpdated' event.
data PreloadPrerenderStatusUpdated = PreloadPrerenderStatusUpdated
  {
    preloadPrerenderStatusUpdatedKey :: PreloadPreloadingAttemptKey,
    preloadPrerenderStatusUpdatedPipelineId :: PreloadPreloadPipelineId,
    preloadPrerenderStatusUpdatedStatus :: PreloadPreloadingStatus,
    preloadPrerenderStatusUpdatedPrerenderStatus :: Maybe PreloadPrerenderFinalStatus,
    -- | This is used to give users more information about the name of Mojo interface
    --   that is incompatible with prerender and has caused the cancellation of the attempt.
    preloadPrerenderStatusUpdatedDisallowedMojoInterface :: Maybe T.Text,
    preloadPrerenderStatusUpdatedMismatchedHeaders :: Maybe [PreloadPrerenderMismatchedHeaders]
  }
  deriving (Eq, Show)
instance FromJSON PreloadPrerenderStatusUpdated where
  parseJSON = A.withObject "PreloadPrerenderStatusUpdated" $ \o -> PreloadPrerenderStatusUpdated
    <$> o A..: "key"
    <*> o A..: "pipelineId"
    <*> o A..: "status"
    <*> o A..:? "prerenderStatus"
    <*> o A..:? "disallowedMojoInterface"
    <*> o A..:? "mismatchedHeaders"
instance Event PreloadPrerenderStatusUpdated where
  eventName _ = "Preload.prerenderStatusUpdated"

-- | Type of the 'Preload.preloadingAttemptSourcesUpdated' event.
data PreloadPreloadingAttemptSourcesUpdated = PreloadPreloadingAttemptSourcesUpdated
  {
    preloadPreloadingAttemptSourcesUpdatedLoaderId :: DOMNetworkEmulationPageSecurity.NetworkLoaderId,
    preloadPreloadingAttemptSourcesUpdatedPreloadingAttemptSources :: [PreloadPreloadingAttemptSource]
  }
  deriving (Eq, Show)
instance FromJSON PreloadPreloadingAttemptSourcesUpdated where
  parseJSON = A.withObject "PreloadPreloadingAttemptSourcesUpdated" $ \o -> PreloadPreloadingAttemptSourcesUpdated
    <$> o A..: "loaderId"
    <*> o A..: "preloadingAttemptSources"
instance Event PreloadPreloadingAttemptSourcesUpdated where
  eventName _ = "Preload.preloadingAttemptSourcesUpdated"


-- | Parameters of the 'Preload.enable' command.
data PPreloadEnable = PPreloadEnable
  deriving (Eq, Show)
pPreloadEnable
  :: PPreloadEnable
pPreloadEnable
  = PPreloadEnable
instance ToJSON PPreloadEnable where
  toJSON _ = A.Null
instance Command PPreloadEnable where
  type CommandResponse PPreloadEnable = ()
  commandName _ = "Preload.enable"
  fromJSON = const . A.Success . const ()


-- | Parameters of the 'Preload.disable' command.
data PPreloadDisable = PPreloadDisable
  deriving (Eq, Show)
pPreloadDisable
  :: PPreloadDisable
pPreloadDisable
  = PPreloadDisable
instance ToJSON PPreloadDisable where
  toJSON _ = A.Null
instance Command PPreloadDisable where
  type CommandResponse PPreloadDisable = ()
  commandName _ = "Preload.disable"
  fromJSON = const . A.Success . const ()

