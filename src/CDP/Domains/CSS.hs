{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}


{- |
= CSS

This domain exposes CSS read/write operations. All CSS objects (stylesheets, rules, and styles)
have an associated `id` used in subsequent operations on the related object. Each object type has
a specific `id` structure, and those are not interchangeable between objects of different kinds.
CSS objects can be loaded using the `get*ForNode()` calls (which accept a DOM node id). A client
can also keep track of stylesheets via the `styleSheetAdded`/`styleSheetRemoved` events and
subsequently load the required stylesheet contents using the `getStyleSheet[Text]()` methods.
-}


module CDP.Domains.CSS (module CDP.Domains.CSS) where

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


-- | Type 'CSS.StyleSheetOrigin'.
--   Stylesheet type: "injected" for stylesheets injected via extension, "user-agent" for user-agent
--   stylesheets, "inspector" for stylesheets created by the inspector (i.e. those holding the "via
--   inspector" rules), "regular" for regular stylesheets.
data CSSStyleSheetOrigin = CSSStyleSheetOriginInjected | CSSStyleSheetOriginUserAgent | CSSStyleSheetOriginInspector | CSSStyleSheetOriginRegular
  deriving (Ord, Eq, Show, Read)
instance FromJSON CSSStyleSheetOrigin where
  parseJSON = A.withText "CSSStyleSheetOrigin" $ \v -> case v of
    "injected" -> pure CSSStyleSheetOriginInjected
    "user-agent" -> pure CSSStyleSheetOriginUserAgent
    "inspector" -> pure CSSStyleSheetOriginInspector
    "regular" -> pure CSSStyleSheetOriginRegular
    "_" -> fail "failed to parse CSSStyleSheetOrigin"
instance ToJSON CSSStyleSheetOrigin where
  toJSON v = A.String $ case v of
    CSSStyleSheetOriginInjected -> "injected"
    CSSStyleSheetOriginUserAgent -> "user-agent"
    CSSStyleSheetOriginInspector -> "inspector"
    CSSStyleSheetOriginRegular -> "regular"

-- | Type 'CSS.PseudoElementMatches'.
--   CSS rule collection for a single pseudo style.
data CSSPseudoElementMatches = CSSPseudoElementMatches
  {
    -- | Pseudo element type.
    cSSPseudoElementMatchesPseudoType :: DOMNetworkEmulationPageSecurity.DOMPseudoType,
    -- | Pseudo element custom ident.
    cSSPseudoElementMatchesPseudoIdentifier :: Maybe T.Text,
    -- | Matches of CSS rules applicable to the pseudo style.
    cSSPseudoElementMatchesMatches :: [CSSRuleMatch]
  }
  deriving (Eq, Show)
instance FromJSON CSSPseudoElementMatches where
  parseJSON = A.withObject "CSSPseudoElementMatches" $ \o -> CSSPseudoElementMatches
    <$> o A..: "pseudoType"
    <*> o A..:? "pseudoIdentifier"
    <*> o A..: "matches"
instance ToJSON CSSPseudoElementMatches where
  toJSON p = A.object $ catMaybes [
    ("pseudoType" A..=) <$> Just (cSSPseudoElementMatchesPseudoType p),
    ("pseudoIdentifier" A..=) <$> (cSSPseudoElementMatchesPseudoIdentifier p),
    ("matches" A..=) <$> Just (cSSPseudoElementMatchesMatches p)
    ]

-- | Type 'CSS.CSSAnimationStyle'.
--   CSS style coming from animations with the name of the animation.
data CSSCSSAnimationStyle = CSSCSSAnimationStyle
  {
    -- | The name of the animation.
    cSSCSSAnimationStyleName :: Maybe T.Text,
    -- | The style coming from the animation.
    cSSCSSAnimationStyleStyle :: CSSCSSStyle
  }
  deriving (Eq, Show)
instance FromJSON CSSCSSAnimationStyle where
  parseJSON = A.withObject "CSSCSSAnimationStyle" $ \o -> CSSCSSAnimationStyle
    <$> o A..:? "name"
    <*> o A..: "style"
instance ToJSON CSSCSSAnimationStyle where
  toJSON p = A.object $ catMaybes [
    ("name" A..=) <$> (cSSCSSAnimationStyleName p),
    ("style" A..=) <$> Just (cSSCSSAnimationStyleStyle p)
    ]

-- | Type 'CSS.InheritedStyleEntry'.
--   Inherited CSS rule collection from ancestor node.
data CSSInheritedStyleEntry = CSSInheritedStyleEntry
  {
    -- | The ancestor node's inline style, if any, in the style inheritance chain.
    cSSInheritedStyleEntryInlineStyle :: Maybe CSSCSSStyle,
    -- | Matches of CSS rules matching the ancestor node in the style inheritance chain.
    cSSInheritedStyleEntryMatchedCSSRules :: [CSSRuleMatch]
  }
  deriving (Eq, Show)
instance FromJSON CSSInheritedStyleEntry where
  parseJSON = A.withObject "CSSInheritedStyleEntry" $ \o -> CSSInheritedStyleEntry
    <$> o A..:? "inlineStyle"
    <*> o A..: "matchedCSSRules"
instance ToJSON CSSInheritedStyleEntry where
  toJSON p = A.object $ catMaybes [
    ("inlineStyle" A..=) <$> (cSSInheritedStyleEntryInlineStyle p),
    ("matchedCSSRules" A..=) <$> Just (cSSInheritedStyleEntryMatchedCSSRules p)
    ]

-- | Type 'CSS.InheritedAnimatedStyleEntry'.
--   Inherited CSS style collection for animated styles from ancestor node.
data CSSInheritedAnimatedStyleEntry = CSSInheritedAnimatedStyleEntry
  {
    -- | Styles coming from the animations of the ancestor, if any, in the style inheritance chain.
    cSSInheritedAnimatedStyleEntryAnimationStyles :: Maybe [CSSCSSAnimationStyle],
    -- | The style coming from the transitions of the ancestor, if any, in the style inheritance chain.
    cSSInheritedAnimatedStyleEntryTransitionsStyle :: Maybe CSSCSSStyle
  }
  deriving (Eq, Show)
instance FromJSON CSSInheritedAnimatedStyleEntry where
  parseJSON = A.withObject "CSSInheritedAnimatedStyleEntry" $ \o -> CSSInheritedAnimatedStyleEntry
    <$> o A..:? "animationStyles"
    <*> o A..:? "transitionsStyle"
instance ToJSON CSSInheritedAnimatedStyleEntry where
  toJSON p = A.object $ catMaybes [
    ("animationStyles" A..=) <$> (cSSInheritedAnimatedStyleEntryAnimationStyles p),
    ("transitionsStyle" A..=) <$> (cSSInheritedAnimatedStyleEntryTransitionsStyle p)
    ]

-- | Type 'CSS.InheritedPseudoElementMatches'.
--   Inherited pseudo element matches from pseudos of an ancestor node.
data CSSInheritedPseudoElementMatches = CSSInheritedPseudoElementMatches
  {
    -- | Matches of pseudo styles from the pseudos of an ancestor node.
    cSSInheritedPseudoElementMatchesPseudoElements :: [CSSPseudoElementMatches]
  }
  deriving (Eq, Show)
instance FromJSON CSSInheritedPseudoElementMatches where
  parseJSON = A.withObject "CSSInheritedPseudoElementMatches" $ \o -> CSSInheritedPseudoElementMatches
    <$> o A..: "pseudoElements"
instance ToJSON CSSInheritedPseudoElementMatches where
  toJSON p = A.object $ catMaybes [
    ("pseudoElements" A..=) <$> Just (cSSInheritedPseudoElementMatchesPseudoElements p)
    ]

-- | Type 'CSS.RuleMatch'.
--   Match data for a CSS rule.
data CSSRuleMatch = CSSRuleMatch
  {
    -- | CSS rule in the match.
    cSSRuleMatchRule :: CSSCSSRule,
    -- | Matching selector indices in the rule's selectorList selectors (0-based).
    cSSRuleMatchMatchingSelectors :: [Int]
  }
  deriving (Eq, Show)
instance FromJSON CSSRuleMatch where
  parseJSON = A.withObject "CSSRuleMatch" $ \o -> CSSRuleMatch
    <$> o A..: "rule"
    <*> o A..: "matchingSelectors"
instance ToJSON CSSRuleMatch where
  toJSON p = A.object $ catMaybes [
    ("rule" A..=) <$> Just (cSSRuleMatchRule p),
    ("matchingSelectors" A..=) <$> Just (cSSRuleMatchMatchingSelectors p)
    ]

-- | Type 'CSS.Value'.
--   Data for a simple selector (these are delimited by commas in a selector list).
data CSSValue = CSSValue
  {
    -- | Value text.
    cSSValueText :: T.Text,
    -- | Value range in the underlying resource (if available).
    cSSValueRange :: Maybe CSSSourceRange,
    -- | Specificity of the selector.
    cSSValueSpecificity :: Maybe CSSSpecificity
  }
  deriving (Eq, Show)
instance FromJSON CSSValue where
  parseJSON = A.withObject "CSSValue" $ \o -> CSSValue
    <$> o A..: "text"
    <*> o A..:? "range"
    <*> o A..:? "specificity"
instance ToJSON CSSValue where
  toJSON p = A.object $ catMaybes [
    ("text" A..=) <$> Just (cSSValueText p),
    ("range" A..=) <$> (cSSValueRange p),
    ("specificity" A..=) <$> (cSSValueSpecificity p)
    ]

-- | Type 'CSS.SpecificityComponent'.
--   Contribution of an individual simple selector to specificity.
data CSSSpecificityComponent = CSSSpecificityComponent
  {
    -- | The simple selector text that contributes to specificity.
    cSSSpecificityComponentText :: T.Text,
    -- | The a component contribution.
    cSSSpecificityComponentA :: Int,
    -- | The b component contribution.
    cSSSpecificityComponentB :: Int,
    -- | The c component contribution.
    cSSSpecificityComponentC :: Int
  }
  deriving (Eq, Show)
instance FromJSON CSSSpecificityComponent where
  parseJSON = A.withObject "CSSSpecificityComponent" $ \o -> CSSSpecificityComponent
    <$> o A..: "text"
    <*> o A..: "a"
    <*> o A..: "b"
    <*> o A..: "c"
instance ToJSON CSSSpecificityComponent where
  toJSON p = A.object $ catMaybes [
    ("text" A..=) <$> Just (cSSSpecificityComponentText p),
    ("a" A..=) <$> Just (cSSSpecificityComponentA p),
    ("b" A..=) <$> Just (cSSSpecificityComponentB p),
    ("c" A..=) <$> Just (cSSSpecificityComponentC p)
    ]

-- | Type 'CSS.Specificity'.
--   Specificity:
--   https://drafts.csswg.org/selectors/#specificity-rules
data CSSSpecificity = CSSSpecificity
  {
    -- | The a component, which represents the number of ID selectors.
    cSSSpecificityA :: Int,
    -- | The b component, which represents the number of class selectors, attributes selectors, and
    --   pseudo-classes.
    cSSSpecificityB :: Int,
    -- | The c component, which represents the number of type selectors and pseudo-elements.
    cSSSpecificityC :: Int,
    -- | Per-simple-selector contributions used to explain this specificity.
    cSSSpecificityComponents :: Maybe [CSSSpecificityComponent]
  }
  deriving (Eq, Show)
instance FromJSON CSSSpecificity where
  parseJSON = A.withObject "CSSSpecificity" $ \o -> CSSSpecificity
    <$> o A..: "a"
    <*> o A..: "b"
    <*> o A..: "c"
    <*> o A..:? "components"
instance ToJSON CSSSpecificity where
  toJSON p = A.object $ catMaybes [
    ("a" A..=) <$> Just (cSSSpecificityA p),
    ("b" A..=) <$> Just (cSSSpecificityB p),
    ("c" A..=) <$> Just (cSSSpecificityC p),
    ("components" A..=) <$> (cSSSpecificityComponents p)
    ]

-- | Type 'CSS.SelectorList'.
--   Selector list data.
data CSSSelectorList = CSSSelectorList
  {
    -- | Selectors in the list.
    cSSSelectorListSelectors :: [CSSValue],
    -- | Rule selector text.
    cSSSelectorListText :: T.Text
  }
  deriving (Eq, Show)
instance FromJSON CSSSelectorList where
  parseJSON = A.withObject "CSSSelectorList" $ \o -> CSSSelectorList
    <$> o A..: "selectors"
    <*> o A..: "text"
instance ToJSON CSSSelectorList where
  toJSON p = A.object $ catMaybes [
    ("selectors" A..=) <$> Just (cSSSelectorListSelectors p),
    ("text" A..=) <$> Just (cSSSelectorListText p)
    ]

-- | Type 'CSS.CSSStyleSheetHeader'.
--   CSS stylesheet metainformation.
data CSSCSSStyleSheetHeader = CSSCSSStyleSheetHeader
  {
    -- | The stylesheet identifier.
    cSSCSSStyleSheetHeaderStyleSheetId :: DOMNetworkEmulationPageSecurity.DOMStyleSheetId,
    -- | Owner frame identifier.
    cSSCSSStyleSheetHeaderFrameId :: DOMNetworkEmulationPageSecurity.PageFrameId,
    -- | Stylesheet resource URL. Empty if this is a constructed stylesheet created using
    --   new CSSStyleSheet() (but non-empty if this is a constructed stylesheet imported
    --   as a CSS module script).
    cSSCSSStyleSheetHeaderSourceURL :: T.Text,
    -- | URL of source map associated with the stylesheet (if any).
    cSSCSSStyleSheetHeaderSourceMapURL :: Maybe T.Text,
    -- | Stylesheet origin.
    cSSCSSStyleSheetHeaderOrigin :: CSSStyleSheetOrigin,
    -- | Stylesheet title.
    cSSCSSStyleSheetHeaderTitle :: T.Text,
    -- | The backend id for the owner node of the stylesheet.
    cSSCSSStyleSheetHeaderOwnerNode :: Maybe DOMNetworkEmulationPageSecurity.DOMBackendNodeId,
    -- | Denotes whether the stylesheet is disabled.
    cSSCSSStyleSheetHeaderDisabled :: Bool,
    -- | Whether the sourceURL field value comes from the sourceURL comment.
    cSSCSSStyleSheetHeaderHasSourceURL :: Maybe Bool,
    -- | Whether this stylesheet is created for STYLE tag by parser. This flag is not set for
    --   document.written STYLE tags.
    cSSCSSStyleSheetHeaderIsInline :: Bool,
    -- | Whether this stylesheet is mutable. Inline stylesheets become mutable
    --   after they have been modified via CSSOM API.
    --   `<link>` element's stylesheets become mutable only if DevTools modifies them.
    --   Constructed stylesheets (new CSSStyleSheet()) are mutable immediately after creation.
    cSSCSSStyleSheetHeaderIsMutable :: Bool,
    -- | True if this stylesheet is created through new CSSStyleSheet() or imported as a
    --   CSS module script.
    cSSCSSStyleSheetHeaderIsConstructed :: Bool,
    -- | Line offset of the stylesheet within the resource (zero based).
    cSSCSSStyleSheetHeaderStartLine :: Double,
    -- | Column offset of the stylesheet within the resource (zero based).
    cSSCSSStyleSheetHeaderStartColumn :: Double,
    -- | Size of the content (in characters).
    cSSCSSStyleSheetHeaderLength :: Double,
    -- | Line offset of the end of the stylesheet within the resource (zero based).
    cSSCSSStyleSheetHeaderEndLine :: Double,
    -- | Column offset of the end of the stylesheet within the resource (zero based).
    cSSCSSStyleSheetHeaderEndColumn :: Double,
    -- | If the style sheet was loaded from a network resource, this indicates when the resource failed to load
    cSSCSSStyleSheetHeaderLoadingFailed :: Maybe Bool
  }
  deriving (Eq, Show)
instance FromJSON CSSCSSStyleSheetHeader where
  parseJSON = A.withObject "CSSCSSStyleSheetHeader" $ \o -> CSSCSSStyleSheetHeader
    <$> o A..: "styleSheetId"
    <*> o A..: "frameId"
    <*> o A..: "sourceURL"
    <*> o A..:? "sourceMapURL"
    <*> o A..: "origin"
    <*> o A..: "title"
    <*> o A..:? "ownerNode"
    <*> o A..: "disabled"
    <*> o A..:? "hasSourceURL"
    <*> o A..: "isInline"
    <*> o A..: "isMutable"
    <*> o A..: "isConstructed"
    <*> o A..: "startLine"
    <*> o A..: "startColumn"
    <*> o A..: "length"
    <*> o A..: "endLine"
    <*> o A..: "endColumn"
    <*> o A..:? "loadingFailed"
instance ToJSON CSSCSSStyleSheetHeader where
  toJSON p = A.object $ catMaybes [
    ("styleSheetId" A..=) <$> Just (cSSCSSStyleSheetHeaderStyleSheetId p),
    ("frameId" A..=) <$> Just (cSSCSSStyleSheetHeaderFrameId p),
    ("sourceURL" A..=) <$> Just (cSSCSSStyleSheetHeaderSourceURL p),
    ("sourceMapURL" A..=) <$> (cSSCSSStyleSheetHeaderSourceMapURL p),
    ("origin" A..=) <$> Just (cSSCSSStyleSheetHeaderOrigin p),
    ("title" A..=) <$> Just (cSSCSSStyleSheetHeaderTitle p),
    ("ownerNode" A..=) <$> (cSSCSSStyleSheetHeaderOwnerNode p),
    ("disabled" A..=) <$> Just (cSSCSSStyleSheetHeaderDisabled p),
    ("hasSourceURL" A..=) <$> (cSSCSSStyleSheetHeaderHasSourceURL p),
    ("isInline" A..=) <$> Just (cSSCSSStyleSheetHeaderIsInline p),
    ("isMutable" A..=) <$> Just (cSSCSSStyleSheetHeaderIsMutable p),
    ("isConstructed" A..=) <$> Just (cSSCSSStyleSheetHeaderIsConstructed p),
    ("startLine" A..=) <$> Just (cSSCSSStyleSheetHeaderStartLine p),
    ("startColumn" A..=) <$> Just (cSSCSSStyleSheetHeaderStartColumn p),
    ("length" A..=) <$> Just (cSSCSSStyleSheetHeaderLength p),
    ("endLine" A..=) <$> Just (cSSCSSStyleSheetHeaderEndLine p),
    ("endColumn" A..=) <$> Just (cSSCSSStyleSheetHeaderEndColumn p),
    ("loadingFailed" A..=) <$> (cSSCSSStyleSheetHeaderLoadingFailed p)
    ]

-- | Type 'CSS.CSSRule'.
--   CSS rule representation.
data CSSCSSRule = CSSCSSRule
  {
    -- | The css style sheet identifier (absent for user agent stylesheet and user-specified
    --   stylesheet rules) this rule came from.
    cSSCSSRuleStyleSheetId :: Maybe DOMNetworkEmulationPageSecurity.DOMStyleSheetId,
    -- | Rule selector data.
    cSSCSSRuleSelectorList :: CSSSelectorList,
    -- | Array of selectors from ancestor style rules, sorted by distance from the current rule.
    cSSCSSRuleNestingSelectors :: Maybe [T.Text],
    -- | Parent stylesheet's origin.
    cSSCSSRuleOrigin :: CSSStyleSheetOrigin,
    -- | Associated style declaration.
    cSSCSSRuleStyle :: CSSCSSStyle,
    -- | The BackendNodeId of the DOM node that constitutes the origin tree scope of this rule.
    cSSCSSRuleOriginTreeScopeNodeId :: Maybe DOMNetworkEmulationPageSecurity.DOMBackendNodeId,
    -- | Media list array (for rules involving media queries). The array enumerates media queries
    --   starting with the innermost one, going outwards.
    cSSCSSRuleMedia :: Maybe [CSSCSSMedia],
    -- | Container query list array (for rules involving container queries).
    --   The array enumerates container queries starting with the innermost one, going outwards.
    cSSCSSRuleContainerQueries :: Maybe [CSSCSSContainerQuery],
    -- | @supports CSS at-rule array.
    --   The array enumerates @supports at-rules starting with the innermost one, going outwards.
    cSSCSSRuleSupports :: Maybe [CSSCSSSupports],
    -- | Cascade layer array. Contains the layer hierarchy that this rule belongs to starting
    --   with the innermost layer and going outwards.
    cSSCSSRuleLayers :: Maybe [CSSCSSLayer],
    -- | @scope CSS at-rule array.
    --   The array enumerates @scope at-rules starting with the innermost one, going outwards.
    cSSCSSRuleScopes :: Maybe [CSSCSSScope],
    -- | The array keeps the types of ancestor CSSRules from the innermost going outwards.
    cSSCSSRuleRuleTypes :: Maybe [CSSCSSRuleType],
    -- | @starting-style CSS at-rule array.
    --   The array enumerates @starting-style at-rules starting with the innermost one, going outwards.
    cSSCSSRuleStartingStyles :: Maybe [CSSCSSStartingStyle],
    -- | @navigation CSS at-rule array.
    --   The array enumerates @navigation at-rules starting with the innermost one, going outwards.
    cSSCSSRuleNavigations :: Maybe [CSSCSSNavigation]
  }
  deriving (Eq, Show)
instance FromJSON CSSCSSRule where
  parseJSON = A.withObject "CSSCSSRule" $ \o -> CSSCSSRule
    <$> o A..:? "styleSheetId"
    <*> o A..: "selectorList"
    <*> o A..:? "nestingSelectors"
    <*> o A..: "origin"
    <*> o A..: "style"
    <*> o A..:? "originTreeScopeNodeId"
    <*> o A..:? "media"
    <*> o A..:? "containerQueries"
    <*> o A..:? "supports"
    <*> o A..:? "layers"
    <*> o A..:? "scopes"
    <*> o A..:? "ruleTypes"
    <*> o A..:? "startingStyles"
    <*> o A..:? "navigations"
instance ToJSON CSSCSSRule where
  toJSON p = A.object $ catMaybes [
    ("styleSheetId" A..=) <$> (cSSCSSRuleStyleSheetId p),
    ("selectorList" A..=) <$> Just (cSSCSSRuleSelectorList p),
    ("nestingSelectors" A..=) <$> (cSSCSSRuleNestingSelectors p),
    ("origin" A..=) <$> Just (cSSCSSRuleOrigin p),
    ("style" A..=) <$> Just (cSSCSSRuleStyle p),
    ("originTreeScopeNodeId" A..=) <$> (cSSCSSRuleOriginTreeScopeNodeId p),
    ("media" A..=) <$> (cSSCSSRuleMedia p),
    ("containerQueries" A..=) <$> (cSSCSSRuleContainerQueries p),
    ("supports" A..=) <$> (cSSCSSRuleSupports p),
    ("layers" A..=) <$> (cSSCSSRuleLayers p),
    ("scopes" A..=) <$> (cSSCSSRuleScopes p),
    ("ruleTypes" A..=) <$> (cSSCSSRuleRuleTypes p),
    ("startingStyles" A..=) <$> (cSSCSSRuleStartingStyles p),
    ("navigations" A..=) <$> (cSSCSSRuleNavigations p)
    ]

-- | Type 'CSS.CSSRuleType'.
--   Enum indicating the type of a CSS rule, used to represent the order of a style rule's ancestors.
--   This list only contains rule types that are collected during the ancestor rule collection.
data CSSCSSRuleType = CSSCSSRuleTypeMediaRule | CSSCSSRuleTypeSupportsRule | CSSCSSRuleTypeContainerRule | CSSCSSRuleTypeLayerRule | CSSCSSRuleTypeScopeRule | CSSCSSRuleTypeStyleRule | CSSCSSRuleTypeStartingStyleRule | CSSCSSRuleTypeNavigationRule
  deriving (Ord, Eq, Show, Read)
instance FromJSON CSSCSSRuleType where
  parseJSON = A.withText "CSSCSSRuleType" $ \v -> case v of
    "MediaRule" -> pure CSSCSSRuleTypeMediaRule
    "SupportsRule" -> pure CSSCSSRuleTypeSupportsRule
    "ContainerRule" -> pure CSSCSSRuleTypeContainerRule
    "LayerRule" -> pure CSSCSSRuleTypeLayerRule
    "ScopeRule" -> pure CSSCSSRuleTypeScopeRule
    "StyleRule" -> pure CSSCSSRuleTypeStyleRule
    "StartingStyleRule" -> pure CSSCSSRuleTypeStartingStyleRule
    "NavigationRule" -> pure CSSCSSRuleTypeNavigationRule
    "_" -> fail "failed to parse CSSCSSRuleType"
instance ToJSON CSSCSSRuleType where
  toJSON v = A.String $ case v of
    CSSCSSRuleTypeMediaRule -> "MediaRule"
    CSSCSSRuleTypeSupportsRule -> "SupportsRule"
    CSSCSSRuleTypeContainerRule -> "ContainerRule"
    CSSCSSRuleTypeLayerRule -> "LayerRule"
    CSSCSSRuleTypeScopeRule -> "ScopeRule"
    CSSCSSRuleTypeStyleRule -> "StyleRule"
    CSSCSSRuleTypeStartingStyleRule -> "StartingStyleRule"
    CSSCSSRuleTypeNavigationRule -> "NavigationRule"

-- | Type 'CSS.RuleUsage'.
--   CSS coverage information.
data CSSRuleUsage = CSSRuleUsage
  {
    -- | The css style sheet identifier (absent for user agent stylesheet and user-specified
    --   stylesheet rules) this rule came from.
    cSSRuleUsageStyleSheetId :: DOMNetworkEmulationPageSecurity.DOMStyleSheetId,
    -- | Offset of the start of the rule (including selector) from the beginning of the stylesheet.
    cSSRuleUsageStartOffset :: Double,
    -- | Offset of the end of the rule body from the beginning of the stylesheet.
    cSSRuleUsageEndOffset :: Double,
    -- | Indicates whether the rule was actually used by some element in the page.
    cSSRuleUsageUsed :: Bool
  }
  deriving (Eq, Show)
instance FromJSON CSSRuleUsage where
  parseJSON = A.withObject "CSSRuleUsage" $ \o -> CSSRuleUsage
    <$> o A..: "styleSheetId"
    <*> o A..: "startOffset"
    <*> o A..: "endOffset"
    <*> o A..: "used"
instance ToJSON CSSRuleUsage where
  toJSON p = A.object $ catMaybes [
    ("styleSheetId" A..=) <$> Just (cSSRuleUsageStyleSheetId p),
    ("startOffset" A..=) <$> Just (cSSRuleUsageStartOffset p),
    ("endOffset" A..=) <$> Just (cSSRuleUsageEndOffset p),
    ("used" A..=) <$> Just (cSSRuleUsageUsed p)
    ]

-- | Type 'CSS.SourceRange'.
--   Text range within a resource. All numbers are zero-based.
data CSSSourceRange = CSSSourceRange
  {
    -- | Start line of range.
    cSSSourceRangeStartLine :: Int,
    -- | Start column of range (inclusive).
    cSSSourceRangeStartColumn :: Int,
    -- | End line of range
    cSSSourceRangeEndLine :: Int,
    -- | End column of range (exclusive).
    cSSSourceRangeEndColumn :: Int
  }
  deriving (Eq, Show)
instance FromJSON CSSSourceRange where
  parseJSON = A.withObject "CSSSourceRange" $ \o -> CSSSourceRange
    <$> o A..: "startLine"
    <*> o A..: "startColumn"
    <*> o A..: "endLine"
    <*> o A..: "endColumn"
instance ToJSON CSSSourceRange where
  toJSON p = A.object $ catMaybes [
    ("startLine" A..=) <$> Just (cSSSourceRangeStartLine p),
    ("startColumn" A..=) <$> Just (cSSSourceRangeStartColumn p),
    ("endLine" A..=) <$> Just (cSSSourceRangeEndLine p),
    ("endColumn" A..=) <$> Just (cSSSourceRangeEndColumn p)
    ]

-- | Type 'CSS.ShorthandEntry'.
data CSSShorthandEntry = CSSShorthandEntry
  {
    -- | Shorthand name.
    cSSShorthandEntryName :: T.Text,
    -- | Shorthand value.
    cSSShorthandEntryValue :: T.Text,
    -- | Whether the property has "!important" annotation (implies `false` if absent).
    cSSShorthandEntryImportant :: Maybe Bool
  }
  deriving (Eq, Show)
instance FromJSON CSSShorthandEntry where
  parseJSON = A.withObject "CSSShorthandEntry" $ \o -> CSSShorthandEntry
    <$> o A..: "name"
    <*> o A..: "value"
    <*> o A..:? "important"
instance ToJSON CSSShorthandEntry where
  toJSON p = A.object $ catMaybes [
    ("name" A..=) <$> Just (cSSShorthandEntryName p),
    ("value" A..=) <$> Just (cSSShorthandEntryValue p),
    ("important" A..=) <$> (cSSShorthandEntryImportant p)
    ]

-- | Type 'CSS.CSSComputedStyleProperty'.
data CSSCSSComputedStyleProperty = CSSCSSComputedStyleProperty
  {
    -- | Computed style property name.
    cSSCSSComputedStylePropertyName :: T.Text,
    -- | Computed style property value.
    cSSCSSComputedStylePropertyValue :: T.Text
  }
  deriving (Eq, Show)
instance FromJSON CSSCSSComputedStyleProperty where
  parseJSON = A.withObject "CSSCSSComputedStyleProperty" $ \o -> CSSCSSComputedStyleProperty
    <$> o A..: "name"
    <*> o A..: "value"
instance ToJSON CSSCSSComputedStyleProperty where
  toJSON p = A.object $ catMaybes [
    ("name" A..=) <$> Just (cSSCSSComputedStylePropertyName p),
    ("value" A..=) <$> Just (cSSCSSComputedStylePropertyValue p)
    ]

-- | Type 'CSS.ComputedStyleExtraFields'.
data CSSComputedStyleExtraFields = CSSComputedStyleExtraFields
  {
    -- | Returns whether or not this node is being rendered with base appearance,
    --   which happens when it has its appearance property set to base/base-select
    --   or it is in the subtree of an element being rendered with base appearance.
    cSSComputedStyleExtraFieldsIsAppearanceBase :: Bool
  }
  deriving (Eq, Show)
instance FromJSON CSSComputedStyleExtraFields where
  parseJSON = A.withObject "CSSComputedStyleExtraFields" $ \o -> CSSComputedStyleExtraFields
    <$> o A..: "isAppearanceBase"
instance ToJSON CSSComputedStyleExtraFields where
  toJSON p = A.object $ catMaybes [
    ("isAppearanceBase" A..=) <$> Just (cSSComputedStyleExtraFieldsIsAppearanceBase p)
    ]

-- | Type 'CSS.CSSStyle'.
--   CSS style representation.
data CSSCSSStyle = CSSCSSStyle
  {
    -- | The css style sheet identifier (absent for user agent stylesheet and user-specified
    --   stylesheet rules) this rule came from.
    cSSCSSStyleStyleSheetId :: Maybe DOMNetworkEmulationPageSecurity.DOMStyleSheetId,
    -- | CSS properties in the style.
    cSSCSSStyleCssProperties :: [CSSCSSProperty],
    -- | Computed values for all shorthands found in the style.
    cSSCSSStyleShorthandEntries :: [CSSShorthandEntry],
    -- | Style declaration text (if available).
    cSSCSSStyleCssText :: Maybe T.Text,
    -- | Style declaration range in the enclosing stylesheet (if available).
    cSSCSSStyleRange :: Maybe CSSSourceRange
  }
  deriving (Eq, Show)
instance FromJSON CSSCSSStyle where
  parseJSON = A.withObject "CSSCSSStyle" $ \o -> CSSCSSStyle
    <$> o A..:? "styleSheetId"
    <*> o A..: "cssProperties"
    <*> o A..: "shorthandEntries"
    <*> o A..:? "cssText"
    <*> o A..:? "range"
instance ToJSON CSSCSSStyle where
  toJSON p = A.object $ catMaybes [
    ("styleSheetId" A..=) <$> (cSSCSSStyleStyleSheetId p),
    ("cssProperties" A..=) <$> Just (cSSCSSStyleCssProperties p),
    ("shorthandEntries" A..=) <$> Just (cSSCSSStyleShorthandEntries p),
    ("cssText" A..=) <$> (cSSCSSStyleCssText p),
    ("range" A..=) <$> (cSSCSSStyleRange p)
    ]

-- | Type 'CSS.CSSProperty'.
--   CSS property declaration data.
data CSSCSSProperty = CSSCSSProperty
  {
    -- | The property name.
    cSSCSSPropertyName :: T.Text,
    -- | The property value.
    cSSCSSPropertyValue :: T.Text,
    -- | Whether the property has "!important" annotation (implies `false` if absent).
    cSSCSSPropertyImportant :: Maybe Bool,
    -- | Whether the property is implicit (implies `false` if absent).
    cSSCSSPropertyImplicit :: Maybe Bool,
    -- | The full property text as specified in the style.
    cSSCSSPropertyText :: Maybe T.Text,
    -- | Whether the property is understood by the browser (implies `true` if absent).
    cSSCSSPropertyParsedOk :: Maybe Bool,
    -- | Whether the property is disabled by the user (present for source-based properties only).
    cSSCSSPropertyDisabled :: Maybe Bool,
    -- | The entire property range in the enclosing style declaration (if available).
    cSSCSSPropertyRange :: Maybe CSSSourceRange,
    -- | Parsed longhand components of this property if it is a shorthand.
    --   This field will be empty if the given property is not a shorthand.
    cSSCSSPropertyLonghandProperties :: Maybe [CSSCSSProperty]
  }
  deriving (Eq, Show)
instance FromJSON CSSCSSProperty where
  parseJSON = A.withObject "CSSCSSProperty" $ \o -> CSSCSSProperty
    <$> o A..: "name"
    <*> o A..: "value"
    <*> o A..:? "important"
    <*> o A..:? "implicit"
    <*> o A..:? "text"
    <*> o A..:? "parsedOk"
    <*> o A..:? "disabled"
    <*> o A..:? "range"
    <*> o A..:? "longhandProperties"
instance ToJSON CSSCSSProperty where
  toJSON p = A.object $ catMaybes [
    ("name" A..=) <$> Just (cSSCSSPropertyName p),
    ("value" A..=) <$> Just (cSSCSSPropertyValue p),
    ("important" A..=) <$> (cSSCSSPropertyImportant p),
    ("implicit" A..=) <$> (cSSCSSPropertyImplicit p),
    ("text" A..=) <$> (cSSCSSPropertyText p),
    ("parsedOk" A..=) <$> (cSSCSSPropertyParsedOk p),
    ("disabled" A..=) <$> (cSSCSSPropertyDisabled p),
    ("range" A..=) <$> (cSSCSSPropertyRange p),
    ("longhandProperties" A..=) <$> (cSSCSSPropertyLonghandProperties p)
    ]

-- | Type 'CSS.CSSMedia'.
--   CSS media rule descriptor.
data CSSCSSMediaSource = CSSCSSMediaSourceMediaRule | CSSCSSMediaSourceImportRule | CSSCSSMediaSourceLinkedSheet | CSSCSSMediaSourceInlineSheet
  deriving (Ord, Eq, Show, Read)
instance FromJSON CSSCSSMediaSource where
  parseJSON = A.withText "CSSCSSMediaSource" $ \v -> case v of
    "mediaRule" -> pure CSSCSSMediaSourceMediaRule
    "importRule" -> pure CSSCSSMediaSourceImportRule
    "linkedSheet" -> pure CSSCSSMediaSourceLinkedSheet
    "inlineSheet" -> pure CSSCSSMediaSourceInlineSheet
    "_" -> fail "failed to parse CSSCSSMediaSource"
instance ToJSON CSSCSSMediaSource where
  toJSON v = A.String $ case v of
    CSSCSSMediaSourceMediaRule -> "mediaRule"
    CSSCSSMediaSourceImportRule -> "importRule"
    CSSCSSMediaSourceLinkedSheet -> "linkedSheet"
    CSSCSSMediaSourceInlineSheet -> "inlineSheet"
data CSSCSSMedia = CSSCSSMedia
  {
    -- | Media query text.
    cSSCSSMediaText :: T.Text,
    -- | Source of the media query: "mediaRule" if specified by a @media rule, "importRule" if
    --   specified by an @import rule, "linkedSheet" if specified by a "media" attribute in a linked
    --   stylesheet's LINK tag, "inlineSheet" if specified by a "media" attribute in an inline
    --   stylesheet's STYLE tag.
    cSSCSSMediaSource :: CSSCSSMediaSource,
    -- | URL of the document containing the media query description.
    cSSCSSMediaSourceURL :: Maybe T.Text,
    -- | The associated rule (@media or @import) header range in the enclosing stylesheet (if
    --   available).
    cSSCSSMediaRange :: Maybe CSSSourceRange,
    -- | Identifier of the stylesheet containing this object (if exists).
    cSSCSSMediaStyleSheetId :: Maybe DOMNetworkEmulationPageSecurity.DOMStyleSheetId,
    -- | Array of media queries.
    cSSCSSMediaMediaList :: Maybe [CSSMediaQuery]
  }
  deriving (Eq, Show)
instance FromJSON CSSCSSMedia where
  parseJSON = A.withObject "CSSCSSMedia" $ \o -> CSSCSSMedia
    <$> o A..: "text"
    <*> o A..: "source"
    <*> o A..:? "sourceURL"
    <*> o A..:? "range"
    <*> o A..:? "styleSheetId"
    <*> o A..:? "mediaList"
instance ToJSON CSSCSSMedia where
  toJSON p = A.object $ catMaybes [
    ("text" A..=) <$> Just (cSSCSSMediaText p),
    ("source" A..=) <$> Just (cSSCSSMediaSource p),
    ("sourceURL" A..=) <$> (cSSCSSMediaSourceURL p),
    ("range" A..=) <$> (cSSCSSMediaRange p),
    ("styleSheetId" A..=) <$> (cSSCSSMediaStyleSheetId p),
    ("mediaList" A..=) <$> (cSSCSSMediaMediaList p)
    ]

-- | Type 'CSS.MediaQuery'.
--   Media query descriptor.
data CSSMediaQuery = CSSMediaQuery
  {
    -- | Array of media query expressions.
    cSSMediaQueryExpressions :: [CSSMediaQueryExpression],
    -- | Whether the media query condition is satisfied.
    cSSMediaQueryActive :: Bool
  }
  deriving (Eq, Show)
instance FromJSON CSSMediaQuery where
  parseJSON = A.withObject "CSSMediaQuery" $ \o -> CSSMediaQuery
    <$> o A..: "expressions"
    <*> o A..: "active"
instance ToJSON CSSMediaQuery where
  toJSON p = A.object $ catMaybes [
    ("expressions" A..=) <$> Just (cSSMediaQueryExpressions p),
    ("active" A..=) <$> Just (cSSMediaQueryActive p)
    ]

-- | Type 'CSS.MediaQueryExpression'.
--   Media query expression descriptor.
data CSSMediaQueryExpression = CSSMediaQueryExpression
  {
    -- | Media query expression value.
    cSSMediaQueryExpressionValue :: Double,
    -- | Media query expression units.
    cSSMediaQueryExpressionUnit :: T.Text,
    -- | Media query expression feature.
    cSSMediaQueryExpressionFeature :: T.Text,
    -- | The associated range of the value text in the enclosing stylesheet (if available).
    cSSMediaQueryExpressionValueRange :: Maybe CSSSourceRange,
    -- | Computed length of media query expression (if applicable).
    cSSMediaQueryExpressionComputedLength :: Maybe Double
  }
  deriving (Eq, Show)
instance FromJSON CSSMediaQueryExpression where
  parseJSON = A.withObject "CSSMediaQueryExpression" $ \o -> CSSMediaQueryExpression
    <$> o A..: "value"
    <*> o A..: "unit"
    <*> o A..: "feature"
    <*> o A..:? "valueRange"
    <*> o A..:? "computedLength"
instance ToJSON CSSMediaQueryExpression where
  toJSON p = A.object $ catMaybes [
    ("value" A..=) <$> Just (cSSMediaQueryExpressionValue p),
    ("unit" A..=) <$> Just (cSSMediaQueryExpressionUnit p),
    ("feature" A..=) <$> Just (cSSMediaQueryExpressionFeature p),
    ("valueRange" A..=) <$> (cSSMediaQueryExpressionValueRange p),
    ("computedLength" A..=) <$> (cSSMediaQueryExpressionComputedLength p)
    ]

-- | Type 'CSS.CSSContainerQuery'.
--   CSS container query rule descriptor.
data CSSCSSContainerQuery = CSSCSSContainerQuery
  {
    -- | The associated rule header range in the enclosing stylesheet (if
    --   available).
    cSSCSSContainerQueryRange :: Maybe CSSSourceRange,
    -- | Identifier of the stylesheet containing this object (if exists).
    cSSCSSContainerQueryStyleSheetId :: Maybe DOMNetworkEmulationPageSecurity.DOMStyleSheetId,
    -- | Optional name for the container.
    cSSCSSContainerQueryName :: Maybe T.Text,
    -- | Optional physical axes queried for the container.
    cSSCSSContainerQueryPhysicalAxes :: Maybe DOMNetworkEmulationPageSecurity.DOMPhysicalAxes,
    -- | Optional logical axes queried for the container.
    cSSCSSContainerQueryLogicalAxes :: Maybe DOMNetworkEmulationPageSecurity.DOMLogicalAxes,
    -- | true if the query contains scroll-state() queries.
    cSSCSSContainerQueryQueriesScrollState :: Maybe Bool,
    -- | true if the query contains anchored() queries.
    cSSCSSContainerQueryQueriesAnchored :: Maybe Bool,
    -- | CSSContainerRule.conditionText
    cSSCSSContainerQueryConditionText :: T.Text
  }
  deriving (Eq, Show)
instance FromJSON CSSCSSContainerQuery where
  parseJSON = A.withObject "CSSCSSContainerQuery" $ \o -> CSSCSSContainerQuery
    <$> o A..:? "range"
    <*> o A..:? "styleSheetId"
    <*> o A..:? "name"
    <*> o A..:? "physicalAxes"
    <*> o A..:? "logicalAxes"
    <*> o A..:? "queriesScrollState"
    <*> o A..:? "queriesAnchored"
    <*> o A..: "conditionText"
instance ToJSON CSSCSSContainerQuery where
  toJSON p = A.object $ catMaybes [
    ("range" A..=) <$> (cSSCSSContainerQueryRange p),
    ("styleSheetId" A..=) <$> (cSSCSSContainerQueryStyleSheetId p),
    ("name" A..=) <$> (cSSCSSContainerQueryName p),
    ("physicalAxes" A..=) <$> (cSSCSSContainerQueryPhysicalAxes p),
    ("logicalAxes" A..=) <$> (cSSCSSContainerQueryLogicalAxes p),
    ("queriesScrollState" A..=) <$> (cSSCSSContainerQueryQueriesScrollState p),
    ("queriesAnchored" A..=) <$> (cSSCSSContainerQueryQueriesAnchored p),
    ("conditionText" A..=) <$> Just (cSSCSSContainerQueryConditionText p)
    ]

-- | Type 'CSS.CSSSupports'.
--   CSS Supports at-rule descriptor.
data CSSCSSSupports = CSSCSSSupports
  {
    -- | Supports rule text.
    cSSCSSSupportsText :: T.Text,
    -- | Whether the supports condition is satisfied.
    cSSCSSSupportsActive :: Bool,
    -- | The associated rule header range in the enclosing stylesheet (if
    --   available).
    cSSCSSSupportsRange :: Maybe CSSSourceRange,
    -- | Identifier of the stylesheet containing this object (if exists).
    cSSCSSSupportsStyleSheetId :: Maybe DOMNetworkEmulationPageSecurity.DOMStyleSheetId
  }
  deriving (Eq, Show)
instance FromJSON CSSCSSSupports where
  parseJSON = A.withObject "CSSCSSSupports" $ \o -> CSSCSSSupports
    <$> o A..: "text"
    <*> o A..: "active"
    <*> o A..:? "range"
    <*> o A..:? "styleSheetId"
instance ToJSON CSSCSSSupports where
  toJSON p = A.object $ catMaybes [
    ("text" A..=) <$> Just (cSSCSSSupportsText p),
    ("active" A..=) <$> Just (cSSCSSSupportsActive p),
    ("range" A..=) <$> (cSSCSSSupportsRange p),
    ("styleSheetId" A..=) <$> (cSSCSSSupportsStyleSheetId p)
    ]

-- | Type 'CSS.CSSNavigation'.
--   CSS Navigation at-rule descriptor.
data CSSCSSNavigation = CSSCSSNavigation
  {
    -- | Navigation rule text.
    cSSCSSNavigationText :: T.Text,
    -- | Whether the navigation condition is satisfied.
    cSSCSSNavigationActive :: Maybe Bool,
    -- | The associated rule header range in the enclosing stylesheet (if
    --   available).
    cSSCSSNavigationRange :: Maybe CSSSourceRange,
    -- | Identifier of the stylesheet containing this object (if exists).
    cSSCSSNavigationStyleSheetId :: Maybe DOMNetworkEmulationPageSecurity.DOMStyleSheetId
  }
  deriving (Eq, Show)
instance FromJSON CSSCSSNavigation where
  parseJSON = A.withObject "CSSCSSNavigation" $ \o -> CSSCSSNavigation
    <$> o A..: "text"
    <*> o A..:? "active"
    <*> o A..:? "range"
    <*> o A..:? "styleSheetId"
instance ToJSON CSSCSSNavigation where
  toJSON p = A.object $ catMaybes [
    ("text" A..=) <$> Just (cSSCSSNavigationText p),
    ("active" A..=) <$> (cSSCSSNavigationActive p),
    ("range" A..=) <$> (cSSCSSNavigationRange p),
    ("styleSheetId" A..=) <$> (cSSCSSNavigationStyleSheetId p)
    ]

-- | Type 'CSS.CSSScope'.
--   CSS Scope at-rule descriptor.
data CSSCSSScope = CSSCSSScope
  {
    -- | Scope rule text.
    cSSCSSScopeText :: T.Text,
    -- | The associated rule header range in the enclosing stylesheet (if
    --   available).
    cSSCSSScopeRange :: Maybe CSSSourceRange,
    -- | Identifier of the stylesheet containing this object (if exists).
    cSSCSSScopeStyleSheetId :: Maybe DOMNetworkEmulationPageSecurity.DOMStyleSheetId
  }
  deriving (Eq, Show)
instance FromJSON CSSCSSScope where
  parseJSON = A.withObject "CSSCSSScope" $ \o -> CSSCSSScope
    <$> o A..: "text"
    <*> o A..:? "range"
    <*> o A..:? "styleSheetId"
instance ToJSON CSSCSSScope where
  toJSON p = A.object $ catMaybes [
    ("text" A..=) <$> Just (cSSCSSScopeText p),
    ("range" A..=) <$> (cSSCSSScopeRange p),
    ("styleSheetId" A..=) <$> (cSSCSSScopeStyleSheetId p)
    ]

-- | Type 'CSS.CSSLayer'.
--   CSS Layer at-rule descriptor.
data CSSCSSLayer = CSSCSSLayer
  {
    -- | Layer name.
    cSSCSSLayerText :: T.Text,
    -- | The associated rule header range in the enclosing stylesheet (if
    --   available).
    cSSCSSLayerRange :: Maybe CSSSourceRange,
    -- | Identifier of the stylesheet containing this object (if exists).
    cSSCSSLayerStyleSheetId :: Maybe DOMNetworkEmulationPageSecurity.DOMStyleSheetId
  }
  deriving (Eq, Show)
instance FromJSON CSSCSSLayer where
  parseJSON = A.withObject "CSSCSSLayer" $ \o -> CSSCSSLayer
    <$> o A..: "text"
    <*> o A..:? "range"
    <*> o A..:? "styleSheetId"
instance ToJSON CSSCSSLayer where
  toJSON p = A.object $ catMaybes [
    ("text" A..=) <$> Just (cSSCSSLayerText p),
    ("range" A..=) <$> (cSSCSSLayerRange p),
    ("styleSheetId" A..=) <$> (cSSCSSLayerStyleSheetId p)
    ]

-- | Type 'CSS.CSSStartingStyle'.
--   CSS Starting Style at-rule descriptor.
data CSSCSSStartingStyle = CSSCSSStartingStyle
  {
    -- | The associated rule header range in the enclosing stylesheet (if
    --   available).
    cSSCSSStartingStyleRange :: Maybe CSSSourceRange,
    -- | Identifier of the stylesheet containing this object (if exists).
    cSSCSSStartingStyleStyleSheetId :: Maybe DOMNetworkEmulationPageSecurity.DOMStyleSheetId
  }
  deriving (Eq, Show)
instance FromJSON CSSCSSStartingStyle where
  parseJSON = A.withObject "CSSCSSStartingStyle" $ \o -> CSSCSSStartingStyle
    <$> o A..:? "range"
    <*> o A..:? "styleSheetId"
instance ToJSON CSSCSSStartingStyle where
  toJSON p = A.object $ catMaybes [
    ("range" A..=) <$> (cSSCSSStartingStyleRange p),
    ("styleSheetId" A..=) <$> (cSSCSSStartingStyleStyleSheetId p)
    ]

-- | Type 'CSS.CSSLayerData'.
--   CSS Layer data.
data CSSCSSLayerData = CSSCSSLayerData
  {
    -- | Layer name.
    cSSCSSLayerDataName :: T.Text,
    -- | Direct sub-layers
    cSSCSSLayerDataSubLayers :: Maybe [CSSCSSLayerData],
    -- | Layer order. The order determines the order of the layer in the cascade order.
    --   A higher number has higher priority in the cascade order.
    cSSCSSLayerDataOrder :: Double
  }
  deriving (Eq, Show)
instance FromJSON CSSCSSLayerData where
  parseJSON = A.withObject "CSSCSSLayerData" $ \o -> CSSCSSLayerData
    <$> o A..: "name"
    <*> o A..:? "subLayers"
    <*> o A..: "order"
instance ToJSON CSSCSSLayerData where
  toJSON p = A.object $ catMaybes [
    ("name" A..=) <$> Just (cSSCSSLayerDataName p),
    ("subLayers" A..=) <$> (cSSCSSLayerDataSubLayers p),
    ("order" A..=) <$> Just (cSSCSSLayerDataOrder p)
    ]

-- | Type 'CSS.PlatformFontUsage'.
--   Information about amount of glyphs that were rendered with given font.
data CSSPlatformFontUsage = CSSPlatformFontUsage
  {
    -- | Font's family name reported by platform.
    cSSPlatformFontUsageFamilyName :: T.Text,
    -- | Font's PostScript name reported by platform.
    cSSPlatformFontUsagePostScriptName :: T.Text,
    -- | Indicates if the font was downloaded or resolved locally.
    cSSPlatformFontUsageIsCustomFont :: Bool,
    -- | Amount of glyphs that were rendered with this font.
    cSSPlatformFontUsageGlyphCount :: Double
  }
  deriving (Eq, Show)
instance FromJSON CSSPlatformFontUsage where
  parseJSON = A.withObject "CSSPlatformFontUsage" $ \o -> CSSPlatformFontUsage
    <$> o A..: "familyName"
    <*> o A..: "postScriptName"
    <*> o A..: "isCustomFont"
    <*> o A..: "glyphCount"
instance ToJSON CSSPlatformFontUsage where
  toJSON p = A.object $ catMaybes [
    ("familyName" A..=) <$> Just (cSSPlatformFontUsageFamilyName p),
    ("postScriptName" A..=) <$> Just (cSSPlatformFontUsagePostScriptName p),
    ("isCustomFont" A..=) <$> Just (cSSPlatformFontUsageIsCustomFont p),
    ("glyphCount" A..=) <$> Just (cSSPlatformFontUsageGlyphCount p)
    ]

-- | Type 'CSS.FontVariationAxis'.
--   Information about font variation axes for variable fonts
data CSSFontVariationAxis = CSSFontVariationAxis
  {
    -- | The font-variation-setting tag (a.k.a. "axis tag").
    cSSFontVariationAxisTag :: T.Text,
    -- | Human-readable variation name in the default language (normally, "en").
    cSSFontVariationAxisName :: T.Text,
    -- | The minimum value (inclusive) the font supports for this tag.
    cSSFontVariationAxisMinValue :: Double,
    -- | The maximum value (inclusive) the font supports for this tag.
    cSSFontVariationAxisMaxValue :: Double,
    -- | The default value.
    cSSFontVariationAxisDefaultValue :: Double
  }
  deriving (Eq, Show)
instance FromJSON CSSFontVariationAxis where
  parseJSON = A.withObject "CSSFontVariationAxis" $ \o -> CSSFontVariationAxis
    <$> o A..: "tag"
    <*> o A..: "name"
    <*> o A..: "minValue"
    <*> o A..: "maxValue"
    <*> o A..: "defaultValue"
instance ToJSON CSSFontVariationAxis where
  toJSON p = A.object $ catMaybes [
    ("tag" A..=) <$> Just (cSSFontVariationAxisTag p),
    ("name" A..=) <$> Just (cSSFontVariationAxisName p),
    ("minValue" A..=) <$> Just (cSSFontVariationAxisMinValue p),
    ("maxValue" A..=) <$> Just (cSSFontVariationAxisMaxValue p),
    ("defaultValue" A..=) <$> Just (cSSFontVariationAxisDefaultValue p)
    ]

-- | Type 'CSS.FontFace'.
--   Properties of a web font: https://www.w3.org/TR/2008/REC-CSS2-20080411/fonts.html#font-descriptions
--   and additional information such as platformFontFamily and fontVariationAxes.
data CSSFontFace = CSSFontFace
  {
    -- | The font-family.
    cSSFontFaceFontFamily :: T.Text,
    -- | The font-style.
    cSSFontFaceFontStyle :: T.Text,
    -- | The font-variant.
    cSSFontFaceFontVariant :: T.Text,
    -- | The font-weight.
    cSSFontFaceFontWeight :: T.Text,
    -- | The font-stretch.
    cSSFontFaceFontStretch :: T.Text,
    -- | The font-display.
    cSSFontFaceFontDisplay :: T.Text,
    -- | The unicode-range.
    cSSFontFaceUnicodeRange :: T.Text,
    -- | The src.
    cSSFontFaceSrc :: T.Text,
    -- | The resolved platform font family
    cSSFontFacePlatformFontFamily :: T.Text,
    -- | Available variation settings (a.k.a. "axes").
    cSSFontFaceFontVariationAxes :: Maybe [CSSFontVariationAxis]
  }
  deriving (Eq, Show)
instance FromJSON CSSFontFace where
  parseJSON = A.withObject "CSSFontFace" $ \o -> CSSFontFace
    <$> o A..: "fontFamily"
    <*> o A..: "fontStyle"
    <*> o A..: "fontVariant"
    <*> o A..: "fontWeight"
    <*> o A..: "fontStretch"
    <*> o A..: "fontDisplay"
    <*> o A..: "unicodeRange"
    <*> o A..: "src"
    <*> o A..: "platformFontFamily"
    <*> o A..:? "fontVariationAxes"
instance ToJSON CSSFontFace where
  toJSON p = A.object $ catMaybes [
    ("fontFamily" A..=) <$> Just (cSSFontFaceFontFamily p),
    ("fontStyle" A..=) <$> Just (cSSFontFaceFontStyle p),
    ("fontVariant" A..=) <$> Just (cSSFontFaceFontVariant p),
    ("fontWeight" A..=) <$> Just (cSSFontFaceFontWeight p),
    ("fontStretch" A..=) <$> Just (cSSFontFaceFontStretch p),
    ("fontDisplay" A..=) <$> Just (cSSFontFaceFontDisplay p),
    ("unicodeRange" A..=) <$> Just (cSSFontFaceUnicodeRange p),
    ("src" A..=) <$> Just (cSSFontFaceSrc p),
    ("platformFontFamily" A..=) <$> Just (cSSFontFacePlatformFontFamily p),
    ("fontVariationAxes" A..=) <$> (cSSFontFaceFontVariationAxes p)
    ]

-- | Type 'CSS.CSSTryRule'.
--   CSS try rule representation.
data CSSCSSTryRule = CSSCSSTryRule
  {
    -- | The css style sheet identifier (absent for user agent stylesheet and user-specified
    --   stylesheet rules) this rule came from.
    cSSCSSTryRuleStyleSheetId :: Maybe DOMNetworkEmulationPageSecurity.DOMStyleSheetId,
    -- | Parent stylesheet's origin.
    cSSCSSTryRuleOrigin :: CSSStyleSheetOrigin,
    -- | Associated style declaration.
    cSSCSSTryRuleStyle :: CSSCSSStyle
  }
  deriving (Eq, Show)
instance FromJSON CSSCSSTryRule where
  parseJSON = A.withObject "CSSCSSTryRule" $ \o -> CSSCSSTryRule
    <$> o A..:? "styleSheetId"
    <*> o A..: "origin"
    <*> o A..: "style"
instance ToJSON CSSCSSTryRule where
  toJSON p = A.object $ catMaybes [
    ("styleSheetId" A..=) <$> (cSSCSSTryRuleStyleSheetId p),
    ("origin" A..=) <$> Just (cSSCSSTryRuleOrigin p),
    ("style" A..=) <$> Just (cSSCSSTryRuleStyle p)
    ]

-- | Type 'CSS.CSSPositionTryRule'.
--   CSS @position-try rule representation.
data CSSCSSPositionTryRule = CSSCSSPositionTryRule
  {
    -- | The prelude dashed-ident name
    cSSCSSPositionTryRuleName :: CSSValue,
    -- | The css style sheet identifier (absent for user agent stylesheet and user-specified
    --   stylesheet rules) this rule came from.
    cSSCSSPositionTryRuleStyleSheetId :: Maybe DOMNetworkEmulationPageSecurity.DOMStyleSheetId,
    -- | Parent stylesheet's origin.
    cSSCSSPositionTryRuleOrigin :: CSSStyleSheetOrigin,
    -- | Associated style declaration.
    cSSCSSPositionTryRuleStyle :: CSSCSSStyle,
    cSSCSSPositionTryRuleActive :: Bool
  }
  deriving (Eq, Show)
instance FromJSON CSSCSSPositionTryRule where
  parseJSON = A.withObject "CSSCSSPositionTryRule" $ \o -> CSSCSSPositionTryRule
    <$> o A..: "name"
    <*> o A..:? "styleSheetId"
    <*> o A..: "origin"
    <*> o A..: "style"
    <*> o A..: "active"
instance ToJSON CSSCSSPositionTryRule where
  toJSON p = A.object $ catMaybes [
    ("name" A..=) <$> Just (cSSCSSPositionTryRuleName p),
    ("styleSheetId" A..=) <$> (cSSCSSPositionTryRuleStyleSheetId p),
    ("origin" A..=) <$> Just (cSSCSSPositionTryRuleOrigin p),
    ("style" A..=) <$> Just (cSSCSSPositionTryRuleStyle p),
    ("active" A..=) <$> Just (cSSCSSPositionTryRuleActive p)
    ]

-- | Type 'CSS.CSSKeyframesRule'.
--   CSS keyframes rule representation.
data CSSCSSKeyframesRule = CSSCSSKeyframesRule
  {
    -- | Animation name.
    cSSCSSKeyframesRuleAnimationName :: CSSValue,
    -- | List of keyframes.
    cSSCSSKeyframesRuleKeyframes :: [CSSCSSKeyframeRule]
  }
  deriving (Eq, Show)
instance FromJSON CSSCSSKeyframesRule where
  parseJSON = A.withObject "CSSCSSKeyframesRule" $ \o -> CSSCSSKeyframesRule
    <$> o A..: "animationName"
    <*> o A..: "keyframes"
instance ToJSON CSSCSSKeyframesRule where
  toJSON p = A.object $ catMaybes [
    ("animationName" A..=) <$> Just (cSSCSSKeyframesRuleAnimationName p),
    ("keyframes" A..=) <$> Just (cSSCSSKeyframesRuleKeyframes p)
    ]

-- | Type 'CSS.CSSPropertyRegistration'.
--   Representation of a custom property registration through CSS.registerProperty
data CSSCSSPropertyRegistration = CSSCSSPropertyRegistration
  {
    cSSCSSPropertyRegistrationPropertyName :: T.Text,
    cSSCSSPropertyRegistrationInitialValue :: Maybe CSSValue,
    cSSCSSPropertyRegistrationInherits :: Bool,
    cSSCSSPropertyRegistrationSyntax :: T.Text
  }
  deriving (Eq, Show)
instance FromJSON CSSCSSPropertyRegistration where
  parseJSON = A.withObject "CSSCSSPropertyRegistration" $ \o -> CSSCSSPropertyRegistration
    <$> o A..: "propertyName"
    <*> o A..:? "initialValue"
    <*> o A..: "inherits"
    <*> o A..: "syntax"
instance ToJSON CSSCSSPropertyRegistration where
  toJSON p = A.object $ catMaybes [
    ("propertyName" A..=) <$> Just (cSSCSSPropertyRegistrationPropertyName p),
    ("initialValue" A..=) <$> (cSSCSSPropertyRegistrationInitialValue p),
    ("inherits" A..=) <$> Just (cSSCSSPropertyRegistrationInherits p),
    ("syntax" A..=) <$> Just (cSSCSSPropertyRegistrationSyntax p)
    ]

-- | Type 'CSS.CSSAtRule'.
--   CSS generic @rule representation.
data CSSCSSAtRuleType = CSSCSSAtRuleTypeFontFace | CSSCSSAtRuleTypeFontFeatureValues | CSSCSSAtRuleTypeFontPaletteValues | CSSCSSAtRuleTypeCounterStyle
  deriving (Ord, Eq, Show, Read)
instance FromJSON CSSCSSAtRuleType where
  parseJSON = A.withText "CSSCSSAtRuleType" $ \v -> case v of
    "font-face" -> pure CSSCSSAtRuleTypeFontFace
    "font-feature-values" -> pure CSSCSSAtRuleTypeFontFeatureValues
    "font-palette-values" -> pure CSSCSSAtRuleTypeFontPaletteValues
    "counter-style" -> pure CSSCSSAtRuleTypeCounterStyle
    "_" -> fail "failed to parse CSSCSSAtRuleType"
instance ToJSON CSSCSSAtRuleType where
  toJSON v = A.String $ case v of
    CSSCSSAtRuleTypeFontFace -> "font-face"
    CSSCSSAtRuleTypeFontFeatureValues -> "font-feature-values"
    CSSCSSAtRuleTypeFontPaletteValues -> "font-palette-values"
    CSSCSSAtRuleTypeCounterStyle -> "counter-style"
data CSSCSSAtRuleSubsection = CSSCSSAtRuleSubsectionSwash | CSSCSSAtRuleSubsectionAnnotation | CSSCSSAtRuleSubsectionOrnaments | CSSCSSAtRuleSubsectionStylistic | CSSCSSAtRuleSubsectionStyleset | CSSCSSAtRuleSubsectionCharacterVariant
  deriving (Ord, Eq, Show, Read)
instance FromJSON CSSCSSAtRuleSubsection where
  parseJSON = A.withText "CSSCSSAtRuleSubsection" $ \v -> case v of
    "swash" -> pure CSSCSSAtRuleSubsectionSwash
    "annotation" -> pure CSSCSSAtRuleSubsectionAnnotation
    "ornaments" -> pure CSSCSSAtRuleSubsectionOrnaments
    "stylistic" -> pure CSSCSSAtRuleSubsectionStylistic
    "styleset" -> pure CSSCSSAtRuleSubsectionStyleset
    "character-variant" -> pure CSSCSSAtRuleSubsectionCharacterVariant
    "_" -> fail "failed to parse CSSCSSAtRuleSubsection"
instance ToJSON CSSCSSAtRuleSubsection where
  toJSON v = A.String $ case v of
    CSSCSSAtRuleSubsectionSwash -> "swash"
    CSSCSSAtRuleSubsectionAnnotation -> "annotation"
    CSSCSSAtRuleSubsectionOrnaments -> "ornaments"
    CSSCSSAtRuleSubsectionStylistic -> "stylistic"
    CSSCSSAtRuleSubsectionStyleset -> "styleset"
    CSSCSSAtRuleSubsectionCharacterVariant -> "character-variant"
data CSSCSSAtRule = CSSCSSAtRule
  {
    -- | Type of at-rule.
    cSSCSSAtRuleType :: CSSCSSAtRuleType,
    -- | Subsection of font-feature-values, if this is a subsection.
    cSSCSSAtRuleSubsection :: Maybe CSSCSSAtRuleSubsection,
    -- | LINT.ThenChange(//third_party/blink/renderer/core/inspector/inspector_style_sheet.cc:FontVariantAlternatesFeatureType,//third_party/blink/renderer/core/inspector/inspector_css_agent.cc:FontVariantAlternatesFeatureType)
    --   Associated name, if applicable.
    cSSCSSAtRuleName :: Maybe CSSValue,
    -- | The css style sheet identifier (absent for user agent stylesheet and user-specified
    --   stylesheet rules) this rule came from.
    cSSCSSAtRuleStyleSheetId :: Maybe DOMNetworkEmulationPageSecurity.DOMStyleSheetId,
    -- | Parent stylesheet's origin.
    cSSCSSAtRuleOrigin :: CSSStyleSheetOrigin,
    -- | Associated style declaration.
    cSSCSSAtRuleStyle :: CSSCSSStyle
  }
  deriving (Eq, Show)
instance FromJSON CSSCSSAtRule where
  parseJSON = A.withObject "CSSCSSAtRule" $ \o -> CSSCSSAtRule
    <$> o A..: "type"
    <*> o A..:? "subsection"
    <*> o A..:? "name"
    <*> o A..:? "styleSheetId"
    <*> o A..: "origin"
    <*> o A..: "style"
instance ToJSON CSSCSSAtRule where
  toJSON p = A.object $ catMaybes [
    ("type" A..=) <$> Just (cSSCSSAtRuleType p),
    ("subsection" A..=) <$> (cSSCSSAtRuleSubsection p),
    ("name" A..=) <$> (cSSCSSAtRuleName p),
    ("styleSheetId" A..=) <$> (cSSCSSAtRuleStyleSheetId p),
    ("origin" A..=) <$> Just (cSSCSSAtRuleOrigin p),
    ("style" A..=) <$> Just (cSSCSSAtRuleStyle p)
    ]

-- | Type 'CSS.CSSPropertyRule'.
--   CSS property at-rule representation.
data CSSCSSPropertyRule = CSSCSSPropertyRule
  {
    -- | The css style sheet identifier (absent for user agent stylesheet and user-specified
    --   stylesheet rules) this rule came from.
    cSSCSSPropertyRuleStyleSheetId :: Maybe DOMNetworkEmulationPageSecurity.DOMStyleSheetId,
    -- | Parent stylesheet's origin.
    cSSCSSPropertyRuleOrigin :: CSSStyleSheetOrigin,
    -- | Associated property name.
    cSSCSSPropertyRulePropertyName :: CSSValue,
    -- | Associated style declaration.
    cSSCSSPropertyRuleStyle :: CSSCSSStyle
  }
  deriving (Eq, Show)
instance FromJSON CSSCSSPropertyRule where
  parseJSON = A.withObject "CSSCSSPropertyRule" $ \o -> CSSCSSPropertyRule
    <$> o A..:? "styleSheetId"
    <*> o A..: "origin"
    <*> o A..: "propertyName"
    <*> o A..: "style"
instance ToJSON CSSCSSPropertyRule where
  toJSON p = A.object $ catMaybes [
    ("styleSheetId" A..=) <$> (cSSCSSPropertyRuleStyleSheetId p),
    ("origin" A..=) <$> Just (cSSCSSPropertyRuleOrigin p),
    ("propertyName" A..=) <$> Just (cSSCSSPropertyRulePropertyName p),
    ("style" A..=) <$> Just (cSSCSSPropertyRuleStyle p)
    ]

-- | Type 'CSS.CSSFunctionParameter'.
--   CSS function argument representation.
data CSSCSSFunctionParameter = CSSCSSFunctionParameter
  {
    -- | The parameter name.
    cSSCSSFunctionParameterName :: T.Text,
    -- | The parameter type.
    cSSCSSFunctionParameterType :: T.Text
  }
  deriving (Eq, Show)
instance FromJSON CSSCSSFunctionParameter where
  parseJSON = A.withObject "CSSCSSFunctionParameter" $ \o -> CSSCSSFunctionParameter
    <$> o A..: "name"
    <*> o A..: "type"
instance ToJSON CSSCSSFunctionParameter where
  toJSON p = A.object $ catMaybes [
    ("name" A..=) <$> Just (cSSCSSFunctionParameterName p),
    ("type" A..=) <$> Just (cSSCSSFunctionParameterType p)
    ]

-- | Type 'CSS.CSSFunctionConditionNode'.
--   CSS function conditional block representation.
data CSSCSSFunctionConditionNode = CSSCSSFunctionConditionNode
  {
    -- | Media query for this conditional block. Only one type of condition should be set.
    cSSCSSFunctionConditionNodeMedia :: Maybe CSSCSSMedia,
    -- | Container query for this conditional block. Only one type of condition should be set.
    cSSCSSFunctionConditionNodeContainerQueries :: Maybe CSSCSSContainerQuery,
    -- | @supports CSS at-rule condition. Only one type of condition should be set.
    cSSCSSFunctionConditionNodeSupports :: Maybe CSSCSSSupports,
    -- | @navigation condition. Only one type of condition should be set.
    cSSCSSFunctionConditionNodeNavigation :: Maybe CSSCSSNavigation,
    -- | Block body.
    cSSCSSFunctionConditionNodeChildren :: [CSSCSSFunctionNode],
    -- | The condition text.
    cSSCSSFunctionConditionNodeConditionText :: T.Text
  }
  deriving (Eq, Show)
instance FromJSON CSSCSSFunctionConditionNode where
  parseJSON = A.withObject "CSSCSSFunctionConditionNode" $ \o -> CSSCSSFunctionConditionNode
    <$> o A..:? "media"
    <*> o A..:? "containerQueries"
    <*> o A..:? "supports"
    <*> o A..:? "navigation"
    <*> o A..: "children"
    <*> o A..: "conditionText"
instance ToJSON CSSCSSFunctionConditionNode where
  toJSON p = A.object $ catMaybes [
    ("media" A..=) <$> (cSSCSSFunctionConditionNodeMedia p),
    ("containerQueries" A..=) <$> (cSSCSSFunctionConditionNodeContainerQueries p),
    ("supports" A..=) <$> (cSSCSSFunctionConditionNodeSupports p),
    ("navigation" A..=) <$> (cSSCSSFunctionConditionNodeNavigation p),
    ("children" A..=) <$> Just (cSSCSSFunctionConditionNodeChildren p),
    ("conditionText" A..=) <$> Just (cSSCSSFunctionConditionNodeConditionText p)
    ]

-- | Type 'CSS.CSSFunctionNode'.
--   Section of the body of a CSS function rule.
data CSSCSSFunctionNode = CSSCSSFunctionNode
  {
    -- | A conditional block. If set, style should not be set.
    cSSCSSFunctionNodeCondition :: Maybe CSSCSSFunctionConditionNode,
    -- | Values set by this node. If set, condition should not be set.
    cSSCSSFunctionNodeStyle :: Maybe CSSCSSStyle
  }
  deriving (Eq, Show)
instance FromJSON CSSCSSFunctionNode where
  parseJSON = A.withObject "CSSCSSFunctionNode" $ \o -> CSSCSSFunctionNode
    <$> o A..:? "condition"
    <*> o A..:? "style"
instance ToJSON CSSCSSFunctionNode where
  toJSON p = A.object $ catMaybes [
    ("condition" A..=) <$> (cSSCSSFunctionNodeCondition p),
    ("style" A..=) <$> (cSSCSSFunctionNodeStyle p)
    ]

-- | Type 'CSS.CSSFunctionRule'.
--   CSS function at-rule representation.
data CSSCSSFunctionRule = CSSCSSFunctionRule
  {
    -- | Name of the function.
    cSSCSSFunctionRuleName :: CSSValue,
    -- | The css style sheet identifier (absent for user agent stylesheet and user-specified
    --   stylesheet rules) this rule came from.
    cSSCSSFunctionRuleStyleSheetId :: Maybe DOMNetworkEmulationPageSecurity.DOMStyleSheetId,
    -- | Parent stylesheet's origin.
    cSSCSSFunctionRuleOrigin :: CSSStyleSheetOrigin,
    -- | List of parameters.
    cSSCSSFunctionRuleParameters :: [CSSCSSFunctionParameter],
    -- | Function body.
    cSSCSSFunctionRuleChildren :: [CSSCSSFunctionNode],
    -- | The BackendNodeId of the DOM node that constitutes the origin tree scope of this rule.
    cSSCSSFunctionRuleOriginTreeScopeNodeId :: Maybe DOMNetworkEmulationPageSecurity.DOMBackendNodeId
  }
  deriving (Eq, Show)
instance FromJSON CSSCSSFunctionRule where
  parseJSON = A.withObject "CSSCSSFunctionRule" $ \o -> CSSCSSFunctionRule
    <$> o A..: "name"
    <*> o A..:? "styleSheetId"
    <*> o A..: "origin"
    <*> o A..: "parameters"
    <*> o A..: "children"
    <*> o A..:? "originTreeScopeNodeId"
instance ToJSON CSSCSSFunctionRule where
  toJSON p = A.object $ catMaybes [
    ("name" A..=) <$> Just (cSSCSSFunctionRuleName p),
    ("styleSheetId" A..=) <$> (cSSCSSFunctionRuleStyleSheetId p),
    ("origin" A..=) <$> Just (cSSCSSFunctionRuleOrigin p),
    ("parameters" A..=) <$> Just (cSSCSSFunctionRuleParameters p),
    ("children" A..=) <$> Just (cSSCSSFunctionRuleChildren p),
    ("originTreeScopeNodeId" A..=) <$> (cSSCSSFunctionRuleOriginTreeScopeNodeId p)
    ]

-- | Type 'CSS.CSSKeyframeRule'.
--   CSS keyframe rule representation.
data CSSCSSKeyframeRule = CSSCSSKeyframeRule
  {
    -- | The css style sheet identifier (absent for user agent stylesheet and user-specified
    --   stylesheet rules) this rule came from.
    cSSCSSKeyframeRuleStyleSheetId :: Maybe DOMNetworkEmulationPageSecurity.DOMStyleSheetId,
    -- | Parent stylesheet's origin.
    cSSCSSKeyframeRuleOrigin :: CSSStyleSheetOrigin,
    -- | Associated key text.
    cSSCSSKeyframeRuleKeyText :: CSSValue,
    -- | Associated style declaration.
    cSSCSSKeyframeRuleStyle :: CSSCSSStyle
  }
  deriving (Eq, Show)
instance FromJSON CSSCSSKeyframeRule where
  parseJSON = A.withObject "CSSCSSKeyframeRule" $ \o -> CSSCSSKeyframeRule
    <$> o A..:? "styleSheetId"
    <*> o A..: "origin"
    <*> o A..: "keyText"
    <*> o A..: "style"
instance ToJSON CSSCSSKeyframeRule where
  toJSON p = A.object $ catMaybes [
    ("styleSheetId" A..=) <$> (cSSCSSKeyframeRuleStyleSheetId p),
    ("origin" A..=) <$> Just (cSSCSSKeyframeRuleOrigin p),
    ("keyText" A..=) <$> Just (cSSCSSKeyframeRuleKeyText p),
    ("style" A..=) <$> Just (cSSCSSKeyframeRuleStyle p)
    ]

-- | Type 'CSS.StyleDeclarationEdit'.
--   A descriptor of operation to mutate style declaration text.
data CSSStyleDeclarationEdit = CSSStyleDeclarationEdit
  {
    -- | The css style sheet identifier.
    cSSStyleDeclarationEditStyleSheetId :: DOMNetworkEmulationPageSecurity.DOMStyleSheetId,
    -- | The range of the style text in the enclosing stylesheet.
    cSSStyleDeclarationEditRange :: CSSSourceRange,
    -- | New style text.
    cSSStyleDeclarationEditText :: T.Text
  }
  deriving (Eq, Show)
instance FromJSON CSSStyleDeclarationEdit where
  parseJSON = A.withObject "CSSStyleDeclarationEdit" $ \o -> CSSStyleDeclarationEdit
    <$> o A..: "styleSheetId"
    <*> o A..: "range"
    <*> o A..: "text"
instance ToJSON CSSStyleDeclarationEdit where
  toJSON p = A.object $ catMaybes [
    ("styleSheetId" A..=) <$> Just (cSSStyleDeclarationEditStyleSheetId p),
    ("range" A..=) <$> Just (cSSStyleDeclarationEditRange p),
    ("text" A..=) <$> Just (cSSStyleDeclarationEditText p)
    ]

-- | Type of the 'CSS.fontsUpdated' event.
data CSSFontsUpdated = CSSFontsUpdated
  {
    -- | The web font that has loaded.
    cSSFontsUpdatedFont :: Maybe CSSFontFace
  }
  deriving (Eq, Show)
instance FromJSON CSSFontsUpdated where
  parseJSON = A.withObject "CSSFontsUpdated" $ \o -> CSSFontsUpdated
    <$> o A..:? "font"
instance Event CSSFontsUpdated where
  eventName _ = "CSS.fontsUpdated"

-- | Type of the 'CSS.mediaQueryResultChanged' event.
data CSSMediaQueryResultChanged = CSSMediaQueryResultChanged
  deriving (Eq, Show, Read)
instance FromJSON CSSMediaQueryResultChanged where
  parseJSON _ = pure CSSMediaQueryResultChanged
instance Event CSSMediaQueryResultChanged where
  eventName _ = "CSS.mediaQueryResultChanged"

-- | Type of the 'CSS.styleSheetAdded' event.
data CSSStyleSheetAdded = CSSStyleSheetAdded
  {
    -- | Added stylesheet metainfo.
    cSSStyleSheetAddedHeader :: CSSCSSStyleSheetHeader
  }
  deriving (Eq, Show)
instance FromJSON CSSStyleSheetAdded where
  parseJSON = A.withObject "CSSStyleSheetAdded" $ \o -> CSSStyleSheetAdded
    <$> o A..: "header"
instance Event CSSStyleSheetAdded where
  eventName _ = "CSS.styleSheetAdded"

-- | Type of the 'CSS.styleSheetChanged' event.
data CSSStyleSheetChanged = CSSStyleSheetChanged
  {
    cSSStyleSheetChangedStyleSheetId :: DOMNetworkEmulationPageSecurity.DOMStyleSheetId
  }
  deriving (Eq, Show)
instance FromJSON CSSStyleSheetChanged where
  parseJSON = A.withObject "CSSStyleSheetChanged" $ \o -> CSSStyleSheetChanged
    <$> o A..: "styleSheetId"
instance Event CSSStyleSheetChanged where
  eventName _ = "CSS.styleSheetChanged"

-- | Type of the 'CSS.styleSheetRemoved' event.
data CSSStyleSheetRemoved = CSSStyleSheetRemoved
  {
    -- | Identifier of the removed stylesheet.
    cSSStyleSheetRemovedStyleSheetId :: DOMNetworkEmulationPageSecurity.DOMStyleSheetId
  }
  deriving (Eq, Show)
instance FromJSON CSSStyleSheetRemoved where
  parseJSON = A.withObject "CSSStyleSheetRemoved" $ \o -> CSSStyleSheetRemoved
    <$> o A..: "styleSheetId"
instance Event CSSStyleSheetRemoved where
  eventName _ = "CSS.styleSheetRemoved"

-- | Type of the 'CSS.computedStyleUpdated' event.
data CSSComputedStyleUpdated = CSSComputedStyleUpdated
  {
    -- | The node id that has updated computed styles.
    cSSComputedStyleUpdatedNodeId :: DOMNetworkEmulationPageSecurity.DOMNodeId
  }
  deriving (Eq, Show)
instance FromJSON CSSComputedStyleUpdated where
  parseJSON = A.withObject "CSSComputedStyleUpdated" $ \o -> CSSComputedStyleUpdated
    <$> o A..: "nodeId"
instance Event CSSComputedStyleUpdated where
  eventName _ = "CSS.computedStyleUpdated"

-- | Inserts a new rule with the given `ruleText` in a stylesheet with given `styleSheetId`, at the
--   position specified by `location`.

-- | Parameters of the 'CSS.addRule' command.
data PCSSAddRule = PCSSAddRule
  {
    -- | The css style sheet identifier where a new rule should be inserted.
    pCSSAddRuleStyleSheetId :: DOMNetworkEmulationPageSecurity.DOMStyleSheetId,
    -- | The text of a new rule.
    pCSSAddRuleRuleText :: T.Text,
    -- | Text position of a new rule in the target style sheet.
    pCSSAddRuleLocation :: CSSSourceRange,
    -- | NodeId for the DOM node in whose context custom property declarations for registered properties should be
    --   validated. If omitted, declarations in the new rule text can only be validated statically, which may produce
    --   incorrect results if the declaration contains a var() for example.
    pCSSAddRuleNodeForPropertySyntaxValidation :: Maybe DOMNetworkEmulationPageSecurity.DOMNodeId
  }
  deriving (Eq, Show)
pCSSAddRule
  {-
  -- | The css style sheet identifier where a new rule should be inserted.
  -}
  :: DOMNetworkEmulationPageSecurity.DOMStyleSheetId
  {-
  -- | The text of a new rule.
  -}
  -> T.Text
  {-
  -- | Text position of a new rule in the target style sheet.
  -}
  -> CSSSourceRange
  -> PCSSAddRule
pCSSAddRule
  arg_pCSSAddRuleStyleSheetId
  arg_pCSSAddRuleRuleText
  arg_pCSSAddRuleLocation
  = PCSSAddRule
    arg_pCSSAddRuleStyleSheetId
    arg_pCSSAddRuleRuleText
    arg_pCSSAddRuleLocation
    Nothing
instance ToJSON PCSSAddRule where
  toJSON p = A.object $ catMaybes [
    ("styleSheetId" A..=) <$> Just (pCSSAddRuleStyleSheetId p),
    ("ruleText" A..=) <$> Just (pCSSAddRuleRuleText p),
    ("location" A..=) <$> Just (pCSSAddRuleLocation p),
    ("nodeForPropertySyntaxValidation" A..=) <$> (pCSSAddRuleNodeForPropertySyntaxValidation p)
    ]
data CSSAddRule = CSSAddRule
  {
    -- | The newly created rule.
    cSSAddRuleRule :: CSSCSSRule
  }
  deriving (Eq, Show)
instance FromJSON CSSAddRule where
  parseJSON = A.withObject "CSSAddRule" $ \o -> CSSAddRule
    <$> o A..: "rule"
instance Command PCSSAddRule where
  type CommandResponse PCSSAddRule = CSSAddRule
  commandName _ = "CSS.addRule"

-- | Returns all class names from specified stylesheet.

-- | Parameters of the 'CSS.collectClassNames' command.
data PCSSCollectClassNames = PCSSCollectClassNames
  {
    pCSSCollectClassNamesStyleSheetId :: DOMNetworkEmulationPageSecurity.DOMStyleSheetId
  }
  deriving (Eq, Show)
pCSSCollectClassNames
  :: DOMNetworkEmulationPageSecurity.DOMStyleSheetId
  -> PCSSCollectClassNames
pCSSCollectClassNames
  arg_pCSSCollectClassNamesStyleSheetId
  = PCSSCollectClassNames
    arg_pCSSCollectClassNamesStyleSheetId
instance ToJSON PCSSCollectClassNames where
  toJSON p = A.object $ catMaybes [
    ("styleSheetId" A..=) <$> Just (pCSSCollectClassNamesStyleSheetId p)
    ]
data CSSCollectClassNames = CSSCollectClassNames
  {
    -- | Class name list.
    cSSCollectClassNamesClassNames :: [T.Text]
  }
  deriving (Eq, Show)
instance FromJSON CSSCollectClassNames where
  parseJSON = A.withObject "CSSCollectClassNames" $ \o -> CSSCollectClassNames
    <$> o A..: "classNames"
instance Command PCSSCollectClassNames where
  type CommandResponse PCSSCollectClassNames = CSSCollectClassNames
  commandName _ = "CSS.collectClassNames"

-- | Creates a new special "via-inspector" stylesheet in the frame with given `frameId`.

-- | Parameters of the 'CSS.createStyleSheet' command.
data PCSSCreateStyleSheet = PCSSCreateStyleSheet
  {
    -- | Identifier of the frame where "via-inspector" stylesheet should be created.
    pCSSCreateStyleSheetFrameId :: DOMNetworkEmulationPageSecurity.PageFrameId,
    -- | If true, creates a new stylesheet for every call. If false,
    --   returns a stylesheet previously created by a call with force=false
    --   for the frame's document if it exists or creates a new stylesheet
    --   (default: false).
    pCSSCreateStyleSheetForce :: Maybe Bool
  }
  deriving (Eq, Show)
pCSSCreateStyleSheet
  {-
  -- | Identifier of the frame where "via-inspector" stylesheet should be created.
  -}
  :: DOMNetworkEmulationPageSecurity.PageFrameId
  -> PCSSCreateStyleSheet
pCSSCreateStyleSheet
  arg_pCSSCreateStyleSheetFrameId
  = PCSSCreateStyleSheet
    arg_pCSSCreateStyleSheetFrameId
    Nothing
instance ToJSON PCSSCreateStyleSheet where
  toJSON p = A.object $ catMaybes [
    ("frameId" A..=) <$> Just (pCSSCreateStyleSheetFrameId p),
    ("force" A..=) <$> (pCSSCreateStyleSheetForce p)
    ]
data CSSCreateStyleSheet = CSSCreateStyleSheet
  {
    -- | Identifier of the created "via-inspector" stylesheet.
    cSSCreateStyleSheetStyleSheetId :: DOMNetworkEmulationPageSecurity.DOMStyleSheetId
  }
  deriving (Eq, Show)
instance FromJSON CSSCreateStyleSheet where
  parseJSON = A.withObject "CSSCreateStyleSheet" $ \o -> CSSCreateStyleSheet
    <$> o A..: "styleSheetId"
instance Command PCSSCreateStyleSheet where
  type CommandResponse PCSSCreateStyleSheet = CSSCreateStyleSheet
  commandName _ = "CSS.createStyleSheet"

-- | Disables the CSS agent for the given page.

-- | Parameters of the 'CSS.disable' command.
data PCSSDisable = PCSSDisable
  deriving (Eq, Show)
pCSSDisable
  :: PCSSDisable
pCSSDisable
  = PCSSDisable
instance ToJSON PCSSDisable where
  toJSON _ = A.Null
instance Command PCSSDisable where
  type CommandResponse PCSSDisable = ()
  commandName _ = "CSS.disable"
  fromJSON = const . A.Success . const ()

-- | Enables the CSS agent for the given page. Clients should not assume that the CSS agent has been
--   enabled until the result of this command is received.

-- | Parameters of the 'CSS.enable' command.
data PCSSEnable = PCSSEnable
  deriving (Eq, Show)
pCSSEnable
  :: PCSSEnable
pCSSEnable
  = PCSSEnable
instance ToJSON PCSSEnable where
  toJSON _ = A.Null
instance Command PCSSEnable where
  type CommandResponse PCSSEnable = ()
  commandName _ = "CSS.enable"
  fromJSON = const . A.Success . const ()

-- | Ensures that the given node will have specified pseudo-classes whenever its style is computed by
--   the browser.

-- | Parameters of the 'CSS.forcePseudoState' command.
data PCSSForcePseudoState = PCSSForcePseudoState
  {
    -- | The element id for which to force the pseudo state.
    pCSSForcePseudoStateNodeId :: DOMNetworkEmulationPageSecurity.DOMNodeId,
    -- | Element pseudo classes to force when computing the element's style.
    pCSSForcePseudoStateForcedPseudoClasses :: [T.Text]
  }
  deriving (Eq, Show)
pCSSForcePseudoState
  {-
  -- | The element id for which to force the pseudo state.
  -}
  :: DOMNetworkEmulationPageSecurity.DOMNodeId
  {-
  -- | Element pseudo classes to force when computing the element's style.
  -}
  -> [T.Text]
  -> PCSSForcePseudoState
pCSSForcePseudoState
  arg_pCSSForcePseudoStateNodeId
  arg_pCSSForcePseudoStateForcedPseudoClasses
  = PCSSForcePseudoState
    arg_pCSSForcePseudoStateNodeId
    arg_pCSSForcePseudoStateForcedPseudoClasses
instance ToJSON PCSSForcePseudoState where
  toJSON p = A.object $ catMaybes [
    ("nodeId" A..=) <$> Just (pCSSForcePseudoStateNodeId p),
    ("forcedPseudoClasses" A..=) <$> Just (pCSSForcePseudoStateForcedPseudoClasses p)
    ]
instance Command PCSSForcePseudoState where
  type CommandResponse PCSSForcePseudoState = ()
  commandName _ = "CSS.forcePseudoState"
  fromJSON = const . A.Success . const ()

-- | Ensures that the given node is in its starting-style state.

-- | Parameters of the 'CSS.forceStartingStyle' command.
data PCSSForceStartingStyle = PCSSForceStartingStyle
  {
    -- | The element id for which to force the starting-style state.
    pCSSForceStartingStyleNodeId :: DOMNetworkEmulationPageSecurity.DOMNodeId,
    -- | Boolean indicating if this is on or off.
    pCSSForceStartingStyleForced :: Bool
  }
  deriving (Eq, Show)
pCSSForceStartingStyle
  {-
  -- | The element id for which to force the starting-style state.
  -}
  :: DOMNetworkEmulationPageSecurity.DOMNodeId
  {-
  -- | Boolean indicating if this is on or off.
  -}
  -> Bool
  -> PCSSForceStartingStyle
pCSSForceStartingStyle
  arg_pCSSForceStartingStyleNodeId
  arg_pCSSForceStartingStyleForced
  = PCSSForceStartingStyle
    arg_pCSSForceStartingStyleNodeId
    arg_pCSSForceStartingStyleForced
instance ToJSON PCSSForceStartingStyle where
  toJSON p = A.object $ catMaybes [
    ("nodeId" A..=) <$> Just (pCSSForceStartingStyleNodeId p),
    ("forced" A..=) <$> Just (pCSSForceStartingStyleForced p)
    ]
instance Command PCSSForceStartingStyle where
  type CommandResponse PCSSForceStartingStyle = ()
  commandName _ = "CSS.forceStartingStyle"
  fromJSON = const . A.Success . const ()


-- | Parameters of the 'CSS.getBackgroundColors' command.
data PCSSGetBackgroundColors = PCSSGetBackgroundColors
  {
    -- | Id of the node to get background colors for.
    pCSSGetBackgroundColorsNodeId :: DOMNetworkEmulationPageSecurity.DOMNodeId
  }
  deriving (Eq, Show)
pCSSGetBackgroundColors
  {-
  -- | Id of the node to get background colors for.
  -}
  :: DOMNetworkEmulationPageSecurity.DOMNodeId
  -> PCSSGetBackgroundColors
pCSSGetBackgroundColors
  arg_pCSSGetBackgroundColorsNodeId
  = PCSSGetBackgroundColors
    arg_pCSSGetBackgroundColorsNodeId
instance ToJSON PCSSGetBackgroundColors where
  toJSON p = A.object $ catMaybes [
    ("nodeId" A..=) <$> Just (pCSSGetBackgroundColorsNodeId p)
    ]
data CSSGetBackgroundColors = CSSGetBackgroundColors
  {
    -- | The range of background colors behind this element, if it contains any visible text. If no
    --   visible text is present, this will be undefined. In the case of a flat background color,
    --   this will consist of simply that color. In the case of a gradient, this will consist of each
    --   of the color stops. For anything more complicated, this will be an empty array. Images will
    --   be ignored (as if the image had failed to load).
    cSSGetBackgroundColorsBackgroundColors :: Maybe [T.Text],
    -- | The computed font size for this node, as a CSS computed value string (e.g. '12px').
    cSSGetBackgroundColorsComputedFontSize :: Maybe T.Text,
    -- | The computed font weight for this node, as a CSS computed value string (e.g. 'normal' or
    --   '100').
    cSSGetBackgroundColorsComputedFontWeight :: Maybe T.Text
  }
  deriving (Eq, Show)
instance FromJSON CSSGetBackgroundColors where
  parseJSON = A.withObject "CSSGetBackgroundColors" $ \o -> CSSGetBackgroundColors
    <$> o A..:? "backgroundColors"
    <*> o A..:? "computedFontSize"
    <*> o A..:? "computedFontWeight"
instance Command PCSSGetBackgroundColors where
  type CommandResponse PCSSGetBackgroundColors = CSSGetBackgroundColors
  commandName _ = "CSS.getBackgroundColors"

-- | Returns the computed style for a DOM node identified by `nodeId`.

-- | Parameters of the 'CSS.getComputedStyleForNode' command.
data PCSSGetComputedStyleForNode = PCSSGetComputedStyleForNode
  {
    pCSSGetComputedStyleForNodeNodeId :: DOMNetworkEmulationPageSecurity.DOMNodeId
  }
  deriving (Eq, Show)
pCSSGetComputedStyleForNode
  :: DOMNetworkEmulationPageSecurity.DOMNodeId
  -> PCSSGetComputedStyleForNode
pCSSGetComputedStyleForNode
  arg_pCSSGetComputedStyleForNodeNodeId
  = PCSSGetComputedStyleForNode
    arg_pCSSGetComputedStyleForNodeNodeId
instance ToJSON PCSSGetComputedStyleForNode where
  toJSON p = A.object $ catMaybes [
    ("nodeId" A..=) <$> Just (pCSSGetComputedStyleForNodeNodeId p)
    ]
data CSSGetComputedStyleForNode = CSSGetComputedStyleForNode
  {
    -- | Computed style for the specified DOM node.
    cSSGetComputedStyleForNodeComputedStyle :: [CSSCSSComputedStyleProperty],
    -- | A list of non-standard "extra fields" which blink stores alongside each
    --   computed style.
    cSSGetComputedStyleForNodeExtraFields :: CSSComputedStyleExtraFields
  }
  deriving (Eq, Show)
instance FromJSON CSSGetComputedStyleForNode where
  parseJSON = A.withObject "CSSGetComputedStyleForNode" $ \o -> CSSGetComputedStyleForNode
    <$> o A..: "computedStyle"
    <*> o A..: "extraFields"
instance Command PCSSGetComputedStyleForNode where
  type CommandResponse PCSSGetComputedStyleForNode = CSSGetComputedStyleForNode
  commandName _ = "CSS.getComputedStyleForNode"

-- | Resolve the specified values in the context of the provided element.
--   For example, a value of '1em' is evaluated according to the computed
--   'font-size' of the element and a value 'calc(1px + 2px)' will be
--   resolved to '3px'.
--   If the `propertyName` was specified the `values` are resolved as if
--   they were property's declaration. If a value cannot be parsed according
--   to the provided property syntax, the value is parsed using combined
--   syntax as if null `propertyName` was provided. If the value cannot be
--   resolved even then, return the provided value without any changes.
--   Note: this function currently does not resolve CSS random() function,
--   it returns unmodified random() function parts.`

-- | Parameters of the 'CSS.resolveValues' command.
data PCSSResolveValues = PCSSResolveValues
  {
    -- | Cascade-dependent keywords (revert/revert-layer) do not work.
    pCSSResolveValuesValues :: [T.Text],
    -- | Id of the node in whose context the expression is evaluated
    pCSSResolveValuesNodeId :: DOMNetworkEmulationPageSecurity.DOMNodeId,
    -- | Only longhands and custom property names are accepted.
    pCSSResolveValuesPropertyName :: Maybe T.Text,
    -- | Pseudo element type, only works for pseudo elements that generate
    --   elements in the tree, such as ::before and ::after.
    pCSSResolveValuesPseudoType :: Maybe DOMNetworkEmulationPageSecurity.DOMPseudoType,
    -- | Pseudo element custom ident.
    pCSSResolveValuesPseudoIdentifier :: Maybe T.Text
  }
  deriving (Eq, Show)
pCSSResolveValues
  {-
  -- | Cascade-dependent keywords (revert/revert-layer) do not work.
  -}
  :: [T.Text]
  {-
  -- | Id of the node in whose context the expression is evaluated
  -}
  -> DOMNetworkEmulationPageSecurity.DOMNodeId
  -> PCSSResolveValues
pCSSResolveValues
  arg_pCSSResolveValuesValues
  arg_pCSSResolveValuesNodeId
  = PCSSResolveValues
    arg_pCSSResolveValuesValues
    arg_pCSSResolveValuesNodeId
    Nothing
    Nothing
    Nothing
instance ToJSON PCSSResolveValues where
  toJSON p = A.object $ catMaybes [
    ("values" A..=) <$> Just (pCSSResolveValuesValues p),
    ("nodeId" A..=) <$> Just (pCSSResolveValuesNodeId p),
    ("propertyName" A..=) <$> (pCSSResolveValuesPropertyName p),
    ("pseudoType" A..=) <$> (pCSSResolveValuesPseudoType p),
    ("pseudoIdentifier" A..=) <$> (pCSSResolveValuesPseudoIdentifier p)
    ]
data CSSResolveValues = CSSResolveValues
  {
    cSSResolveValuesResults :: [T.Text]
  }
  deriving (Eq, Show)
instance FromJSON CSSResolveValues where
  parseJSON = A.withObject "CSSResolveValues" $ \o -> CSSResolveValues
    <$> o A..: "results"
instance Command PCSSResolveValues where
  type CommandResponse PCSSResolveValues = CSSResolveValues
  commandName _ = "CSS.resolveValues"


-- | Parameters of the 'CSS.getLonghandProperties' command.
data PCSSGetLonghandProperties = PCSSGetLonghandProperties
  {
    pCSSGetLonghandPropertiesShorthandName :: T.Text,
    pCSSGetLonghandPropertiesValue :: T.Text
  }
  deriving (Eq, Show)
pCSSGetLonghandProperties
  :: T.Text
  -> T.Text
  -> PCSSGetLonghandProperties
pCSSGetLonghandProperties
  arg_pCSSGetLonghandPropertiesShorthandName
  arg_pCSSGetLonghandPropertiesValue
  = PCSSGetLonghandProperties
    arg_pCSSGetLonghandPropertiesShorthandName
    arg_pCSSGetLonghandPropertiesValue
instance ToJSON PCSSGetLonghandProperties where
  toJSON p = A.object $ catMaybes [
    ("shorthandName" A..=) <$> Just (pCSSGetLonghandPropertiesShorthandName p),
    ("value" A..=) <$> Just (pCSSGetLonghandPropertiesValue p)
    ]
data CSSGetLonghandProperties = CSSGetLonghandProperties
  {
    cSSGetLonghandPropertiesLonghandProperties :: [CSSCSSProperty]
  }
  deriving (Eq, Show)
instance FromJSON CSSGetLonghandProperties where
  parseJSON = A.withObject "CSSGetLonghandProperties" $ \o -> CSSGetLonghandProperties
    <$> o A..: "longhandProperties"
instance Command PCSSGetLonghandProperties where
  type CommandResponse PCSSGetLonghandProperties = CSSGetLonghandProperties
  commandName _ = "CSS.getLonghandProperties"

-- | Returns the styles defined inline (explicitly in the "style" attribute and implicitly, using DOM
--   attributes) for a DOM node identified by `nodeId`.

-- | Parameters of the 'CSS.getInlineStylesForNode' command.
data PCSSGetInlineStylesForNode = PCSSGetInlineStylesForNode
  {
    pCSSGetInlineStylesForNodeNodeId :: DOMNetworkEmulationPageSecurity.DOMNodeId
  }
  deriving (Eq, Show)
pCSSGetInlineStylesForNode
  :: DOMNetworkEmulationPageSecurity.DOMNodeId
  -> PCSSGetInlineStylesForNode
pCSSGetInlineStylesForNode
  arg_pCSSGetInlineStylesForNodeNodeId
  = PCSSGetInlineStylesForNode
    arg_pCSSGetInlineStylesForNodeNodeId
instance ToJSON PCSSGetInlineStylesForNode where
  toJSON p = A.object $ catMaybes [
    ("nodeId" A..=) <$> Just (pCSSGetInlineStylesForNodeNodeId p)
    ]
data CSSGetInlineStylesForNode = CSSGetInlineStylesForNode
  {
    -- | Inline style for the specified DOM node.
    cSSGetInlineStylesForNodeInlineStyle :: Maybe CSSCSSStyle,
    -- | Attribute-defined element style (e.g. resulting from "width=20 height=100%").
    cSSGetInlineStylesForNodeAttributesStyle :: Maybe CSSCSSStyle
  }
  deriving (Eq, Show)
instance FromJSON CSSGetInlineStylesForNode where
  parseJSON = A.withObject "CSSGetInlineStylesForNode" $ \o -> CSSGetInlineStylesForNode
    <$> o A..:? "inlineStyle"
    <*> o A..:? "attributesStyle"
instance Command PCSSGetInlineStylesForNode where
  type CommandResponse PCSSGetInlineStylesForNode = CSSGetInlineStylesForNode
  commandName _ = "CSS.getInlineStylesForNode"

-- | Returns the styles coming from animations & transitions
--   including the animation & transition styles coming from inheritance chain.

-- | Parameters of the 'CSS.getAnimatedStylesForNode' command.
data PCSSGetAnimatedStylesForNode = PCSSGetAnimatedStylesForNode
  {
    pCSSGetAnimatedStylesForNodeNodeId :: DOMNetworkEmulationPageSecurity.DOMNodeId
  }
  deriving (Eq, Show)
pCSSGetAnimatedStylesForNode
  :: DOMNetworkEmulationPageSecurity.DOMNodeId
  -> PCSSGetAnimatedStylesForNode
pCSSGetAnimatedStylesForNode
  arg_pCSSGetAnimatedStylesForNodeNodeId
  = PCSSGetAnimatedStylesForNode
    arg_pCSSGetAnimatedStylesForNodeNodeId
instance ToJSON PCSSGetAnimatedStylesForNode where
  toJSON p = A.object $ catMaybes [
    ("nodeId" A..=) <$> Just (pCSSGetAnimatedStylesForNodeNodeId p)
    ]
data CSSGetAnimatedStylesForNode = CSSGetAnimatedStylesForNode
  {
    -- | Styles coming from animations.
    cSSGetAnimatedStylesForNodeAnimationStyles :: Maybe [CSSCSSAnimationStyle],
    -- | Style coming from transitions.
    cSSGetAnimatedStylesForNodeTransitionsStyle :: Maybe CSSCSSStyle,
    -- | Inherited style entries for animationsStyle and transitionsStyle from
    --   the inheritance chain of the element.
    cSSGetAnimatedStylesForNodeInherited :: Maybe [CSSInheritedAnimatedStyleEntry]
  }
  deriving (Eq, Show)
instance FromJSON CSSGetAnimatedStylesForNode where
  parseJSON = A.withObject "CSSGetAnimatedStylesForNode" $ \o -> CSSGetAnimatedStylesForNode
    <$> o A..:? "animationStyles"
    <*> o A..:? "transitionsStyle"
    <*> o A..:? "inherited"
instance Command PCSSGetAnimatedStylesForNode where
  type CommandResponse PCSSGetAnimatedStylesForNode = CSSGetAnimatedStylesForNode
  commandName _ = "CSS.getAnimatedStylesForNode"

-- | Returns requested styles for a DOM node identified by `nodeId`.

-- | Parameters of the 'CSS.getMatchedStylesForNode' command.
data PCSSGetMatchedStylesForNode = PCSSGetMatchedStylesForNode
  {
    pCSSGetMatchedStylesForNodeNodeId :: DOMNetworkEmulationPageSecurity.DOMNodeId
  }
  deriving (Eq, Show)
pCSSGetMatchedStylesForNode
  :: DOMNetworkEmulationPageSecurity.DOMNodeId
  -> PCSSGetMatchedStylesForNode
pCSSGetMatchedStylesForNode
  arg_pCSSGetMatchedStylesForNodeNodeId
  = PCSSGetMatchedStylesForNode
    arg_pCSSGetMatchedStylesForNodeNodeId
instance ToJSON PCSSGetMatchedStylesForNode where
  toJSON p = A.object $ catMaybes [
    ("nodeId" A..=) <$> Just (pCSSGetMatchedStylesForNodeNodeId p)
    ]
data CSSGetMatchedStylesForNode = CSSGetMatchedStylesForNode
  {
    -- | Inline style for the specified DOM node.
    cSSGetMatchedStylesForNodeInlineStyle :: Maybe CSSCSSStyle,
    -- | Attribute-defined element style (e.g. resulting from "width=20 height=100%").
    cSSGetMatchedStylesForNodeAttributesStyle :: Maybe CSSCSSStyle,
    -- | CSS rules matching this node, from all applicable stylesheets.
    cSSGetMatchedStylesForNodeMatchedCSSRules :: Maybe [CSSRuleMatch],
    -- | Pseudo style matches for this node.
    cSSGetMatchedStylesForNodePseudoElements :: Maybe [CSSPseudoElementMatches],
    -- | A chain of inherited styles (from the immediate node parent up to the DOM tree root).
    cSSGetMatchedStylesForNodeInherited :: Maybe [CSSInheritedStyleEntry],
    -- | A chain of inherited pseudo element styles (from the immediate node parent up to the DOM tree root).
    cSSGetMatchedStylesForNodeInheritedPseudoElements :: Maybe [CSSInheritedPseudoElementMatches],
    -- | A list of CSS keyframed animations matching this node.
    cSSGetMatchedStylesForNodeCssKeyframesRules :: Maybe [CSSCSSKeyframesRule],
    -- | A list of CSS @position-try rules matching this node, based on the position-try-fallbacks property.
    cSSGetMatchedStylesForNodeCssPositionTryRules :: Maybe [CSSCSSPositionTryRule],
    -- | Index of the active fallback in the applied position-try-fallback property,
    --   will not be set if there is no active position-try fallback.
    cSSGetMatchedStylesForNodeActivePositionFallbackIndex :: Maybe Int,
    -- | A list of CSS at-property rules matching this node.
    cSSGetMatchedStylesForNodeCssPropertyRules :: Maybe [CSSCSSPropertyRule],
    -- | A list of CSS property registrations matching this node.
    cSSGetMatchedStylesForNodeCssPropertyRegistrations :: Maybe [CSSCSSPropertyRegistration],
    -- | A list of simple @rules matching this node or its pseudo-elements.
    cSSGetMatchedStylesForNodeCssAtRules :: Maybe [CSSCSSAtRule],
    -- | Id of the first parent element that does not have display: contents.
    cSSGetMatchedStylesForNodeParentLayoutNodeId :: Maybe DOMNetworkEmulationPageSecurity.DOMNodeId,
    -- | A list of CSS at-function rules referenced by styles of this node.
    cSSGetMatchedStylesForNodeCssFunctionRules :: Maybe [CSSCSSFunctionRule]
  }
  deriving (Eq, Show)
instance FromJSON CSSGetMatchedStylesForNode where
  parseJSON = A.withObject "CSSGetMatchedStylesForNode" $ \o -> CSSGetMatchedStylesForNode
    <$> o A..:? "inlineStyle"
    <*> o A..:? "attributesStyle"
    <*> o A..:? "matchedCSSRules"
    <*> o A..:? "pseudoElements"
    <*> o A..:? "inherited"
    <*> o A..:? "inheritedPseudoElements"
    <*> o A..:? "cssKeyframesRules"
    <*> o A..:? "cssPositionTryRules"
    <*> o A..:? "activePositionFallbackIndex"
    <*> o A..:? "cssPropertyRules"
    <*> o A..:? "cssPropertyRegistrations"
    <*> o A..:? "cssAtRules"
    <*> o A..:? "parentLayoutNodeId"
    <*> o A..:? "cssFunctionRules"
instance Command PCSSGetMatchedStylesForNode where
  type CommandResponse PCSSGetMatchedStylesForNode = CSSGetMatchedStylesForNode
  commandName _ = "CSS.getMatchedStylesForNode"

-- | Returns the values of the default UA-defined environment variables used in env()

-- | Parameters of the 'CSS.getEnvironmentVariables' command.
data PCSSGetEnvironmentVariables = PCSSGetEnvironmentVariables
  deriving (Eq, Show)
pCSSGetEnvironmentVariables
  :: PCSSGetEnvironmentVariables
pCSSGetEnvironmentVariables
  = PCSSGetEnvironmentVariables
instance ToJSON PCSSGetEnvironmentVariables where
  toJSON _ = A.Null
data CSSGetEnvironmentVariables = CSSGetEnvironmentVariables
  {
    cSSGetEnvironmentVariablesEnvironmentVariables :: [(T.Text, T.Text)]
  }
  deriving (Eq, Show)
instance FromJSON CSSGetEnvironmentVariables where
  parseJSON = A.withObject "CSSGetEnvironmentVariables" $ \o -> CSSGetEnvironmentVariables
    <$> o A..: "environmentVariables"
instance Command PCSSGetEnvironmentVariables where
  type CommandResponse PCSSGetEnvironmentVariables = CSSGetEnvironmentVariables
  commandName _ = "CSS.getEnvironmentVariables"

-- | Returns all media queries parsed by the rendering engine.

-- | Parameters of the 'CSS.getMediaQueries' command.
data PCSSGetMediaQueries = PCSSGetMediaQueries
  deriving (Eq, Show)
pCSSGetMediaQueries
  :: PCSSGetMediaQueries
pCSSGetMediaQueries
  = PCSSGetMediaQueries
instance ToJSON PCSSGetMediaQueries where
  toJSON _ = A.Null
data CSSGetMediaQueries = CSSGetMediaQueries
  {
    cSSGetMediaQueriesMedias :: [CSSCSSMedia]
  }
  deriving (Eq, Show)
instance FromJSON CSSGetMediaQueries where
  parseJSON = A.withObject "CSSGetMediaQueries" $ \o -> CSSGetMediaQueries
    <$> o A..: "medias"
instance Command PCSSGetMediaQueries where
  type CommandResponse PCSSGetMediaQueries = CSSGetMediaQueries
  commandName _ = "CSS.getMediaQueries"

-- | Requests information about platform fonts which we used to render child TextNodes in the given
--   node.

-- | Parameters of the 'CSS.getPlatformFontsForNode' command.
data PCSSGetPlatformFontsForNode = PCSSGetPlatformFontsForNode
  {
    pCSSGetPlatformFontsForNodeNodeId :: DOMNetworkEmulationPageSecurity.DOMNodeId
  }
  deriving (Eq, Show)
pCSSGetPlatformFontsForNode
  :: DOMNetworkEmulationPageSecurity.DOMNodeId
  -> PCSSGetPlatformFontsForNode
pCSSGetPlatformFontsForNode
  arg_pCSSGetPlatformFontsForNodeNodeId
  = PCSSGetPlatformFontsForNode
    arg_pCSSGetPlatformFontsForNodeNodeId
instance ToJSON PCSSGetPlatformFontsForNode where
  toJSON p = A.object $ catMaybes [
    ("nodeId" A..=) <$> Just (pCSSGetPlatformFontsForNodeNodeId p)
    ]
data CSSGetPlatformFontsForNode = CSSGetPlatformFontsForNode
  {
    -- | Usage statistics for every employed platform font.
    cSSGetPlatformFontsForNodeFonts :: [CSSPlatformFontUsage]
  }
  deriving (Eq, Show)
instance FromJSON CSSGetPlatformFontsForNode where
  parseJSON = A.withObject "CSSGetPlatformFontsForNode" $ \o -> CSSGetPlatformFontsForNode
    <$> o A..: "fonts"
instance Command PCSSGetPlatformFontsForNode where
  type CommandResponse PCSSGetPlatformFontsForNode = CSSGetPlatformFontsForNode
  commandName _ = "CSS.getPlatformFontsForNode"

-- | Returns the current textual content for a stylesheet.

-- | Parameters of the 'CSS.getStyleSheetText' command.
data PCSSGetStyleSheetText = PCSSGetStyleSheetText
  {
    pCSSGetStyleSheetTextStyleSheetId :: DOMNetworkEmulationPageSecurity.DOMStyleSheetId
  }
  deriving (Eq, Show)
pCSSGetStyleSheetText
  :: DOMNetworkEmulationPageSecurity.DOMStyleSheetId
  -> PCSSGetStyleSheetText
pCSSGetStyleSheetText
  arg_pCSSGetStyleSheetTextStyleSheetId
  = PCSSGetStyleSheetText
    arg_pCSSGetStyleSheetTextStyleSheetId
instance ToJSON PCSSGetStyleSheetText where
  toJSON p = A.object $ catMaybes [
    ("styleSheetId" A..=) <$> Just (pCSSGetStyleSheetTextStyleSheetId p)
    ]
data CSSGetStyleSheetText = CSSGetStyleSheetText
  {
    -- | The stylesheet text.
    cSSGetStyleSheetTextText :: T.Text
  }
  deriving (Eq, Show)
instance FromJSON CSSGetStyleSheetText where
  parseJSON = A.withObject "CSSGetStyleSheetText" $ \o -> CSSGetStyleSheetText
    <$> o A..: "text"
instance Command PCSSGetStyleSheetText where
  type CommandResponse PCSSGetStyleSheetText = CSSGetStyleSheetText
  commandName _ = "CSS.getStyleSheetText"

-- | Returns all layers parsed by the rendering engine for the tree scope of a node.
--   Given a DOM element identified by nodeId, getLayersForNode returns the root
--   layer for the nearest ancestor document or shadow root. The layer root contains
--   the full layer tree for the tree scope and their ordering.

-- | Parameters of the 'CSS.getLayersForNode' command.
data PCSSGetLayersForNode = PCSSGetLayersForNode
  {
    pCSSGetLayersForNodeNodeId :: DOMNetworkEmulationPageSecurity.DOMNodeId
  }
  deriving (Eq, Show)
pCSSGetLayersForNode
  :: DOMNetworkEmulationPageSecurity.DOMNodeId
  -> PCSSGetLayersForNode
pCSSGetLayersForNode
  arg_pCSSGetLayersForNodeNodeId
  = PCSSGetLayersForNode
    arg_pCSSGetLayersForNodeNodeId
instance ToJSON PCSSGetLayersForNode where
  toJSON p = A.object $ catMaybes [
    ("nodeId" A..=) <$> Just (pCSSGetLayersForNodeNodeId p)
    ]
data CSSGetLayersForNode = CSSGetLayersForNode
  {
    cSSGetLayersForNodeRootLayer :: CSSCSSLayerData
  }
  deriving (Eq, Show)
instance FromJSON CSSGetLayersForNode where
  parseJSON = A.withObject "CSSGetLayersForNode" $ \o -> CSSGetLayersForNode
    <$> o A..: "rootLayer"
instance Command PCSSGetLayersForNode where
  type CommandResponse PCSSGetLayersForNode = CSSGetLayersForNode
  commandName _ = "CSS.getLayersForNode"

-- | Given a CSS selector text and a style sheet ID, getLocationForSelector
--   returns an array of locations of the CSS selector in the style sheet.

-- | Parameters of the 'CSS.getLocationForSelector' command.
data PCSSGetLocationForSelector = PCSSGetLocationForSelector
  {
    pCSSGetLocationForSelectorStyleSheetId :: DOMNetworkEmulationPageSecurity.DOMStyleSheetId,
    pCSSGetLocationForSelectorSelectorText :: T.Text
  }
  deriving (Eq, Show)
pCSSGetLocationForSelector
  :: DOMNetworkEmulationPageSecurity.DOMStyleSheetId
  -> T.Text
  -> PCSSGetLocationForSelector
pCSSGetLocationForSelector
  arg_pCSSGetLocationForSelectorStyleSheetId
  arg_pCSSGetLocationForSelectorSelectorText
  = PCSSGetLocationForSelector
    arg_pCSSGetLocationForSelectorStyleSheetId
    arg_pCSSGetLocationForSelectorSelectorText
instance ToJSON PCSSGetLocationForSelector where
  toJSON p = A.object $ catMaybes [
    ("styleSheetId" A..=) <$> Just (pCSSGetLocationForSelectorStyleSheetId p),
    ("selectorText" A..=) <$> Just (pCSSGetLocationForSelectorSelectorText p)
    ]
data CSSGetLocationForSelector = CSSGetLocationForSelector
  {
    cSSGetLocationForSelectorRanges :: [CSSSourceRange]
  }
  deriving (Eq, Show)
instance FromJSON CSSGetLocationForSelector where
  parseJSON = A.withObject "CSSGetLocationForSelector" $ \o -> CSSGetLocationForSelector
    <$> o A..: "ranges"
instance Command PCSSGetLocationForSelector where
  type CommandResponse PCSSGetLocationForSelector = CSSGetLocationForSelector
  commandName _ = "CSS.getLocationForSelector"

-- | Starts tracking the given node for the computed style updates
--   and whenever the computed style is updated for node, it queues
--   a `computedStyleUpdated` event with throttling.
--   There can only be 1 node tracked for computed style updates
--   so passing a new node id removes tracking from the previous node.
--   Pass `undefined` to disable tracking.

-- | Parameters of the 'CSS.trackComputedStyleUpdatesForNode' command.
data PCSSTrackComputedStyleUpdatesForNode = PCSSTrackComputedStyleUpdatesForNode
  {
    pCSSTrackComputedStyleUpdatesForNodeNodeId :: Maybe DOMNetworkEmulationPageSecurity.DOMNodeId
  }
  deriving (Eq, Show)
pCSSTrackComputedStyleUpdatesForNode
  :: PCSSTrackComputedStyleUpdatesForNode
pCSSTrackComputedStyleUpdatesForNode
  = PCSSTrackComputedStyleUpdatesForNode
    Nothing
instance ToJSON PCSSTrackComputedStyleUpdatesForNode where
  toJSON p = A.object $ catMaybes [
    ("nodeId" A..=) <$> (pCSSTrackComputedStyleUpdatesForNodeNodeId p)
    ]
instance Command PCSSTrackComputedStyleUpdatesForNode where
  type CommandResponse PCSSTrackComputedStyleUpdatesForNode = ()
  commandName _ = "CSS.trackComputedStyleUpdatesForNode"
  fromJSON = const . A.Success . const ()

-- | Starts tracking the given computed styles for updates. The specified array of properties
--   replaces the one previously specified. Pass empty array to disable tracking.
--   Use takeComputedStyleUpdates to retrieve the list of nodes that had properties modified.
--   The changes to computed style properties are only tracked for nodes pushed to the front-end
--   by the DOM agent. If no changes to the tracked properties occur after the node has been pushed
--   to the front-end, no updates will be issued for the node.

-- | Parameters of the 'CSS.trackComputedStyleUpdates' command.
data PCSSTrackComputedStyleUpdates = PCSSTrackComputedStyleUpdates
  {
    pCSSTrackComputedStyleUpdatesPropertiesToTrack :: [CSSCSSComputedStyleProperty]
  }
  deriving (Eq, Show)
pCSSTrackComputedStyleUpdates
  :: [CSSCSSComputedStyleProperty]
  -> PCSSTrackComputedStyleUpdates
pCSSTrackComputedStyleUpdates
  arg_pCSSTrackComputedStyleUpdatesPropertiesToTrack
  = PCSSTrackComputedStyleUpdates
    arg_pCSSTrackComputedStyleUpdatesPropertiesToTrack
instance ToJSON PCSSTrackComputedStyleUpdates where
  toJSON p = A.object $ catMaybes [
    ("propertiesToTrack" A..=) <$> Just (pCSSTrackComputedStyleUpdatesPropertiesToTrack p)
    ]
instance Command PCSSTrackComputedStyleUpdates where
  type CommandResponse PCSSTrackComputedStyleUpdates = ()
  commandName _ = "CSS.trackComputedStyleUpdates"
  fromJSON = const . A.Success . const ()

-- | Polls the next batch of computed style updates.

-- | Parameters of the 'CSS.takeComputedStyleUpdates' command.
data PCSSTakeComputedStyleUpdates = PCSSTakeComputedStyleUpdates
  deriving (Eq, Show)
pCSSTakeComputedStyleUpdates
  :: PCSSTakeComputedStyleUpdates
pCSSTakeComputedStyleUpdates
  = PCSSTakeComputedStyleUpdates
instance ToJSON PCSSTakeComputedStyleUpdates where
  toJSON _ = A.Null
data CSSTakeComputedStyleUpdates = CSSTakeComputedStyleUpdates
  {
    -- | The list of node Ids that have their tracked computed styles updated.
    cSSTakeComputedStyleUpdatesNodeIds :: [DOMNetworkEmulationPageSecurity.DOMNodeId]
  }
  deriving (Eq, Show)
instance FromJSON CSSTakeComputedStyleUpdates where
  parseJSON = A.withObject "CSSTakeComputedStyleUpdates" $ \o -> CSSTakeComputedStyleUpdates
    <$> o A..: "nodeIds"
instance Command PCSSTakeComputedStyleUpdates where
  type CommandResponse PCSSTakeComputedStyleUpdates = CSSTakeComputedStyleUpdates
  commandName _ = "CSS.takeComputedStyleUpdates"

-- | Find a rule with the given active property for the given node and set the new value for this
--   property

-- | Parameters of the 'CSS.setEffectivePropertyValueForNode' command.
data PCSSSetEffectivePropertyValueForNode = PCSSSetEffectivePropertyValueForNode
  {
    -- | The element id for which to set property.
    pCSSSetEffectivePropertyValueForNodeNodeId :: DOMNetworkEmulationPageSecurity.DOMNodeId,
    pCSSSetEffectivePropertyValueForNodePropertyName :: T.Text,
    pCSSSetEffectivePropertyValueForNodeValue :: T.Text
  }
  deriving (Eq, Show)
pCSSSetEffectivePropertyValueForNode
  {-
  -- | The element id for which to set property.
  -}
  :: DOMNetworkEmulationPageSecurity.DOMNodeId
  -> T.Text
  -> T.Text
  -> PCSSSetEffectivePropertyValueForNode
pCSSSetEffectivePropertyValueForNode
  arg_pCSSSetEffectivePropertyValueForNodeNodeId
  arg_pCSSSetEffectivePropertyValueForNodePropertyName
  arg_pCSSSetEffectivePropertyValueForNodeValue
  = PCSSSetEffectivePropertyValueForNode
    arg_pCSSSetEffectivePropertyValueForNodeNodeId
    arg_pCSSSetEffectivePropertyValueForNodePropertyName
    arg_pCSSSetEffectivePropertyValueForNodeValue
instance ToJSON PCSSSetEffectivePropertyValueForNode where
  toJSON p = A.object $ catMaybes [
    ("nodeId" A..=) <$> Just (pCSSSetEffectivePropertyValueForNodeNodeId p),
    ("propertyName" A..=) <$> Just (pCSSSetEffectivePropertyValueForNodePropertyName p),
    ("value" A..=) <$> Just (pCSSSetEffectivePropertyValueForNodeValue p)
    ]
instance Command PCSSSetEffectivePropertyValueForNode where
  type CommandResponse PCSSSetEffectivePropertyValueForNode = ()
  commandName _ = "CSS.setEffectivePropertyValueForNode"
  fromJSON = const . A.Success . const ()

-- | Modifies the property rule property name.

-- | Parameters of the 'CSS.setPropertyRulePropertyName' command.
data PCSSSetPropertyRulePropertyName = PCSSSetPropertyRulePropertyName
  {
    pCSSSetPropertyRulePropertyNameStyleSheetId :: DOMNetworkEmulationPageSecurity.DOMStyleSheetId,
    pCSSSetPropertyRulePropertyNameRange :: CSSSourceRange,
    pCSSSetPropertyRulePropertyNamePropertyName :: T.Text
  }
  deriving (Eq, Show)
pCSSSetPropertyRulePropertyName
  :: DOMNetworkEmulationPageSecurity.DOMStyleSheetId
  -> CSSSourceRange
  -> T.Text
  -> PCSSSetPropertyRulePropertyName
pCSSSetPropertyRulePropertyName
  arg_pCSSSetPropertyRulePropertyNameStyleSheetId
  arg_pCSSSetPropertyRulePropertyNameRange
  arg_pCSSSetPropertyRulePropertyNamePropertyName
  = PCSSSetPropertyRulePropertyName
    arg_pCSSSetPropertyRulePropertyNameStyleSheetId
    arg_pCSSSetPropertyRulePropertyNameRange
    arg_pCSSSetPropertyRulePropertyNamePropertyName
instance ToJSON PCSSSetPropertyRulePropertyName where
  toJSON p = A.object $ catMaybes [
    ("styleSheetId" A..=) <$> Just (pCSSSetPropertyRulePropertyNameStyleSheetId p),
    ("range" A..=) <$> Just (pCSSSetPropertyRulePropertyNameRange p),
    ("propertyName" A..=) <$> Just (pCSSSetPropertyRulePropertyNamePropertyName p)
    ]
data CSSSetPropertyRulePropertyName = CSSSetPropertyRulePropertyName
  {
    -- | The resulting key text after modification.
    cSSSetPropertyRulePropertyNamePropertyName :: CSSValue
  }
  deriving (Eq, Show)
instance FromJSON CSSSetPropertyRulePropertyName where
  parseJSON = A.withObject "CSSSetPropertyRulePropertyName" $ \o -> CSSSetPropertyRulePropertyName
    <$> o A..: "propertyName"
instance Command PCSSSetPropertyRulePropertyName where
  type CommandResponse PCSSSetPropertyRulePropertyName = CSSSetPropertyRulePropertyName
  commandName _ = "CSS.setPropertyRulePropertyName"

-- | Modifies the keyframe rule key text.

-- | Parameters of the 'CSS.setKeyframeKey' command.
data PCSSSetKeyframeKey = PCSSSetKeyframeKey
  {
    pCSSSetKeyframeKeyStyleSheetId :: DOMNetworkEmulationPageSecurity.DOMStyleSheetId,
    pCSSSetKeyframeKeyRange :: CSSSourceRange,
    pCSSSetKeyframeKeyKeyText :: T.Text
  }
  deriving (Eq, Show)
pCSSSetKeyframeKey
  :: DOMNetworkEmulationPageSecurity.DOMStyleSheetId
  -> CSSSourceRange
  -> T.Text
  -> PCSSSetKeyframeKey
pCSSSetKeyframeKey
  arg_pCSSSetKeyframeKeyStyleSheetId
  arg_pCSSSetKeyframeKeyRange
  arg_pCSSSetKeyframeKeyKeyText
  = PCSSSetKeyframeKey
    arg_pCSSSetKeyframeKeyStyleSheetId
    arg_pCSSSetKeyframeKeyRange
    arg_pCSSSetKeyframeKeyKeyText
instance ToJSON PCSSSetKeyframeKey where
  toJSON p = A.object $ catMaybes [
    ("styleSheetId" A..=) <$> Just (pCSSSetKeyframeKeyStyleSheetId p),
    ("range" A..=) <$> Just (pCSSSetKeyframeKeyRange p),
    ("keyText" A..=) <$> Just (pCSSSetKeyframeKeyKeyText p)
    ]
data CSSSetKeyframeKey = CSSSetKeyframeKey
  {
    -- | The resulting key text after modification.
    cSSSetKeyframeKeyKeyText :: CSSValue
  }
  deriving (Eq, Show)
instance FromJSON CSSSetKeyframeKey where
  parseJSON = A.withObject "CSSSetKeyframeKey" $ \o -> CSSSetKeyframeKey
    <$> o A..: "keyText"
instance Command PCSSSetKeyframeKey where
  type CommandResponse PCSSSetKeyframeKey = CSSSetKeyframeKey
  commandName _ = "CSS.setKeyframeKey"

-- | Modifies the rule selector.

-- | Parameters of the 'CSS.setMediaText' command.
data PCSSSetMediaText = PCSSSetMediaText
  {
    pCSSSetMediaTextStyleSheetId :: DOMNetworkEmulationPageSecurity.DOMStyleSheetId,
    pCSSSetMediaTextRange :: CSSSourceRange,
    pCSSSetMediaTextText :: T.Text
  }
  deriving (Eq, Show)
pCSSSetMediaText
  :: DOMNetworkEmulationPageSecurity.DOMStyleSheetId
  -> CSSSourceRange
  -> T.Text
  -> PCSSSetMediaText
pCSSSetMediaText
  arg_pCSSSetMediaTextStyleSheetId
  arg_pCSSSetMediaTextRange
  arg_pCSSSetMediaTextText
  = PCSSSetMediaText
    arg_pCSSSetMediaTextStyleSheetId
    arg_pCSSSetMediaTextRange
    arg_pCSSSetMediaTextText
instance ToJSON PCSSSetMediaText where
  toJSON p = A.object $ catMaybes [
    ("styleSheetId" A..=) <$> Just (pCSSSetMediaTextStyleSheetId p),
    ("range" A..=) <$> Just (pCSSSetMediaTextRange p),
    ("text" A..=) <$> Just (pCSSSetMediaTextText p)
    ]
data CSSSetMediaText = CSSSetMediaText
  {
    -- | The resulting CSS media rule after modification.
    cSSSetMediaTextMedia :: CSSCSSMedia
  }
  deriving (Eq, Show)
instance FromJSON CSSSetMediaText where
  parseJSON = A.withObject "CSSSetMediaText" $ \o -> CSSSetMediaText
    <$> o A..: "media"
instance Command PCSSSetMediaText where
  type CommandResponse PCSSSetMediaText = CSSSetMediaText
  commandName _ = "CSS.setMediaText"


-- | Parameters of the 'CSS.setContainerQueryConditionText' command.
data PCSSSetContainerQueryConditionText = PCSSSetContainerQueryConditionText
  {
    pCSSSetContainerQueryConditionTextStyleSheetId :: DOMNetworkEmulationPageSecurity.DOMStyleSheetId,
    pCSSSetContainerQueryConditionTextRange :: CSSSourceRange,
    pCSSSetContainerQueryConditionTextText :: T.Text
  }
  deriving (Eq, Show)
pCSSSetContainerQueryConditionText
  :: DOMNetworkEmulationPageSecurity.DOMStyleSheetId
  -> CSSSourceRange
  -> T.Text
  -> PCSSSetContainerQueryConditionText
pCSSSetContainerQueryConditionText
  arg_pCSSSetContainerQueryConditionTextStyleSheetId
  arg_pCSSSetContainerQueryConditionTextRange
  arg_pCSSSetContainerQueryConditionTextText
  = PCSSSetContainerQueryConditionText
    arg_pCSSSetContainerQueryConditionTextStyleSheetId
    arg_pCSSSetContainerQueryConditionTextRange
    arg_pCSSSetContainerQueryConditionTextText
instance ToJSON PCSSSetContainerQueryConditionText where
  toJSON p = A.object $ catMaybes [
    ("styleSheetId" A..=) <$> Just (pCSSSetContainerQueryConditionTextStyleSheetId p),
    ("range" A..=) <$> Just (pCSSSetContainerQueryConditionTextRange p),
    ("text" A..=) <$> Just (pCSSSetContainerQueryConditionTextText p)
    ]
data CSSSetContainerQueryConditionText = CSSSetContainerQueryConditionText
  {
    -- | The resulting CSS container query rule after modification.
    cSSSetContainerQueryConditionTextContainerQuery :: CSSCSSContainerQuery
  }
  deriving (Eq, Show)
instance FromJSON CSSSetContainerQueryConditionText where
  parseJSON = A.withObject "CSSSetContainerQueryConditionText" $ \o -> CSSSetContainerQueryConditionText
    <$> o A..: "containerQuery"
instance Command PCSSSetContainerQueryConditionText where
  type CommandResponse PCSSSetContainerQueryConditionText = CSSSetContainerQueryConditionText
  commandName _ = "CSS.setContainerQueryConditionText"

-- | Modifies the expression of a supports at-rule.

-- | Parameters of the 'CSS.setSupportsText' command.
data PCSSSetSupportsText = PCSSSetSupportsText
  {
    pCSSSetSupportsTextStyleSheetId :: DOMNetworkEmulationPageSecurity.DOMStyleSheetId,
    pCSSSetSupportsTextRange :: CSSSourceRange,
    pCSSSetSupportsTextText :: T.Text
  }
  deriving (Eq, Show)
pCSSSetSupportsText
  :: DOMNetworkEmulationPageSecurity.DOMStyleSheetId
  -> CSSSourceRange
  -> T.Text
  -> PCSSSetSupportsText
pCSSSetSupportsText
  arg_pCSSSetSupportsTextStyleSheetId
  arg_pCSSSetSupportsTextRange
  arg_pCSSSetSupportsTextText
  = PCSSSetSupportsText
    arg_pCSSSetSupportsTextStyleSheetId
    arg_pCSSSetSupportsTextRange
    arg_pCSSSetSupportsTextText
instance ToJSON PCSSSetSupportsText where
  toJSON p = A.object $ catMaybes [
    ("styleSheetId" A..=) <$> Just (pCSSSetSupportsTextStyleSheetId p),
    ("range" A..=) <$> Just (pCSSSetSupportsTextRange p),
    ("text" A..=) <$> Just (pCSSSetSupportsTextText p)
    ]
data CSSSetSupportsText = CSSSetSupportsText
  {
    -- | The resulting CSS Supports rule after modification.
    cSSSetSupportsTextSupports :: CSSCSSSupports
  }
  deriving (Eq, Show)
instance FromJSON CSSSetSupportsText where
  parseJSON = A.withObject "CSSSetSupportsText" $ \o -> CSSSetSupportsText
    <$> o A..: "supports"
instance Command PCSSSetSupportsText where
  type CommandResponse PCSSSetSupportsText = CSSSetSupportsText
  commandName _ = "CSS.setSupportsText"

-- | Modifies the expression of a navigation at-rule.

-- | Parameters of the 'CSS.setNavigationText' command.
data PCSSSetNavigationText = PCSSSetNavigationText
  {
    pCSSSetNavigationTextStyleSheetId :: DOMNetworkEmulationPageSecurity.DOMStyleSheetId,
    pCSSSetNavigationTextRange :: CSSSourceRange,
    pCSSSetNavigationTextText :: T.Text
  }
  deriving (Eq, Show)
pCSSSetNavigationText
  :: DOMNetworkEmulationPageSecurity.DOMStyleSheetId
  -> CSSSourceRange
  -> T.Text
  -> PCSSSetNavigationText
pCSSSetNavigationText
  arg_pCSSSetNavigationTextStyleSheetId
  arg_pCSSSetNavigationTextRange
  arg_pCSSSetNavigationTextText
  = PCSSSetNavigationText
    arg_pCSSSetNavigationTextStyleSheetId
    arg_pCSSSetNavigationTextRange
    arg_pCSSSetNavigationTextText
instance ToJSON PCSSSetNavigationText where
  toJSON p = A.object $ catMaybes [
    ("styleSheetId" A..=) <$> Just (pCSSSetNavigationTextStyleSheetId p),
    ("range" A..=) <$> Just (pCSSSetNavigationTextRange p),
    ("text" A..=) <$> Just (pCSSSetNavigationTextText p)
    ]
data CSSSetNavigationText = CSSSetNavigationText
  {
    -- | The resulting CSS Navigation rule after modification.
    cSSSetNavigationTextNavigation :: CSSCSSNavigation
  }
  deriving (Eq, Show)
instance FromJSON CSSSetNavigationText where
  parseJSON = A.withObject "CSSSetNavigationText" $ \o -> CSSSetNavigationText
    <$> o A..: "navigation"
instance Command PCSSSetNavigationText where
  type CommandResponse PCSSSetNavigationText = CSSSetNavigationText
  commandName _ = "CSS.setNavigationText"

-- | Modifies the expression of a scope at-rule.

-- | Parameters of the 'CSS.setScopeText' command.
data PCSSSetScopeText = PCSSSetScopeText
  {
    pCSSSetScopeTextStyleSheetId :: DOMNetworkEmulationPageSecurity.DOMStyleSheetId,
    pCSSSetScopeTextRange :: CSSSourceRange,
    pCSSSetScopeTextText :: T.Text
  }
  deriving (Eq, Show)
pCSSSetScopeText
  :: DOMNetworkEmulationPageSecurity.DOMStyleSheetId
  -> CSSSourceRange
  -> T.Text
  -> PCSSSetScopeText
pCSSSetScopeText
  arg_pCSSSetScopeTextStyleSheetId
  arg_pCSSSetScopeTextRange
  arg_pCSSSetScopeTextText
  = PCSSSetScopeText
    arg_pCSSSetScopeTextStyleSheetId
    arg_pCSSSetScopeTextRange
    arg_pCSSSetScopeTextText
instance ToJSON PCSSSetScopeText where
  toJSON p = A.object $ catMaybes [
    ("styleSheetId" A..=) <$> Just (pCSSSetScopeTextStyleSheetId p),
    ("range" A..=) <$> Just (pCSSSetScopeTextRange p),
    ("text" A..=) <$> Just (pCSSSetScopeTextText p)
    ]
data CSSSetScopeText = CSSSetScopeText
  {
    -- | The resulting CSS Scope rule after modification.
    cSSSetScopeTextScope :: CSSCSSScope
  }
  deriving (Eq, Show)
instance FromJSON CSSSetScopeText where
  parseJSON = A.withObject "CSSSetScopeText" $ \o -> CSSSetScopeText
    <$> o A..: "scope"
instance Command PCSSSetScopeText where
  type CommandResponse PCSSSetScopeText = CSSSetScopeText
  commandName _ = "CSS.setScopeText"

-- | Modifies the rule selector.

-- | Parameters of the 'CSS.setRuleSelector' command.
data PCSSSetRuleSelector = PCSSSetRuleSelector
  {
    pCSSSetRuleSelectorStyleSheetId :: DOMNetworkEmulationPageSecurity.DOMStyleSheetId,
    pCSSSetRuleSelectorRange :: CSSSourceRange,
    pCSSSetRuleSelectorSelector :: T.Text
  }
  deriving (Eq, Show)
pCSSSetRuleSelector
  :: DOMNetworkEmulationPageSecurity.DOMStyleSheetId
  -> CSSSourceRange
  -> T.Text
  -> PCSSSetRuleSelector
pCSSSetRuleSelector
  arg_pCSSSetRuleSelectorStyleSheetId
  arg_pCSSSetRuleSelectorRange
  arg_pCSSSetRuleSelectorSelector
  = PCSSSetRuleSelector
    arg_pCSSSetRuleSelectorStyleSheetId
    arg_pCSSSetRuleSelectorRange
    arg_pCSSSetRuleSelectorSelector
instance ToJSON PCSSSetRuleSelector where
  toJSON p = A.object $ catMaybes [
    ("styleSheetId" A..=) <$> Just (pCSSSetRuleSelectorStyleSheetId p),
    ("range" A..=) <$> Just (pCSSSetRuleSelectorRange p),
    ("selector" A..=) <$> Just (pCSSSetRuleSelectorSelector p)
    ]
data CSSSetRuleSelector = CSSSetRuleSelector
  {
    -- | The resulting selector list after modification.
    cSSSetRuleSelectorSelectorList :: CSSSelectorList
  }
  deriving (Eq, Show)
instance FromJSON CSSSetRuleSelector where
  parseJSON = A.withObject "CSSSetRuleSelector" $ \o -> CSSSetRuleSelector
    <$> o A..: "selectorList"
instance Command PCSSSetRuleSelector where
  type CommandResponse PCSSSetRuleSelector = CSSSetRuleSelector
  commandName _ = "CSS.setRuleSelector"

-- | Sets the new stylesheet text.

-- | Parameters of the 'CSS.setStyleSheetText' command.
data PCSSSetStyleSheetText = PCSSSetStyleSheetText
  {
    pCSSSetStyleSheetTextStyleSheetId :: DOMNetworkEmulationPageSecurity.DOMStyleSheetId,
    pCSSSetStyleSheetTextText :: T.Text
  }
  deriving (Eq, Show)
pCSSSetStyleSheetText
  :: DOMNetworkEmulationPageSecurity.DOMStyleSheetId
  -> T.Text
  -> PCSSSetStyleSheetText
pCSSSetStyleSheetText
  arg_pCSSSetStyleSheetTextStyleSheetId
  arg_pCSSSetStyleSheetTextText
  = PCSSSetStyleSheetText
    arg_pCSSSetStyleSheetTextStyleSheetId
    arg_pCSSSetStyleSheetTextText
instance ToJSON PCSSSetStyleSheetText where
  toJSON p = A.object $ catMaybes [
    ("styleSheetId" A..=) <$> Just (pCSSSetStyleSheetTextStyleSheetId p),
    ("text" A..=) <$> Just (pCSSSetStyleSheetTextText p)
    ]
data CSSSetStyleSheetText = CSSSetStyleSheetText
  {
    -- | URL of source map associated with script (if any).
    cSSSetStyleSheetTextSourceMapURL :: Maybe T.Text
  }
  deriving (Eq, Show)
instance FromJSON CSSSetStyleSheetText where
  parseJSON = A.withObject "CSSSetStyleSheetText" $ \o -> CSSSetStyleSheetText
    <$> o A..:? "sourceMapURL"
instance Command PCSSSetStyleSheetText where
  type CommandResponse PCSSSetStyleSheetText = CSSSetStyleSheetText
  commandName _ = "CSS.setStyleSheetText"

-- | Applies specified style edits one after another in the given order.

-- | Parameters of the 'CSS.setStyleTexts' command.
data PCSSSetStyleTexts = PCSSSetStyleTexts
  {
    pCSSSetStyleTextsEdits :: [CSSStyleDeclarationEdit],
    -- | NodeId for the DOM node in whose context custom property declarations for registered properties should be
    --   validated. If omitted, declarations in the new rule text can only be validated statically, which may produce
    --   incorrect results if the declaration contains a var() for example.
    pCSSSetStyleTextsNodeForPropertySyntaxValidation :: Maybe DOMNetworkEmulationPageSecurity.DOMNodeId
  }
  deriving (Eq, Show)
pCSSSetStyleTexts
  :: [CSSStyleDeclarationEdit]
  -> PCSSSetStyleTexts
pCSSSetStyleTexts
  arg_pCSSSetStyleTextsEdits
  = PCSSSetStyleTexts
    arg_pCSSSetStyleTextsEdits
    Nothing
instance ToJSON PCSSSetStyleTexts where
  toJSON p = A.object $ catMaybes [
    ("edits" A..=) <$> Just (pCSSSetStyleTextsEdits p),
    ("nodeForPropertySyntaxValidation" A..=) <$> (pCSSSetStyleTextsNodeForPropertySyntaxValidation p)
    ]
data CSSSetStyleTexts = CSSSetStyleTexts
  {
    -- | The resulting styles after modification.
    cSSSetStyleTextsStyles :: [CSSCSSStyle]
  }
  deriving (Eq, Show)
instance FromJSON CSSSetStyleTexts where
  parseJSON = A.withObject "CSSSetStyleTexts" $ \o -> CSSSetStyleTexts
    <$> o A..: "styles"
instance Command PCSSSetStyleTexts where
  type CommandResponse PCSSSetStyleTexts = CSSSetStyleTexts
  commandName _ = "CSS.setStyleTexts"

-- | Enables the selector recording.

-- | Parameters of the 'CSS.startRuleUsageTracking' command.
data PCSSStartRuleUsageTracking = PCSSStartRuleUsageTracking
  deriving (Eq, Show)
pCSSStartRuleUsageTracking
  :: PCSSStartRuleUsageTracking
pCSSStartRuleUsageTracking
  = PCSSStartRuleUsageTracking
instance ToJSON PCSSStartRuleUsageTracking where
  toJSON _ = A.Null
instance Command PCSSStartRuleUsageTracking where
  type CommandResponse PCSSStartRuleUsageTracking = ()
  commandName _ = "CSS.startRuleUsageTracking"
  fromJSON = const . A.Success . const ()

-- | Stop tracking rule usage and return the list of rules that were used since last call to
--   `takeCoverageDelta` (or since start of coverage instrumentation).

-- | Parameters of the 'CSS.stopRuleUsageTracking' command.
data PCSSStopRuleUsageTracking = PCSSStopRuleUsageTracking
  deriving (Eq, Show)
pCSSStopRuleUsageTracking
  :: PCSSStopRuleUsageTracking
pCSSStopRuleUsageTracking
  = PCSSStopRuleUsageTracking
instance ToJSON PCSSStopRuleUsageTracking where
  toJSON _ = A.Null
data CSSStopRuleUsageTracking = CSSStopRuleUsageTracking
  {
    cSSStopRuleUsageTrackingRuleUsage :: [CSSRuleUsage]
  }
  deriving (Eq, Show)
instance FromJSON CSSStopRuleUsageTracking where
  parseJSON = A.withObject "CSSStopRuleUsageTracking" $ \o -> CSSStopRuleUsageTracking
    <$> o A..: "ruleUsage"
instance Command PCSSStopRuleUsageTracking where
  type CommandResponse PCSSStopRuleUsageTracking = CSSStopRuleUsageTracking
  commandName _ = "CSS.stopRuleUsageTracking"

-- | Obtain list of rules that became used since last call to this method (or since start of coverage
--   instrumentation).

-- | Parameters of the 'CSS.takeCoverageDelta' command.
data PCSSTakeCoverageDelta = PCSSTakeCoverageDelta
  deriving (Eq, Show)
pCSSTakeCoverageDelta
  :: PCSSTakeCoverageDelta
pCSSTakeCoverageDelta
  = PCSSTakeCoverageDelta
instance ToJSON PCSSTakeCoverageDelta where
  toJSON _ = A.Null
data CSSTakeCoverageDelta = CSSTakeCoverageDelta
  {
    cSSTakeCoverageDeltaCoverage :: [CSSRuleUsage],
    -- | Monotonically increasing time, in seconds.
    cSSTakeCoverageDeltaTimestamp :: Double
  }
  deriving (Eq, Show)
instance FromJSON CSSTakeCoverageDelta where
  parseJSON = A.withObject "CSSTakeCoverageDelta" $ \o -> CSSTakeCoverageDelta
    <$> o A..: "coverage"
    <*> o A..: "timestamp"
instance Command PCSSTakeCoverageDelta where
  type CommandResponse PCSSTakeCoverageDelta = CSSTakeCoverageDelta
  commandName _ = "CSS.takeCoverageDelta"

-- | Enables/disables rendering of local CSS fonts (enabled by default).

-- | Parameters of the 'CSS.setLocalFontsEnabled' command.
data PCSSSetLocalFontsEnabled = PCSSSetLocalFontsEnabled
  {
    -- | Whether rendering of local fonts is enabled.
    pCSSSetLocalFontsEnabledEnabled :: Bool
  }
  deriving (Eq, Show)
pCSSSetLocalFontsEnabled
  {-
  -- | Whether rendering of local fonts is enabled.
  -}
  :: Bool
  -> PCSSSetLocalFontsEnabled
pCSSSetLocalFontsEnabled
  arg_pCSSSetLocalFontsEnabledEnabled
  = PCSSSetLocalFontsEnabled
    arg_pCSSSetLocalFontsEnabledEnabled
instance ToJSON PCSSSetLocalFontsEnabled where
  toJSON p = A.object $ catMaybes [
    ("enabled" A..=) <$> Just (pCSSSetLocalFontsEnabledEnabled p)
    ]
instance Command PCSSSetLocalFontsEnabled where
  type CommandResponse PCSSSetLocalFontsEnabled = ()
  commandName _ = "CSS.setLocalFontsEnabled"
  fromJSON = const . A.Success . const ()

